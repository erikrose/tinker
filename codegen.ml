open Llvm
open Llvm_analysis

exception Error of string

(* For the function currently being generated, a map of var name to memory
 * location and type. This will become a stack of hash tables once we support
 * nested functions. *)
let vars:(string, (llvalue * Ast.tipe)) Hashtbl.t = Hashtbl.create ~random:true 20

let rec codegen_expr context the_module builder body =
  match body with
  | Ast.Double n -> const_float (double_type context) n
  | Ast.Int n -> const_int (i64_type context) n
  | Ast.Call (callee, args) ->
    (* Look up the name in the module table. *)
    let callee =
      match lookup_function callee the_module with
      | Some callee -> callee
      | None -> raise (Error "unknown function referenced")
    in
    let params = params callee in

    (* If argument mismatch error: *)
    if Array.length params == List.length args then () else
      raise (Error "incorrect # arguments passed");
    let genned_args = List.map (codegen_expr context the_module builder) args in
    build_call callee (Array.of_list genned_args) "" builder
  | Ast.String str ->
    build_global_stringptr str "" builder
  | Ast.Block exprs ->
    (* The value of a block is the value of its last evaluated expression. *)
    let last_expr_value _ cur_expr =
      codegen_expr context the_module builder cur_expr
    in
    (* This null value will do for now, but it might not be the final one we want: *)
    let null_value = const_null (void_type context) in
    List.fold_left last_expr_value null_value exprs

  (* Vars:
     entry:
       %X = alloca i32           ; type of %X is i32*.
       %tmp = load i32* %X       ; load the stack value %X from the stack.
       %tmp2 = add i32 %tmp, 1   ; increment it
       store i32 %tmp2, i32* %X  ; store it back
  *)
  | Ast.Assignment (name, value) ->
    (* Codegen the value expr, then store it at the address of `name` as given
     * by the hash table. The hash table also gives the type so we know how
     * many bytes to copy. *)
    let genned_value = codegen_expr context the_module builder value in
    let (stack_slot, _) = Hashtbl.find vars name in
    (* Store and return the assigned value: *)
    build_store genned_value stack_slot builder
  | Ast.Var name ->
    let (stack_slot, _) = Hashtbl.find vars name in
    build_load stack_slot name builder (* We actually use the var name as the destination SSA name. *)
  | Ast.If (condition, then_, else_) ->
    (* Generate something along these lines. It's very hard to understand the
     * if-generation code without this as a reference.
     *
     * entry:
     *   %ifcond = <condition_value>
     *   br i1 %ifcond, label %then, label %else
     *
     * then:                                             ; preds = %entry
     *   <whatever code is in the then block>
     *   br label %ifcont
     *
     * else:                                             ; preds = %entry
     *   <whatever code is in the else block>
     *   br label %ifcont
     *
     * ifcont:                                           ; preds = %else, %then
     *   %iftmp = phi i8* [ <then_value>, %then ], [ <else_value>, %else ]
     *   %1 = call i64 @puts(i8* %iftmp)
     *   ret i64 %1
     *)

    let condition_value = codegen_expr context the_module builder condition in
    let int_zero = const_null (i64_type context) in
    let comparison = build_icmp Icmp.Ne condition_value int_zero "ifcond" builder in

    (* Save old BB, and start a new one for the "then" expr: *)
    let start_bb = insertion_block builder in
    let the_function = block_parent start_bb in

    (** Create a bb for a "then" or "else" branch of an if.
     *
     * Create a new bb, codegen the given expression as its contents and value,
     * and return the bb to branch to and the one (possibly different) to phi
     * from. (Codegen of the expr can change the current block.) Note that this
     * function changes the builder's position.
     *
     * @param name A name to use as the label of the block, just for human
     *   comprehensibility
     *)
    let new_bb_with_expr name contents =
      let bb_for_branch = append_block context name the_function in
      position_at_end bb_for_branch builder;
      let value = codegen_expr context the_module builder contents in
      let bb_for_phi = insertion_block builder in
      (bb_for_branch, value, bb_for_phi)
    in

    (* Emit the "then" bb: *)
    let (then_bb, then_value, new_then_bb) = new_bb_with_expr "then" then_ in

    (* Emit "else" bb: *)
    let (else_bb, else_value, new_else_bb) = new_bb_with_expr "else" else_ in

    (* Emit merge block: *)
    let merge_bb = append_block context "ifcont" the_function in
    position_at_end merge_bb builder;
    let incoming = [(then_value, new_then_bb); (else_value, new_else_bb)] in
    let phi = build_phi incoming "iftmp" builder in

    (* Return to the start block to add the conditional branch: *)
    position_at_end start_bb builder;
    ignore (build_cond_br comparison then_bb else_bb builder);

    (* Set an unconditional branch at the end of the "then" block and the
     * "else" block to the "merge" block: *)
    position_at_end new_then_bb builder; ignore (build_br merge_bb builder);
    position_at_end new_else_bb builder; ignore (build_br merge_bb builder);

    (* Finally, set the builder to the end of the merge block: *)
    position_at_end merge_bb builder;

    phi

(* Return the LLVM type corresponding to one of our AST "tipes". *)
let llvm_type ast_type context =
  match ast_type with
  | Ast.DoubleType -> double_type context
  | IntType -> i64_type context (* TODO: make portable *)
  | StringType length -> array_type (i8_type context) length
  | StringPtrType -> pointer_type (i8_type context) (* Does it matter that this doesn't know the string is an array? *)
  | VoidType -> void_type context

let codegen_proto proto context the_module =
  match proto with
  | Ast.Prototype (name, arg_types, ret_type) ->
    (* Make the function type: double(double,double) etc. *)
    let llvm_arg_types = Array.init (Array.length arg_types) (fun i -> llvm_type arg_types.(i) context) in
    (* Return and arg types: *)
    let signature = function_type (llvm_type ret_type context) llvm_arg_types in
    declare_function name signature the_module
    (* TODO: Error if a function gets redeclared. *)

let codegen_func func context the_module builder =
  match func with
  | Ast.Function (proto, body) ->
    let the_function = codegen_proto proto context the_module in

    (* Create a new basic block, and point the builder to the end of it: *)
    let bb = append_block context "entry" the_function in
    position_at_end bb builder;
    
    (* Add allocas for all local vars. They have to be in the entry block, or
     * mem2reg won't work. *)

    (* Return a List of the assigned var names in an expression. *)
    let rec assignments_in node =
      (* TODO: Don't concretize the returned list. *)
      match node with
        | Ast.Double _
        | Ast.Int _
        | Ast.String _
        | Ast.Var _ -> []
        | Ast.Call (name, args) -> List.concat (List.map assignments_in args)
        | Ast.Block exprs -> List.concat (List.map assignments_in exprs)
        | Ast.If (if_, then_, else_) -> List.concat [assignments_in if_;
                                                     assignments_in then_;
                                                     assignments_in else_]
        | Ast.Assignment (var_name, value) -> var_name :: assignments_in value
    in
    (* Gen the alloca, and add an entry to the symbol table: *)
    let add_local_var t var_name =
      let stack_slot = build_alloca (llvm_type IntType context) var_name builder in
      Hashtbl.replace vars var_name (stack_slot, t) in

    (* For the moment, we support assigning only to ints, just so we can get
     * vars proven out without having to write type inference first. *)
    (* TODO: Add a given var to the table only once. *)
    List.iter (add_local_var IntType) (assignments_in body);

    (* Codegen body: *)
    let ret_val = codegen_expr context the_module builder body in

    (* Return the computed value of the body expression: *)
    ignore (build_ret ret_val builder);

    (* Validate the generated code, checking for consistency: *)
    assert_valid_function the_function;

    Hashtbl.reset vars;
    the_function
