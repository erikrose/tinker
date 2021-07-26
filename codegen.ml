open Llvm
open Llvm_analysis

exception UndefinedVariable of string
exception Error of string

(** For the function currently being generated, a map of var name to memory
    location and type. This will become a stack of hash tables once we support
    nested functions. *)
let vars:(string, (llvalue * Ast.tipe)) Hashtbl.t = Hashtbl.create ~random:true 20
(* TODO: Make this module reentrant; these globals have to go. *)

(** Flag saying whether a function is currently being codegenned *)
let is_generating_function:bool ref = ref false

(** Return the LLVM type corresponding to one of our AST "tipes". *)
let rec llvm_type context ast_tipe =
  match ast_tipe with
  | Ast.BoolType -> i1_type context
  | Ast.DoubleType -> double_type context
  | IntType -> i64_type context (* TODO: make portable *)
  | StringType length -> array_type (i8_type context) length
  | StringPtrType -> pointer_type (i8_type context) (* Does it matter that this doesn't know the string is an array? *)
  | VoidType -> void_type context
  | FunctionType (arg_tipes, ret_tipe) ->
    let arg_llvm_types = List.map (llvm_type context) arg_tipes in
    pointer_type (function_type (llvm_type context ret_tipe) (Array.of_list arg_llvm_types))
  | TipeVar _ -> raise (Error "A type variable was still around in code I was asked to generate. It should have been resolved into a concrete type by now, either by inference or by monomorphization.")

type var_name_and_tipe = {name:string; tipe_:Ast.tipe}

(** Return a List of var names and tipes for each assigned var name in the
    scope of an expression.

    If the expression contains a function definition, we don't recurse into
    that; that would be a new scope, so the assignments therein would belong to
    it. *)
let rec assignments_in node : var_name_and_tipe list =
  (* TODO: Don't concretize the returned list. *)
  match node with
    | Ast.TBool _
    | Ast.TDouble _
    | Ast.TInt _
    | Ast.TString _
    | Ast.TFunction _ (* TODO: Figure out what the scope of function names is and how functions are to be referenced. Perhaps we should count a declared inner function as a local var and add it to the local scope. *)
    | Ast.TExternalFunction _
    | Ast.TVar _ -> []
    | Ast.TCall (_, args, _) -> List.concat (List.map assignments_in args)
    | Ast.TBlock (exprs, _) -> List.concat (List.map assignments_in exprs)
    | Ast.TIf (if_, then_, else_, _) -> List.concat [assignments_in if_;
                                                     assignments_in then_;
                                                     assignments_in else_]
    | Ast.TAssignment (var_name, value, t) -> {name=var_name; tipe_=t} :: assignments_in value

(** Assert a type is a FunctionType, and break it into arg types and return
    type. *)
let decomposed_function_tipe t =
  match t with
    | Ast.FunctionType (arg_tipes, ret_tipe) -> (arg_tipes, ret_tipe)
    | _ -> raise (Error "Function had a non-function type.")

let rec codegen_expr context the_module builder exp =
  let codegen_declaration name arg_tipes ret_tipe =
    (* Make the function type: double(double, double) etc. *)
    let llvm_arg_types = Array.init (Array.length arg_tipes) (fun i -> llvm_type context arg_tipes.(i)) in
    (* Return and arg types: *)
    let signature = function_type (llvm_type context ret_tipe) llvm_arg_types in
    declare_function name signature the_module
    (* TODO: Error if a function gets redeclared. *)
  in
  match exp with
  | Ast.TBool b -> const_int (i1_type context) (if b then 1 else 0)
  | Ast.TDouble n -> const_float (double_type context) n
  | Ast.TInt n -> const_int (i64_type context) n
  | Ast.TCall (func_expr, args, _) ->
    let func_ptr = codegen_expr context the_module builder func_expr in
    let genned_args = List.map (codegen_expr context the_module builder) args in
    build_call func_ptr (Array.of_list genned_args) "" builder
  | Ast.TString (str, _) ->
    build_global_stringptr str "" builder
  | Ast.TBlock (exprs, _) ->
    (* The value of a block is the value of its last evaluated expression. *)
    let last_expr_value _ cur_expr =
      codegen_expr context the_module builder cur_expr
    in
    (* This null value will do for now, but it might not be the final one we want: *)
    let null_value = const_null (void_type context) in
    List.fold_left last_expr_value null_value exprs
  | Ast.TAssignment (name, value, _) ->
    (* Codegen the value expr, then store it at the address of `name` as given
       by the hash table. The hash table also gives the type so we know how
       many bytes to copy. *)
    let genned_value = codegen_expr context the_module builder value in
    let (stack_slot, _) = Hashtbl.find vars name in
    (* Store and return the assigned value: *)
    ignore (build_store genned_value stack_slot builder);
    genned_value
  | Ast.TVar (name, _) ->
    begin
      try
        let (stack_slot, _) = Hashtbl.find vars name in
        build_load stack_slot name builder (* We actually use the var name as the destination SSA name. *)
      with Not_found ->
        (* TODO: Once type inference is in place, use lookup_function for things typed as functions and lookup_global for all other globals. *)
        match lookup_function name the_module with (* TODO: This prohibits forward refs. It raises if the global hasn't been added to the module yet. *)
        | Some global -> global
        | None -> raise (UndefinedVariable name)
    end
  | Ast.TIf (condition, then_, else_, _) ->
    (* Generate something along these lines. It's very hard to understand the
       if-generation code without this as a reference.

       entry:
         %ifcond = <condition_value>
         br i1 %ifcond, label %then, label %else

       then:                                             ; preds = %entry
         <whatever code is in the then block>
         br label %ifcont

       else:                                             ; preds = %entry
         <whatever code is in the else block>
         br label %ifcont

       ifcont:                                           ; preds = %else, %then
         %iftmp = phi i8* [ <then_value>, %then ], [ <else_value>, %else ]
         %1 = call i64 @puts(i8* %iftmp)
         ret i64 %1 *)

    let condition_value = codegen_expr context the_module builder condition in
    let int_zero = const_null (i1_type context) in
    let comparison = build_icmp Icmp.Ne condition_value int_zero "ifcond" builder in

    (* Save old BB, and start a new one for the "then" expr: *)
    let start_bb = insertion_block builder in
    let the_function = block_parent start_bb in

    (** Create a bb for a "then" or "else" branch of an if.

        Create a new bb, codegen the given expression as its contents and
        value, and return the bb to branch to and the one (possibly different)
        to phi from. (Codegen of the expr can change the current block.) Note
        that this function changes the builder's position.

        @param name A name to use as the label of the block, just for human
          comprehensibility *)
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
       "else" block to the "merge" block: *)
    position_at_end new_then_bb builder; ignore (build_br merge_bb builder);
    position_at_end new_else_bb builder; ignore (build_br merge_bb builder);

    (* Finally, set the builder to the end of the merge block: *)
    position_at_end merge_bb builder;

    phi
  | Ast.TFunction (name, _, body, t) ->
    let (arg_tipes, ret_tipe) = decomposed_function_tipe t in
    if !is_generating_function then
      begin
        is_generating_function := false; (* so future test-harness calls don't get a stale value *)
        raise (Error "Inner functions are not allowed yet.")
      end
    else
      is_generating_function := true;
    let the_function = codegen_declaration name (Array.of_list arg_tipes) ret_tipe in (* TODO: munge names *)
    (* Create a new basic block, and point the builder to the end of it: *)
    let bb = append_block context "entry" the_function in
    position_at_end bb builder;

    Ast.assert_no_unwritten_reads_in_scope body;

    (* TODO: What happens when we encounter an inner function? Don't screw up. Start a new Hashtbl (or go with a COW tree). Watch your block position. I guess generate all funcs as globals with munged names. LLVM IR does expect func ptrs as params to all call instructions, so we could just pass func ptrs around. *)

    (* Add allocas for all local vars. They have to be in the entry block, or
       mem2reg won't work. *)

    (** If the given var isn't already allocated, gen the alloca, and add an
        entry to the symbol table: *)
    let add_local_var (name_and_tipe:var_name_and_tipe) =
      let {name=var_name; tipe_=var_tipe} = name_and_tipe in
      match Hashtbl.find_opt vars var_name with
      | None ->
        let stack_slot = build_alloca (llvm_type context var_tipe) var_name builder in
        Hashtbl.replace vars var_name (stack_slot, var_tipe)
      | Some _ -> ()
    in
    List.iter add_local_var (assignments_in body);

    (* Codegen body: *)
    let ret_val = codegen_expr context the_module builder body in

    (* Return the computed value of the body expression: *)
    ignore (build_ret ret_val builder);

    (* Validate the generated code, checking for consistency: *)
    assert_valid_function the_function;

    Hashtbl.reset vars;
    is_generating_function := false;
    the_function
  | Ast.TExternalFunction (name, t) ->
    let (arg_tipes, ret_tipe) = (decomposed_function_tipe t) in
    codegen_declaration name (Array.of_list arg_tipes) ret_tipe
