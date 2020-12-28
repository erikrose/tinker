open Llvm
open Llvm_analysis

exception Error of string

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
    if Array.length params == Array.length args then () else
      raise (Error "incorrect # arguments passed");
    let genned_args = Array.map (codegen_expr context the_module builder) args in
    build_call callee genned_args "" builder (* TODO: Stop naming this. Won't it collide with other calls in the same bb? *)
  | Ast.String str ->
    build_global_stringptr str "" builder
  | Ast.Block exprs ->
    (* The value of a block is the value of its last evaluated expression. *)
    let last_expr_value _ cur_expr =
      codegen_expr context the_module builder cur_expr
    in
    (* This null value will do for now, but it might not be the final one we want: *)
    let null_value = const_null (void_type context) in
    Array.fold_left last_expr_value null_value exprs
  | Ast.If (condition, then_, else_) ->
    (* Position the builder at the end of a new block, codegen it?, and return the block where the codegen ends up.  *)
    (* let end_of_new_block =  *)
  
    let condition_value = codegen_expr context the_module builder condition in
    let int_zero = const_null (i64_type context) in (* TODO: Support taking other types of condition than int. *)
    let comparison = build_icmp Icmp.Ne condition_value int_zero "ifcond" builder in
    
    (* Save old BB, and start a new one for the "then" expr: *)
    let start_bb = insertion_block builder in
    let the_function = block_parent start_bb in

    (* Emit the "then" bb: *)
    let then_bb = append_block context "then" the_function in
    position_at_end then_bb builder;
    let then_value = codegen_expr context the_module builder then_ in

    (* Codegen of "then" can change the current block. Grab that for the
     * phi. We create a new name because one is used for the phi node and the
     * other is used for the conditional branch. *)
    let new_then_bb = insertion_block builder in

    (* Emit "else" bb: *)
    let else_bb = append_block context "else" the_function in
    position_at_end else_bb builder;
    let else_value = codegen_expr context the_module builder else_ in

    (* Codegen of 'else' can change the current block. Grab that for the
     * phi. *)
    let new_else_bb = insertion_block builder in

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

    (* Codegen body: *)
    let ret_val = codegen_expr context the_module builder body in

    (* Return the computed value of the body expression: *)
    ignore (build_ret ret_val builder);

    (* Validate the generated code, checking for consistency: *)
    assert_valid_function the_function;

    the_function
