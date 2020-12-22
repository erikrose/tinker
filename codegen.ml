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
