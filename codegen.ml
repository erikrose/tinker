open Llvm
open Llvm_analysis

exception Error of string

let rec codegen_expr context the_module builder body =
  match body with
  | Ast.Number n -> const_float (double_type context) n
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
    build_call callee genned_args "calltmp" builder
  | Ast.String (str) ->
    build_global_stringptr str "" builder
    (* const_stringz context str (* representation of i8 array *)*)

let codegen_proto proto context the_module =
  match proto with
  | Ast.Prototype (name, arg_types) ->
    (* Make the function type: double(double,double) etc. *)
    let double_type = double_type context in
    let llvm_types = Array.init (Array.length arg_types)
                               (fun i -> match arg_types.(i) with
                                         | NumberType -> double_type
                                         | StringType length -> array_type (i8_type context) length
                                         | StringPtrType -> pointer_type (i8_type context) (* Does it matter that this doesn't know the string is an array? *)
                                         | VoidType -> void_type context) in
    (* Return and arg types: *)
    let signature = function_type double_type llvm_types in
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
