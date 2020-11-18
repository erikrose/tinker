open Llvm
open Llvm_analysis

let codegen_expr body context =
  match body with
  | Ast.Number n -> const_float (double_type context) n

let codegen_proto proto context the_module =
  match proto with
  | Ast.Prototype (name, args) ->
    (* Make the function type: double(double,double) etc. *)
    let double_type = double_type context in
    let doubles = Array.make (Array.length args) double_type in
    (* Return type and arg types: *)
    let types = function_type double_type doubles in
    declare_function name types the_module

let codegen_func func context the_module builder =
  match func with
  | Ast.Function (proto, body) ->
    let the_function = codegen_proto proto context the_module in

    (* Create a new basic block to start insertion into. *)
    let bb = append_block context "entry" the_function in
    position_at_end bb builder;

    (* Get code for body *)
    let ret_val = codegen_expr body context in

    (* Finish off the function. *)
    let _ = build_ret ret_val builder in

    (* Validate the generated code, checking for consistency. *)
    assert_valid_function the_function;

    the_function

let main () =
  let context = global_context () in
  let the_module = create_module context "my singleton module" in
  let builder = builder context in
  let proto = Ast.Prototype ("code", [| |]) in
  let func = Ast.Function (proto, Number 33.) in
  let _ = codegen_func func context the_module builder in
  dump_module the_module
  
let () = main ()
