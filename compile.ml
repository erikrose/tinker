open Llvm

let main () =
  let context = global_context () in
  let the_module = create_module context "my singleton module" in
  let builder = builder context in
  let print_double_proto = Ast.Prototype ("print_double", [| "f" |]) in
  let _ = Codegen.codegen_proto print_double_proto context the_module in
  let proto = Ast.Prototype ("main", [| |]) in
  let func = Ast.Function (proto,
                           Ast.Call("print_double", [| Number 33. |])) in
  let _ = Codegen.codegen_func func context the_module builder in
  dump_module the_module

let () = main ()
