open Llvm

let main () =
  let context = global_context () in
  let the_module = create_module context "my singleton module" in
  let builder = builder context in

  let print_double_proto = Ast.Prototype ("print_double", [| "f" |]) in
  ignore (Codegen.codegen_proto print_double_proto context the_module);

  let main_proto = Ast.Prototype ("main", [| |]) in
  let main = Ast.Function (main_proto,
                           Ast.Call("print_double", [| Number 33. |])) in
  ignore (Codegen.codegen_func main context the_module builder);

  dump_module the_module

let () = main ()
