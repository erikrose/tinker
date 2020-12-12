open Llvm

let main () =
  let context = global_context () in
  let the_module = create_module context "my singleton module" in
  let builder = builder context in

  let puts_proto = Ast.Prototype ("puts", [| StringPtrType |]) in
  ignore (Codegen.codegen_proto puts_proto context the_module);

  let main_proto = Ast.Prototype ("main", [| |]) in
  let main = Ast.Function (main_proto,
                           Ast.Call("puts", [| String ("howdy") |])) in
  ignore (Codegen.codegen_func main context the_module builder);

  dump_module the_module

let () = main ()

(* TODO: make puts() return an int, like it really does. Add an Int type for that, and make protos and funcs take return types. *)