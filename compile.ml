open Llvm

let main () =
  let context = global_context () in
  let the_module = create_module context "my singleton module" in
  let builder = builder context in

  let puts_proto = Ast.Prototype ("puts", [| StringPtrType |], IntType) in
  ignore (Codegen.codegen_proto puts_proto context the_module);

  let main_proto = Ast.Prototype ("main", [| |], IntType) in
  let main = Ast.Function (main_proto,
                           Ast.Block([|
                                        Ast.Call("puts", [| String ("howdy") |]);
                                        Ast.Call("puts", [| Ast.If(Int(1), String("true"), String("false")) |])
                                     |])) in
  ignore (Codegen.codegen_func main context the_module builder);

  dump_module the_module

let () = main ()
