open Llvm

let main () =
  let context = global_context () in
  let the_module = create_module context "my singleton module" in
  let builder = builder context in

  let puts = Ast.Function ("puts", [| StringPtrType |], IntType, Ast.External) in
  ignore (Codegen.codegen_expr context the_module builder puts);

  let main = Ast.Function ("main",
                           [| |],
                           IntType,
                           Ast.Body (Ast.Block ([
                                        Ast.Call("puts", [ String ("howdy") ]);
                                        Ast.Assignment("x", Int(1));
                                        Ast.Call("puts", [ Ast.If(Ast.Var("x"), String("true"), String("false")) ])
                                     ]))) in
  ignore (Codegen.codegen_expr context the_module builder main);

  dump_module the_module

let () = main ()
