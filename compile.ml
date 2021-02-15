open Llvm

let main () =
  let context = global_context () in
  let the_module = create_module context "my singleton module" in
  let builder = builder context in

  let puts = Ast.Function ("puts", [| StringPtrType |], IntType, Ast.External) in
  ignore (Codegen.codegen_expr context the_module builder puts);

  let other_func = Ast.Function (
      "other", [| |], IntType,
      Ast.Internal (
        Ast.Int 44
      )
    ) in
  ignore (Codegen.codegen_expr context the_module builder other_func);
  let main_func = Ast.Function (
      "main", [| |], IntType,
      Ast.Internal (
        Ast.Block [
          Ast.Assignment ("first_class_other_func",
                          Ast.Var "other",
                          Ast.FunctionType ([], IntType));
          Ast.Call (Ast.Var "first_class_other_func",
                    [])
        ]
      )
    ) in
  ignore (Codegen.codegen_expr context the_module builder main_func);

  dump_module the_module

let () = main ()
