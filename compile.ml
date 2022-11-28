open Llvm

let main () =
  let context = global_context () in
  let the_module = create_module context "my singleton module" in
  let builder = builder context in

  let puts = Ast.ExternalFunction ("puts", FunctionType ([StringPtrType], IntType)) in
  ignore (Codegen.codegen_expr context the_module builder (Infer.infer_types puts));

  let other_func = Ast.Function (
      "other", [| |],
      Int 44
    ) in
  ignore (Codegen.codegen_expr context the_module builder (Infer.infer_types other_func));

  let main_func = Ast.Function (
      "main", [| |],
      Ast.Block [
        Assignment ("first_class_other_func",
                    Var "other");
        Call (Var "first_class_other_func",
              [])
      ]
    ) in
  ignore (Codegen.codegen_expr context the_module builder (Infer.infer_types main_func));

  dump_module the_module

let () = main ()
