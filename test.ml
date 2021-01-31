open Llvm
open OUnit2

let main_function expr =
  Ast.Function (
    "main",
    [| |],
    IntType,
    Ast.Internal (
      expr
    )
   )

(** Repro a bug wherein assignment didn't return a value, leading LLVM function
    validation to fail. *)
let assignment_values test_ctxt =
  let context = global_context () in
  let the_module = create_module context "my singleton module" in
  let builder = builder context in

  let main = main_function (Ast.Assignment ("x", Int(1))) in
  ignore (Codegen.codegen_expr context the_module builder main)

let undefined_if_branches test_ctxt =
  let body = Ast.Block([
                        Ast.Assignment("x", Int(1));
                        Ast.If(Ast.Var("x"), Ast.Assignment("a", Int(1)), Ast.Assignment("q", Int(2)));
                        Ast.Var("a")
                       ]) in
  assert_raises (Exc.Undefined_var "a") (fun () -> Ast.assert_no_unwritten_reads_in_scope body)

let inner_functions_raise_exception test_ctxt =
  let context = global_context () in
  let the_module = create_module context "my singleton module" in
  let builder = builder context in

  let main = main_function (
              Ast.Function (
                "sub",
                [| |],
                VoidType,
                Ast.Internal (
                  Ast.Block []
                )
              )
             ) in
  assert_raises (Codegen.Error "Inner functions are not allowed yet.") (fun () -> Codegen.codegen_expr context the_module builder main)

let suite =
"suite" >:::
  [
    "Assignments must return their value." >:: assignment_values;
    "When the branches of an `if` read an undefined var, raise an error." >:: undefined_if_branches;
    "Inner functions are disallowed for now." >:: inner_functions_raise_exception;
  ]

let () =
  run_test_tt_main suite
