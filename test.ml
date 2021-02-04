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

let run exp =
  let context = global_context () in
  let the_module = create_module context "my singleton module" in
  let builder = builder context in
  ignore (Codegen.codegen_expr context the_module builder exp);
  ignore (Llvm_executionengine.initialize ()); (* I don't think we care if this fails (returns false), because I suppose it then falls back to the interpreter, which is enough for test purposes. *)
  let engine = Llvm_executionengine.create the_module in
  (* This has the side effect of generating the machine code, after which no
     more changes to the module will ever be taken into account: *)
  let func = Llvm_executionengine.get_function_address "main" (Foreign.funptr (Ctypes.(@->) Ctypes.void (Ctypes.returning Ctypes.int))) engine in
  let result:int = func () in
  Llvm_executionengine.dispose engine;
  result

(** Repro a bug wherein assignment didn't return a value, leading LLVM function
    validation to fail. *)
let assignments_return_values test_ctxt =
  let context = global_context () in
  let the_module = create_module context "my singleton module" in
  let builder = builder context in

  let main = main_function (Ast.Assignment ("x", Int(1))) in
  ignore (Codegen.codegen_expr context the_module builder main)

let undefined_reads_in_if_branches_not_allowed test_ctxt =
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
  assert_raises (Codegen.Error "Inner functions are not allowed yet.")
                (fun () -> Codegen.codegen_expr context the_module builder main)

let jit_works_for_tests test_ctxt =
  let result = run (main_function (Ast.Int 33)) in
  assert_equal result 33

let suite =
"suite" >:::
  [
    "Assignments must return their value." >:: assignments_return_values;
    "When the branches of an `if` read an undefined var, raise an error." >:: undefined_reads_in_if_branches_not_allowed;
    "Inner functions are disallowed for now." >:: inner_functions_raise_exception;
    "JITting test harness works" >:: jit_works_for_tests;
  ]

let () =
  run_test_tt_main suite
