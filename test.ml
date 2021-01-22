open Llvm
open OUnit2

(** Repro a bug wherein assignment didn't return a value, leading LLVM function
    validation to fail. *)
let bad_phi_refs test_ctxt =
  let context = global_context () in
  let the_module = create_module context "my singleton module" in
  let builder = builder context in

  let main_proto = Ast.Prototype ("main", [| |], IntType) in
  let main = Ast.Function (main_proto,
                           Ast.Assignment("x", Int(1))) in
  ignore (Codegen.codegen_func main context the_module builder)

let undefined_if_branches test_ctxt =
  let body = Ast.Block([
                        Ast.Assignment("x", Int(1));
                        Ast.If(Ast.Var("x"), Ast.Assignment("a", Int(1)), Ast.Assignment("q", Int(2)));
                        Ast.Var("a")
                       ]) in
  assert_raises (Exc.Undefined_var "a") (fun () -> Ast.assert_no_unwritten_reads_in body)

let suite =
"suite">:::
  [
    "Assignments must return their value.">:: bad_phi_refs;
    "When the branches of an `if` read an undefined var, raise an error.">:: undefined_if_branches
  ]

let () =
  run_test_tt_main suite
