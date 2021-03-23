open Ast
open OUnit2

(** Just see if we can assign some simple type vars to things. *)
let annotate_bools_doubles_ifs_functions test_ctxt =
  let ast = Function (
      "main",
      [| |],
      If (
        Bool true,
        Double 1.2,
        Double 3.4
      )
    ) in
  let typed = TFunction (
      "main",
      [| |],
      TIf (
        TBool true,
        TDouble 1.2,
        TDouble 3.4,
        DoubleType
      ),
      FunctionType ([], DoubleType)
    ) in
  assert_equal (Infer.annotate ast) typed

(*
let annotate_free =
  ast = fun main(i) -> if (true) then 1.2 else 3.4
  expected = fun main ('0) -> double
*)

(* Then maybe test an If whose branches differ in type and make sure that doesn't unify. *)

let suite =
"suite" >:::
  [
    "Ifs and function return types annotate correctly. Doubles and bools work, too." >:: annotate_bools_doubles_ifs_functions;
  ]

let () =
  run_test_tt_main suite
