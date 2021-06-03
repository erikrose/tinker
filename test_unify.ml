open OUnit2

open Ast

let assert_annotate_equal annotated ast = 
  assert_equal annotated (Infer.annotate ast) ~printer:show_texpr

(** Just see if we can assign some simple type vars to things. *)
let annotate_bools_doubles_ifs_functions _ =
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
  assert_annotate_equal typed ast

let annotate_strings _ =
  let ast = String "smoobar" in
  let typed = TString ("smoobar", StringType 7) in
  assert_annotate_equal typed ast

let annotate_calls _ =
  let ast = Call (
    Var "someFunc",
    [Int 1]
  ) in
  let expected = TCall (
    TVar ("someFunc", TipeVar 1),
    [TInt 1],
    TipeVar 2
  ) in
  assert_annotate_equal expected ast

let annotate_block _ =
  let ast = Block [Int 4; Int 5; Bool true] in
  let typed = TBlock ([TInt 4; TInt 5; TBool true], BoolType) in
  assert_annotate_equal typed ast

(* Ifs are self-evidently correct. *)
(* It's hard to test Vars to any extent further than "it doesn't crash" until we collect(). *)
(* Assignments are self-evidently correct. *)

let annotate_function_returning_bound_var _ =
  let ast = Function (
    "main",
    [| "a" |],
    Var "a"
  ) in
  let expected = TFunction (
    "main",
    [| "a" |],
    TVar ("a", TipeVar 1),
    FunctionType ([TipeVar 1], TipeVar 1)
  ) in
  assert_annotate_equal expected ast

let collect_call _ =
  let annotated = TCall (
    TVar ("repeat", TipeVar 1),
    [TString ("foo", StringType 3); TInt 9],
    TipeVar 2
  ) in
  assert_equal [TipeVar 1,
                FunctionType ([StringType 3; IntType], TipeVar 2)]
               (Infer.collect [annotated] [])

(* Then maybe test an If whose branches differ in type and make sure that doesn't unify. *)

let suite =
"suite" >:::
  [
    "Ifs and function return types annotate correctly. Doubles and bools work, too." >:: annotate_bools_doubles_ifs_functions;
    "Strings annotate." >:: annotate_strings;
    "Calls to undefined functions annotate properly. Also, the new-free-var branch of var lookup works." >:: annotate_calls;
    "The type of a block is the type of its last expr." >:: annotate_block;
    "Types of bound vars are looked up successfully." >:: annotate_function_returning_bound_var;
    "The type of a function is constrained to agree with the types of its args and return value" >:: collect_function;
  ]

let () =
  run_test_tt_main suite
