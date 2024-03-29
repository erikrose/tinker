open OUnit2

open Ast

let assert_annotate_equal annotated ast = 
  assert_equal annotated (Infer.annotate ast) ~printer:show_texpr

let tests = "Unification tests" >::: [
  "Ifs and function return types annotate correctly. Doubles and bools work, too." >:: (fun _ -> (
    (* Just see if we can assign some simple type vars to things. *)
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
  ));

  "Strings annotate." >:: (fun _ -> (
    let ast = String "smoobar" in
    let typed = TString ("smoobar", StringType 7) in
    assert_annotate_equal typed ast
  ));

  "Calls to undefined functions annotate properly. Also, the new-free-var branch of var lookup works." >:: (fun _ -> (
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
  ));

  "The type of a block is the type of its last expr." >:: (fun _ -> (
    let ast = Block [Int 4; Int 5; Bool true] in
    let typed = TBlock ([TInt 4; TInt 5; TBool true], BoolType) in
    assert_annotate_equal typed ast
  ));

(* Ifs are self-evidently correct. *)
(* It's hard to test Vars to any extent further than "it doesn't crash" until we collect(). *)
(* Assignments are self-evidently correct. *)

  "Later var refs can see vars made by Assignments." >:: (fun _ -> (
    (* This allays my doubts that assignments are properly added to bound_vars
       in annotate. *)
    let ast = Function (
      "foo",
      [| |],
      Block [
        Assignment ("x", Int 7);
        Var "x"
      ]
    ) in
    let typed = TFunction (
      "foo",
      [| |],
      TBlock (
        [
          TAssignment ("x", TInt 7, IntType);
          TVar ("x", IntType)
        ],
        IntType
      ),
      FunctionType (
        [],
        IntType
      )
    ) in
    assert_equal typed (Infer.infer_types ast) ~printer:show_texpr
  ));

  "Types of bound vars are looked up successfully." >:: (fun _ -> (
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
  ));

  "The type of a call is constrained to agree with the types of its args and return value." >:: (fun _ -> (
    let annotated = TCall (
      TVar ("repeat", TipeVar 1),
      [TString ("foo", StringType 3); TInt 9],
      TipeVar 2
    ) in
    assert_equal [TipeVar 1,
                  FunctionType ([StringType 3; IntType], TipeVar 2)]
                 (Infer.collect [annotated] [])
  ));

  "Proper type constraints for ifs are generated." >:: (fun _ -> (
    let annotated = TIf (
      TInt 8, (* cond *)
      TVar ("thenExpr", TipeVar 1),
      TVar ("elseExpr", TipeVar 2),
      TipeVar 3
    ) in
    (* Of course, the following wouldn't successfully unify, but it more
       unambiguously demonstrates that annotation is working than would repeating
       TipeVars: *)
    assert_equal [(IntType, BoolType); 
                  (TipeVar 1, TipeVar 2);
                  (TipeVar 1, TipeVar 3)]
                 (Infer.collect [annotated] [])
  ));
]

(* TODO: Maybe test an If whose branches differ in type and make sure that doesn't unify. *)
