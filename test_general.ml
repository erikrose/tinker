open Llvm
open OUnit2

let main_function exp =
  Ast.Function (
    "main",
    [| |],
    exp
   )

(** Codegen an expression into a new module, and return the module. *)
let compile exp =
  let context = global_context () in
  let the_module = create_module context "my singleton module" in
  let builder = builder context in
  ignore (Codegen.codegen_expr context the_module builder (Infer.infer_types exp));
  the_module

(** Run the no-args function called "main" from the given module. Return its
    int result. *)
let run_main the_module =
  ignore (Llvm_executionengine.initialize ()); (* I don't think we care if this fails (returns false), because I suppose it then falls back to the interpreter, which is enough for test purposes. *)
  let engine = Llvm_executionengine.create the_module in
  (* This has the side effect of generating the machine code, after which no
     more changes to the module will ever be taken into account: *)
  let func = Llvm_executionengine.get_function_address "main" (Foreign.funptr (Ctypes.(@->) Ctypes.void (Ctypes.returning Ctypes.int))) engine in
  let result : int = func () in
  Llvm_executionengine.dispose engine;
  result

let tests = "General tests" >::: [
  (** Repro a bug wherein assignment didn't return a value, leading LLVM function
      validation to fail. *)
  "Assignments must return their value." >:: (fun _ -> (
    let main = main_function (Ast.Assignment ("x", Int(1))) in
    ignore (compile main)
  ));

  "When the branches of an `if` read an undefined var, raise an error." >:: (fun _ -> (
    let func = Ast.Function (
      "foo",
      [| |],
      Ast.Block (
        [
          Ast.Assignment ("x", Ast.Bool true);
          Ast.If (
            Ast.Var "x",
            Ast.Assignment("a", Int 1),
            Ast.Assignment("q", Int 2)
          );
          Ast.Var "a"
        ]
      )
    ) in
    let tfunc = Infer.infer_types func in
    let tbody = match tfunc with
      | TFunction (_, _, body, _) -> body
      | _ -> assert_failure "Something went terribly wrong."
    in
    assert_raises (Exc.Undefined_var "a") (fun () -> Ast.assert_no_unwritten_reads_in_scope tbody)
  ));

  "Inner functions are disallowed for now." >:: (fun _ -> (
    let main = main_function (
                Ast.Function (
                  "sub",
                  [| |],
                  Ast.Int(3)
                )
               ) in
    assert_raises (Codegen.Error "Inner functions are not allowed yet.")
                  (fun () -> compile main)
  ));

  "JITting test harness works" >:: (fun _ -> (
    let result = run_main (compile (main_function (Ast.Int 33))) in
    assert_equal result 33
  ));

(*
  "Global functions are first-class." >:: (fun _ -> (
    let context = global_context () in
    let the_module = create_module context "my singleton module" in
    let builder = builder context in

    let other_func = Ast.Function (
        "other", [| |],
        Ast.Int 44
      ) in
    assert_equal !Codegen.is_generating_function false;
    ignore (Codegen.codegen_expr context the_module builder (Infer.infer_types other_func));
    assert_equal !Codegen.is_generating_function false;
    let main_func = Ast.Function (
        "main", [| |],
        Ast.Block [
          Ast.Assignment ("first_class_other_func",
                          Ast.Var "other");
          Ast.Call (Ast.Var "first_class_other_func",
                    [])
        ]
      ) in
    ignore (Codegen.codegen_expr context the_module builder (Infer.infer_types main_func));

    let result = run_main the_module in
    assert_equal result 44
  ));
*)
]
