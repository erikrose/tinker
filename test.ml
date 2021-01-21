open Llvm
open OUnit2

(** Repro a bug wherein phi refs for the [then] and [else] blocks were bad,
    leading LLVM function validation to fail. *)
let bad_phi_refs test_ctxt =
  let context = global_context () in
  let the_module = create_module context "my singleton module" in
  let builder = builder context in

  let puts_proto = Ast.Prototype ("puts", [| StringPtrType |], IntType) in
  ignore (Codegen.codegen_proto puts_proto context the_module);

  let main_proto = Ast.Prototype ("main", [| |], IntType) in
  let main = Ast.Function (main_proto,
                           Ast.Block([
                                      Ast.Call("puts", [ String ("howdy") ]);
                                      Ast.Assignment("x", Int(1));
                                      Ast.If(Ast.Var("x"), Ast.Assignment("a", Int(1)), Ast.Assignment("a", Int(2)));
                                      Ast.Var("a")
                                     ])) in
  ignore (Codegen.codegen_func main context the_module builder)

let suite =
"suite">:::
 ["bad_phi_refs">:: bad_phi_refs]

let () =
  run_test_tt_main suite
