open Llvm

let main () =
  let context = global_context () in
  let the_module = create_module context "my singleton module" in
  let builder = builder context in
  let proto = Ast.Prototype ("code", [| |]) in
  let func = Ast.Function (proto, Number 33.) in
  let _ = Codegen.codegen_func func context the_module builder in
  dump_module the_module
  
let () = main ()
