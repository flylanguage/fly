module L = Llvm
module A = Ast

let translate (globals, functions) =
  let context = L.global_context () in
  let the_module = L.create_module context "Fly" in
  the_module
;;
