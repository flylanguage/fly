module L = Llvm
module A = Ast
module S = Sast

let translate blocks =
  let context = L.global_context () in
  let the_module = L.create_module context "Fly" in
  let process_block block =
    match block with
    | S.SMutDeclTyped (_s, _t, _e) -> print_endline "SMutDeclTyped found"
    | _ -> raise (Failure "not implemented")
  in
  let rec process_blocks = function
    | [] -> ()
    | block :: rest ->
      process_block block;
      process_blocks rest
  in
  process_blocks blocks;
  the_module
;;
