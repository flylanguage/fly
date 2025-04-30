module L = Llvm
module A = Ast
open Sast
module StringMap = Map.Make (String)

let context = L.global_context ()

let l_int = L.i32_type context
and l_bool = L.i1_type context
and l_char = L.i8_type context
and l_float = L.float_type context

let ltype_of_typ = function
  | A.Int -> l_int
  | A.Bool -> l_bool
  | A.Float -> l_float
  (* | A.Char -> l_char *)
  | e -> raise (Failure (Printf.sprintf "not implemented: %s" (Utils.string_of_type e)))
;;

let translate blocks =
  let the_module = L.create_module context "Fly" in
  let local_vars = StringMap.empty in
  let add_local_val typ var vars =
    print_endline "updated local var";
    let init = L.const_int (ltype_of_typ typ) 0
    in StringMap.add var (L.define_global var init the_module) vars
  in
  let process_block block vars =
    match block with
  (*   (* | SMutDeclTyped (_s, _t, _e) -> print_endline "SMutDeclTyped found" *) *)
    | SDeclTyped (_s, _t, _e) ->
             add_local_val _t _s vars
  (*     (* print_endline "SDeclTyped found" *) *)
    | b ->
      raise (Failure (Printf.sprintf "not implemented: %s" (Utils.string_of_sblock b)))
  in

  let rec process_blocks blocks vars = match blocks with 
    | [] -> ()
    | block :: rest ->
      process_blocks rest (process_block block vars)
  in
  process_blocks blocks local_vars;
  the_module
;;
