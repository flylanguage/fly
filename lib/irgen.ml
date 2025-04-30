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
    let init = L.const_int (ltype_of_typ typ) 0 in
    let local = L.define_global var init the_module in
    StringMap.add var local vars
  in
  let add_global_val typ var vars =
    let init = L.const_int (ltype_of_typ typ) 0 in
    let global = L.define_global var init the_module in
    StringMap.add var global vars
  in
  let process_block block vars (curr_func : string option) =
    match block with
    (* | SMutDeclTyped (_s, _t, _e) -> print_endline "SMutDeclTyped found" *)
    | SDeclTyped (id, typ, _expr) ->
      if Option.is_some curr_func
      then add_local_val typ id vars, curr_func
      else add_global_val typ id vars, curr_func
    | b ->
      raise (Failure (Printf.sprintf "not implemented: %s" (Utils.string_of_sblock b)))
  in
  let rec process_blocks blocks vars (curr_func : string option) =
    match blocks with
    | [] -> ()
    | block :: rest ->
      let updated_vars, updated_curr_func = process_block block vars curr_func in
      process_blocks rest updated_vars updated_curr_func
  in
  (* we start off in no function *)
  let curr_func = None in
  process_blocks blocks local_vars curr_func;
  the_module
;;
