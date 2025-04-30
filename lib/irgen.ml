module L = Llvm
module A = Ast
open Sast
module StringMap = Map.Make (String)

let context = L.global_context ()

let l_int = L.i32_type context
and l_bool = L.i1_type context
and l_char = L.i8_type context
and l_unit = L.void_type context
and l_float = L.float_type context

let ltype_of_typ = function
  | A.Int -> l_int
  | A.Bool -> l_bool
  | A.Float -> l_float
  (* | A.Char -> l_char *)
  | A.Unit -> l_unit
  | e -> raise (Failure (Printf.sprintf "not implemented: %s" (Utils.string_of_type e)))
;;

let get_lformals_arr (formals : A.formal list) =
  let lformal_list = List.map ltype_of_typ (List.map snd formals) in
  Array.of_list lformal_list
;;

let translate blocks =
  let the_module = L.create_module context "Fly" in
  let local_vars = StringMap.empty in
  let add_local_val typ var vars builder =
    let local = L.build_alloca (ltype_of_typ typ) var builder in
    StringMap.add var local vars
  in
  let add_global_val typ var vars =
    let init = L.const_int (ltype_of_typ typ) 0 in
    let global = L.define_global var init the_module in
    StringMap.add var global vars
  in
  let declare_function typ id (formals : A.formal list) _body _func_blocks =
    let lfunc =
      L.define_function
        id
        (L.function_type (ltype_of_typ typ) (get_lformals_arr formals))
        the_module
    in
    let _builder = L.builder_at_end context (L.entry_block lfunc) in
    (* Add function block in blocks-to-declare list *)
    (lfunc, formals, _body) :: _func_blocks
  in
  (* Receives all func blocks after all functions have been declared and fills each func blocks' body *)
  let rec process_func_blocks func_blocks vars =
    match func_blocks with
    | [] -> ()
    | _blk :: rst ->
      process_func_block _blk vars;
      process_func_blocks rst vars
  and process_func_block (func_block : L.llvalue * A.formal list * sblock list) vars =
    let lfunc, _, blocks = func_block in
    let curr_func = Some lfunc in
    let builder = L.builder_at_end context (L.entry_block lfunc) in
    process_blocks blocks vars curr_func [] (Some builder)
  and process_block block vars (curr_func : L.llvalue option) func_blocks builder =
    match block with
    | SDeclTyped (id, typ, _expr) ->
      if Option.is_some curr_func
      then add_local_val typ id vars (Option.get builder), curr_func, func_blocks
      else add_global_val typ id vars, curr_func, func_blocks
    | SFunctionDefinition (typ, id, formals, body) ->
      let u_func_blocks = declare_function typ id formals body func_blocks in
      vars, curr_func, u_func_blocks
    | b ->
      raise (Failure (Printf.sprintf "not implemented: %s" (Utils.string_of_sblock b)))
  and process_blocks blocks vars (curr_func : L.llvalue option) func_blocks _builder =
    match blocks with
    (* We've declared all objects, lets fill in all function bodies *)
    | [] -> process_func_blocks func_blocks vars
    | block :: rest ->
      let updated_vars, updated_curr_func, u_func_blocks =
        process_block block vars curr_func func_blocks _builder
      in
      process_blocks rest updated_vars updated_curr_func u_func_blocks None
  in
  (* we start off in no function *)
  let curr_func = None in
  let func_blocks = [] in
  (* We start off with no builder *)
  let builder = None in
  process_blocks blocks local_vars curr_func func_blocks builder;
  the_module
;;
