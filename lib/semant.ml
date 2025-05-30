open Ast
open Sast
open Utils
module StringMap = Map.Make (String)
module StringSet = Set.Make (String)

(* Type definitions for function signatures, user-defined types, and enums *)
type func_sig =
  { args : (string * resolved_typ) list
  ; rtyp : resolved_typ
  }

type udt_def =
  { members : (string * resolved_typ) list
  ; methods : string list
  }

type enum_def = enum_variant list

type envs =
  { var_env : resolved_typ StringMap.t
  ; func_env : func_sig StringMap.t
  ; udt_env : udt_def StringMap.t
  ; enum_env : enum_def StringMap.t
  }

let rec resolve_typ (ast_t : Ast.typ) (envs : envs) : resolved_typ =
  match ast_t with
  | Ast.Int -> Sast.RInt
  | Ast.Bool -> Sast.RBool
  | Ast.Char -> Sast.RChar
  | Ast.Float -> Sast.RFloat
  | Ast.String -> Sast.RString
  | Ast.Unit -> Sast.RUnit
  | Ast.TypeName name ->
    let is_enum = StringMap.mem name envs.enum_env in
    let is_udt = StringMap.mem name envs.udt_env in
    (match is_enum, is_udt with
     | true, false -> REnumType name
     | false, true -> RUserType name
     | true, true -> failwith ("Conflict Type and Enum have the same name: " ^ name)
     | false, false -> failwith ("Undefined type name: " ^ name))
  | Ast.List t1 -> Sast.RList (resolve_typ t1 envs)
  | Ast.Tuple tl -> Sast.RTuple (List.map (fun t -> resolve_typ t envs) tl)
;;

let rec find_var var_name env =
  try StringMap.find var_name env with
  | Not_found -> raise (Failure ("Undeclared variable " ^ var_name))

and find_func func_name env =
  try StringMap.find func_name env with
  | Not_found -> raise (Failure ("Undefined function " ^ func_name))

and find_udt udt_name env =
  try StringMap.find udt_name env with
  | Not_found -> raise (Failure ("Undefined user type " ^ udt_name))

and find_enum enum_name env =
  try StringMap.find enum_name env with
  | Not_found -> raise (Failure ("Undefined enum " ^ enum_name))

(* Arguments: variable name, variable type, environments *)
and var_dec_helper var_name t envs =
  let open StringMap in
  let env_checks =
    [ mem var_name envs.var_env
    ; mem var_name envs.func_env
    ; mem var_name envs.udt_env
    ; mem var_name envs.enum_env
    ]
  in
  if List.exists (fun x -> x) env_checks
  then raise (Failure (var_name ^ " already exists"))
  else StringMap.add var_name t envs.var_env

and func_def_helper func_name args rtyp envs =
  let open StringMap in
  let env_checks =
    [ mem func_name envs.var_env
    ; mem func_name envs.func_env
    ; mem func_name envs.udt_env
    ; mem func_name envs.enum_env
    ]
  in
  if List.exists (fun x -> x) env_checks
  then raise (Failure (func_name ^ "already exists"))
  else StringMap.add func_name { args; rtyp } envs.func_env

and udt_def_helper udt_name udt_members envs =
  let open StringMap in
  let env_checks =
    [ mem udt_name envs.var_env
    ; mem udt_name envs.func_env
    ; mem udt_name envs.udt_env
    ; mem udt_name envs.enum_env
    ]
  in
  if List.exists (fun x -> x) env_checks
  then raise (Failure (udt_name ^ "already exists"))
  else StringMap.add udt_name udt_members envs.udt_env

and enum_dec_helper enum_name enum_variants envs =
  let open StringMap in
  let env_checks =
    [ mem enum_name envs.var_env
    ; mem enum_name envs.func_env
    ; mem enum_name envs.udt_env
    ; mem enum_name envs.enum_env
    ]
  in
  if List.exists (fun x -> x) env_checks
  then raise (Failure (enum_name ^ "already exists"))
  else StringMap.add enum_name enum_variants envs.enum_env

and add_func_args func_args envs =
  match func_args with
  | [] -> envs.var_env
  | first_arg :: rest ->
    let new_var_env = var_dec_helper (fst first_arg) (snd first_arg) envs in
    let updated_envs = { envs with var_env = new_var_env } in
    add_func_args rest updated_envs

and add_bound_func_def func_name udt_name envs =
  let udt_exists = StringMap.find_opt udt_name envs.udt_env in
  match udt_exists with
  | Some udt_info ->
    let updated_udt_info = { udt_info with methods = func_name :: udt_info.methods } in
    StringMap.add udt_name updated_udt_info envs.udt_env
  | None -> raise (Failure ("Trying to bind to a non-existent type " ^ udt_name))

and format_ifelif_error expr t =
  Printf.sprintf
    "Expression: \'%s\' has type: %s, but if/else if conditions must be bool"
    (string_of_expr expr)
    (string_of_resolved_type t)

and check_binop expr e1 binop e2 envs special_blocks =
  let t1, e1' = check_expr e1 envs special_blocks in
  let t2, e2' = check_expr e2 envs special_blocks in
  let rtype = get_binop_return_type expr t1 binop t2 in
  rtype, SBinop ((t1, e1'), binop, (t2, e2'))

and format_binop_error expr t1 t2 =
  Printf.sprintf
    "Expression: \'%s\' LHS: %s, RHS: %s"
    (string_of_expr expr)
    (string_of_resolved_type t1)
    (string_of_resolved_type t2)

and get_binop_return_type expr t1 binop t2 =
  match binop with
  | Add | Sub | Mult | Div | Mod | Exp ->
    (match t1, t2 with
     | RInt, RInt -> RInt
     | RFloat, RFloat -> RFloat
     | RString, RString -> RString
     | _, _ -> raise (Failure (format_binop_error expr t1 t2)))
  | Equal | Neq ->
    (match t1, t2 with
     | REnumType s1, REnumType s2 when s1 = s2 -> RBool
     | _ when t1 = t2 -> RBool
     | _ -> failwith (format_binop_error expr t1 t2))
    (* if t1 = t2 then Bool else raise (Failure (format_binop_error expr t1 t2)) *)
  | Less | Leq | Greater | Geq ->
    (match t1, t2 with
     | RInt, RInt -> RBool
     | RFloat, RFloat -> RBool
     | _, _ -> raise (Failure (format_binop_error expr t1 t2)))
  | And | Or ->
    (match t1, t2 with
     | RBool, RBool -> RBool
     | _, _ -> raise (Failure (format_binop_error expr t1 t2)))
  | Cons ->
    (match t1, t2 with
     | t1', RList t2' when t1' = t2' -> RList t1'
     | _, _ -> raise (Failure (format_binop_error expr t1 t2)))
  | _ -> raise (Failure "Invalid binary operator")

and check_pattern pattern envs =
  match pattern with
  | PLiteral i -> PLiteral i
  | PBoolLit b -> PBoolLit b
  | PFloatLit f -> PFloatLit f
  | PCharLit c -> PCharLit c
  | PStringLit s -> PStringLit s
  | PId id -> PId id
  | PWildcard -> PWildcard
  | PEmptyList -> PEmptyList
  | PCons (p1, p2) -> PCons (check_pattern p1 envs, check_pattern p2 envs)
  | PEnumAccess (enum_name, variant_name) ->
    let enum_variants =
      try StringMap.find enum_name envs.enum_env with
      | Not_found -> raise (Failure ("Undefined enum " ^ enum_name))
    in
    let variant_exists =
      List.exists
        (function
          | EnumVariantDefault n | EnumVariantExplicit (n, _) -> n = variant_name)
        enum_variants
    in
    if not variant_exists
    then
      raise (Failure (Printf.sprintf "Enum %s has no variant %s" enum_name variant_name));
    PEnumAccess (enum_name, variant_name)

and update_func_body checked_func_body func_name is_unit rtyp envs =
  let rec walk_block blk recurse =
    match blk with
    | SIfNonEnd (_, if_body, other_body) ->
      walk_block_list if_body recurse && walk_block other_body recurse
    | SElifNonEnd (_, elif_body, other_body) ->
      walk_block_list elif_body recurse && walk_block other_body recurse
    | SElseEnd else_body -> walk_block_list else_body recurse
    | _ -> false
  and walk_block_list body recurse =
    match body with
    | curr_block :: rest ->
      if is_unit
      then (
        match curr_block with
        | SReturnUnit -> true
        | SIfNonEnd _ ->
          if recurse
          then (
            let res = walk_block curr_block recurse in
            if res then true else walk_block_list rest recurse)
          else walk_block_list rest recurse
        | _ -> walk_block_list rest recurse)
      else (
        match curr_block with
        | SReturnVal _ -> true
        | SIfNonEnd _ ->
          if recurse
          then (
            let res = walk_block curr_block recurse in
            if res then true else walk_block_list rest recurse)
          else walk_block_list rest recurse
        | _ -> walk_block_list rest recurse)
    | [] -> false
  in
  let top_level_ret = walk_block_list checked_func_body false in
  let nested_ret = walk_block_list checked_func_body true in
  if top_level_ret = false && nested_ret = false && not is_unit
  then raise (Failure ("Missing return statement in " ^ func_name))
  else if top_level_ret = false && is_unit
  then checked_func_body @ [ SReturnUnit ]
  else if top_level_ret = false && not is_unit
  then (
    let rec construct_default_sexpr rt envs =
      match rt with
      | Sast.RInt -> Sast.RInt, SLiteral 0
      | Sast.RBool -> Sast.RBool, SBoolLit true
      | Sast.RChar -> Sast.RChar, SCharLit '0'
      | Sast.RFloat -> Sast.RFloat, SFloatLit 0.0
      | Sast.RString -> Sast.RString, SStringLit "0"
      | Sast.REnumType name ->
        let enum_variant = List.hd (StringMap.find name envs.enum_env) in
        (match enum_variant with
         | EnumVariantDefault variant_name ->
           REnumType name, SEnumAccess ((REnumType name, SId name), variant_name)
         | EnumVariantExplicit (variant_name, _) ->
           REnumType name, SEnumAccess ((REnumType name, SId name), variant_name))
      | Sast.RUserType name ->
        let udt_info = StringMap.find name envs.udt_env in
        let udt_members = udt_info.members in
        let default_exprs =
          List.map
            (fun member -> fst member, construct_default_sexpr (snd member) envs)
            udt_members
        in
        RUserType name, SUDTInstance (name, default_exprs)
      | Sast.RList resolved_t ->
        Sast.RList resolved_t, SList [ construct_default_sexpr resolved_t envs ]
      | Sast.RTuple resolved_tl ->
        ( Sast.RTuple resolved_tl
        , STuple (List.map (fun t -> construct_default_sexpr t envs) resolved_tl) )
      | _ -> failwith "Failed to construct default expression"
    in
    let resolved_t = resolve_typ rtyp envs in
    checked_func_body @ [ SReturnVal (construct_default_sexpr resolved_t envs) ])
  else checked_func_body

and check_expr expr envs special_blocks =
  match expr with
  | Literal i -> RInt, SLiteral i
  | BoolLit b -> RBool, SBoolLit b
  | FloatLit f -> RFloat, SFloatLit f
  | CharLit c -> RChar, SCharLit c
  | StringLit s -> RString, SStringLit s
  | Unit -> RUnit, SUnit
  | Id id_name ->
    if StringMap.mem id_name envs.var_env
    then (
      let t = find_var id_name envs.var_env in
      t, SId id_name)
    else if StringMap.mem id_name envs.enum_env
    then REnumType id_name, SId id_name
    else if StringMap.mem id_name envs.udt_env
    then RUserType id_name, SId id_name
    else raise (Failure ("Undeclared variable or type " ^ id_name))
  | Tuple expr_list ->
    let sexpr_list = List.map (fun e -> check_expr e envs special_blocks) expr_list in
    let typs, _ = List.split sexpr_list in
    RTuple typs, STuple sexpr_list
  | List expr_list ->
    let sexpr_list = List.map (fun e -> check_expr e envs special_blocks) expr_list in
    let typs, _ = List.split sexpr_list in
    (match typs with
     | [] ->
       raise (Failure "Cannot infer type on empty list") (* TODO: Allow empty lists *)
     | first_typ :: rest ->
       if List.for_all (fun x -> x = first_typ) rest
       then RList first_typ, SList sexpr_list
       else raise (Failure "Lists must only have 1 type"))
  | UDTInstance (udt_name, udt_members) ->
    let udt_def = find_udt udt_name envs.udt_env in
    let def_names, _ = List.split udt_def.members in
    let instance_names, instance_exprs = List.split udt_members in
    let sexpr_list =
      List.map (fun e -> check_expr e envs special_blocks) instance_exprs
    in
    let skv_list = List.combine instance_names sexpr_list in
    let skv_list_sorted = List.map (fun n -> n, List.assoc n skv_list) def_names in
    RUserType udt_name, SUDTInstance (udt_name, skv_list_sorted)
  | Binop (e1, binop, e2) -> check_binop expr e1 binop e2 envs special_blocks
  | Unop (e, unop) ->
    let t, e' = check_expr e envs special_blocks in
    if t <> RBool
    then raise (Failure "Trying to do NOT on a non-boolean expression")
    else RBool, SUnop ((t, e'), unop)
  | UnopSideEffect (id_name, side_effect_op) ->
    let typ = find_var id_name envs.var_env in
    if typ = RInt || typ = RFloat
    then typ, SUnopSideEffect (id_name, side_effect_op)
    else raise (Failure "Trying to do increment/decrement on a non-numeric expression")
  | FunctionCall (func_name, func_args) ->
    let sfunc_args = List.map (fun arg -> check_expr arg envs special_blocks) func_args in
    let t = find_func func_name envs.func_env in
    (* t is return type of this function call *)
    t.rtyp, SFunctionCall (func_name, sfunc_args)
  | UDTAccess (udt_expr, udt_accessed_member) ->
    let udt_typ, checked_udt_expr = check_expr udt_expr envs special_blocks in
    let udt_def = find_udt (string_of_resolved_type udt_typ) envs.udt_env in
    (match udt_accessed_member with
     | UDTVariable udt_var ->
       (match List.assoc_opt udt_var udt_def.members with
        | Some accessed_type ->
          accessed_type, SUDTAccess ((udt_typ, checked_udt_expr), SUDTVariable udt_var)
        | None ->
          raise (Failure (udt_var ^ "is not in " ^ string_of_resolved_type udt_typ)))
     | UDTFunction udt_func ->
       (match List.find_opt (fun x -> x = fst udt_func) udt_def.methods with
        | Some _ ->
          let func_sig = find_func (fst udt_func) envs.func_env in
          let _, def_arg_types = List.split func_sig.args in
          let def_arg_types = List.tl def_arg_types in
          let sexpr_list =
            List.map (fun e -> check_expr e envs special_blocks) (snd udt_func)
          in
          let arg_types, _ = List.split sexpr_list in
          if arg_types = def_arg_types
          then
            ( func_sig.rtyp
            , SUDTAccess
                ((udt_typ, checked_udt_expr), SUDTFunction (fst udt_func, sexpr_list)) )
          else raise (Failure "Incorrect types passed to this method")
        | None ->
          raise
            (Failure
               (fst udt_func ^ " is not a method bound to "
                ^ string_of_resolved_type udt_typ))))
  | UDTStaticAccess (udt_name, (func_name, args)) ->
    let udt_typ = find_udt udt_name envs.udt_env in
    (match List.find_opt (fun x -> x = func_name) udt_typ.methods with
     | Some _ ->
       let func_sig = find_func func_name envs.func_env in
       let _, def_arg_types = List.split func_sig.args in
       let sexpr_list = List.map (fun e -> check_expr e envs special_blocks) args in
       let arg_types, _ = List.split sexpr_list in
       if arg_types = def_arg_types
       then func_sig.rtyp, SUDTStaticAccess (udt_name, (func_name, sexpr_list))
       else raise (Failure "Incorrect types passed to this method")
     | None -> raise (Failure (func_name ^ "is not a method bound to " ^ udt_name)))
  | EnumAccess (enum_expr, variant) ->
    let t_enum, checked_enum_expr = check_expr enum_expr envs special_blocks in
    let enum_name =
      match enum_expr with
      | Id name -> name
      | _ ->
        Printf.eprintf "EnumAccess base: %s\n" (Utils.string_of_expr enum_expr);
        failwith "EnumAccess base must be an identifier"
    in
    let enum_variants =
      try StringMap.find enum_name envs.enum_env with
      | Not_found -> raise (Failure ("Undefined enum " ^ enum_name))
    in
    let variant_exists =
      List.exists
        (function
          | EnumVariantDefault name -> name = variant
          | EnumVariantExplicit (name, _) -> name = variant)
        enum_variants
    in
    if not variant_exists
    then raise (Failure ("Undefined variant " ^ variant ^ " in enum " ^ enum_name))
    else REnumType enum_name, SEnumAccess ((t_enum, checked_enum_expr), variant)
  | Index (e1, e2) ->
    let t1, e1' = check_expr e1 envs special_blocks in
    let t2, e2' = check_expr e2 envs special_blocks in
    (match t1 with
     | RList x -> x, SIndex ((t1, e1'), (t2, e2'))
     (* This is a very weird edge case. We need to find the type of member that was accessed.
    But we can't compute the actual index the user wants in this phase??? *)
     (* | Tuple typ_list ->
      if List.length typ_list <=
      (y, SIndex ((t1, e1'), (t2, e2'))) *)
     | _ -> raise (Failure "Trying to index into an non-indexable expression"))
  | Match (matched_expr, match_arms) ->
    let s_matched = check_expr matched_expr envs special_blocks in
    let checked_arms =
      List.map
        (fun (pattern, arm_expr) ->
           let checked_pattern = check_pattern pattern envs in
           let updated_special_blocks = StringSet.add "wildcard" special_blocks in
           let s_arm_expr = check_expr arm_expr envs updated_special_blocks in
           checked_pattern, s_arm_expr)
        match_arms
    in
    let _, sexpr_list = List.split checked_arms in
    let typs, _ = List.split sexpr_list in
    (match typs with
     | [] ->
       raise (Failure "Cannot infer type on empty match expression")
       (* This should never run?*)
     | first_typ :: rest ->
       if List.for_all (fun x -> x = first_typ) rest
       then first_typ, SMatch (s_matched, checked_arms)
       else raise (Failure "All match arms must return the same type"))
  | Wildcard ->
    if StringSet.mem "wildcard" special_blocks
    then RUnit, SWildcard
    else raise (Failure "Unallowed wildcard")
  | TypeCast (target_typ, e) ->
    (* There is some really weird type casting allowed. Might need further discussion *)
    let t, e' = check_expr e envs special_blocks in
    let sexpr = t, e' in
    let result_type =
      match t, target_typ with
      (* we can throw away the result of the match because we have target_typ *)
      | RInt, Int -> RInt
      | RInt, Float -> RFloat
      | RInt, Bool -> RBool
      | RInt, String -> RString
      | RInt, Char -> RChar
      | RFloat, Int -> RInt
      | RFloat, Bool -> RBool
      | RFloat, String -> RString
      | RBool, Int -> RInt
      | RBool, Float -> RFloat
      | RBool, String -> RString
      | RChar, Int -> RInt
      | RChar, String -> RString
      | RString, Int -> RInt
      | RString, Bool -> RBool
      | RString, Char -> RChar
      | _, _ -> raise (Failure "Invalid type casting")
    in
    result_type, STypeCast (result_type, sexpr)

and check_block block envs special_blocks func_ret_type =
  (* let res_typ = resolve_typ  *)
  match block with
  | MutDeclTyped (var_name, t, e) ->
    let rt = resolve_typ t envs in
    let annotated_e = check_expr e envs special_blocks in
    let typ, _ = annotated_e in
    if typ <> rt
    then
      raise
        (Failure
           (var_name ^ " is supposed to have type " ^ string_of_type t
          ^ " but expression has type " ^ string_of_resolved_type typ))
    else (
      let new_var_env = var_dec_helper var_name rt envs in
      let updated_envs = { envs with var_env = new_var_env } in
      ( updated_envs
      , special_blocks
      , func_ret_type
      , SMutDeclTyped (var_name, rt, annotated_e) ))
  | MutDeclInfer (var_name, e) ->
    let annotated_e = check_expr e envs special_blocks in
    let typ, _ = annotated_e in
    let new_var_env = var_dec_helper var_name typ envs in
    let updated_envs = { envs with var_env = new_var_env } in
    updated_envs, special_blocks, func_ret_type, SMutDeclTyped (var_name, typ, annotated_e)
  | DeclTyped (var_name, t, e) ->
    let rt = resolve_typ t envs in
    let annotated_e = check_expr e envs special_blocks in
    let typ, _ = annotated_e in
    if typ <> rt
    then
      raise
        (Failure
           (var_name ^ " is supposed to have type " ^ string_of_type t
          ^ " but expression has type " ^ string_of_resolved_type typ))
    else (
      let new_var_env = var_dec_helper var_name rt envs in
      let updated_envs = { envs with var_env = new_var_env } in
      updated_envs, special_blocks, func_ret_type, SDeclTyped (var_name, rt, annotated_e))
  | DeclInfer (var_name, e) ->
    let annotated_e = check_expr e envs special_blocks in
    let typ, _ = annotated_e in
    let new_var_env = var_dec_helper var_name typ envs in
    let updated_envs = { envs with var_env = new_var_env } in
    updated_envs, special_blocks, func_ret_type, SDeclTyped (var_name, typ, annotated_e)
  | Assign (e1, op, e2) ->
    let annotated_e1 = check_expr e1 envs special_blocks in
    let annotated_e2 = check_expr e2 envs special_blocks in
    envs, special_blocks, func_ret_type, SAssign (annotated_e1, op, annotated_e2)
  | FunctionDefinition (rtyp, func_name, func_args, func_body) ->
    let rt = resolve_typ rtyp envs in
    let args =
      List.map (fun formal -> fst formal, resolve_typ (snd formal) envs) func_args
    in
    let new_func_env = func_def_helper func_name args rt envs in
    (* add function name to environment *)
    let updated_envs1 = { envs with func_env = new_func_env } in
    let new_var_env = add_func_args args updated_envs1 in
    (* add function arguments to environment *)
    let updated_envs2 = { updated_envs1 with var_env = new_var_env } in
    let updated_special_blocks =
      if rtyp = Unit
      then StringSet.add "ReturnUnit" special_blocks
      else StringSet.add "ReturnVal" special_blocks
    in
    let checked_func_body =
      check_block_list func_body updated_envs2 updated_special_blocks rtyp
    in
    let is_unit = rtyp = Unit in
    let updated_checked_func_body =
      update_func_body checked_func_body func_name is_unit rtyp envs
    in
    ( updated_envs1
    , updated_special_blocks
    , rtyp
    , SFunctionDefinition (rt, func_name, args, updated_checked_func_body) )
  | BoundFunctionDefinition (rtyp, func_name, func_args, func_body, bound_type) ->
    let rt = resolve_typ rtyp envs in
    let args =
      List.map (fun formal -> fst formal, resolve_typ (snd formal) envs) func_args
    in
    let bound_type = resolve_typ bound_type envs in
    let new_func_env = func_def_helper func_name args rt envs in
    (* add function name to environment *)
    let updated_envs1 = { envs with func_env = new_func_env } in
    let new_udt_env =
      add_bound_func_def func_name (string_of_resolved_type bound_type) envs
    in
    let updated_envs2 = { updated_envs1 with udt_env = new_udt_env } in
    let new_var_env = add_func_args args updated_envs1 in
    (* add function arguments to environment *)
    let updated_envs3 = { updated_envs2 with var_env = new_var_env } in
    let updated_special_blocks =
      if rtyp = Unit
      then StringSet.add "ReturnUnit" special_blocks
      else StringSet.add "ReturnVal" special_blocks
    in
    let checked_func_body =
      check_block_list func_body updated_envs3 updated_special_blocks rtyp
    in
    let is_unit = rtyp = Unit in
    let updated_checked_func_body =
      update_func_body checked_func_body func_name is_unit rtyp envs
    in
    ( updated_envs2
    , updated_special_blocks
    , rtyp
    , SBoundFunctionDefinition (rt, func_name, args, updated_checked_func_body, bound_type)
    )
  | EnumDeclaration (enum_name, enum_variants) ->
    let new_enum_env = enum_dec_helper enum_name enum_variants envs in
    let updated_envs = { envs with enum_env = new_enum_env } in
    let convert_enum_variant = function
      | EnumVariantDefault name -> SEnumVariantDefault name
      | EnumVariantExplicit (name, value) -> SEnumVariantExplicit (name, value)
    in
    let senum_variants = List.map convert_enum_variant enum_variants in
    ( updated_envs
    , special_blocks
    , func_ret_type
    , SEnumDeclaration (enum_name, senum_variants) )
  | UDTDef (udt_name, udt_members) ->
    let members =
      List.map (fun formal -> fst formal, resolve_typ (snd formal) envs) udt_members
    in

    let initial_udt_def = { members; methods = [] } in
    (* follow type definition udt_def above *)
    let new_udt_env = udt_def_helper udt_name initial_udt_def envs in
    let updated_envs = { envs with udt_env = new_udt_env } in
    updated_envs, special_blocks, func_ret_type, SUDTDef (udt_name, members)
  | IfEnd (condition, body) ->
    let checked_condition = check_expr condition envs special_blocks in
    let t, _ = checked_condition in
    if t <> RBool
    then raise (Failure (format_ifelif_error condition t))
    else (
      let checked_body = check_block_list body envs special_blocks func_ret_type in
      envs, special_blocks, func_ret_type, SIfEnd (checked_condition, checked_body))
  | IfNonEnd (condition, body, other_arm) ->
    let checked_condition = check_expr condition envs special_blocks in
    let t, _ = checked_condition in
    if t <> RBool
    then raise (Failure (format_ifelif_error condition t))
    else (
      let checked_body = check_block_list body envs special_blocks func_ret_type in
      let _, _, _, checked_other_arm =
        check_block other_arm envs special_blocks func_ret_type
      in
      ( envs
      , special_blocks
      , func_ret_type
      , SIfNonEnd (checked_condition, checked_body, checked_other_arm) ))
  | ElifNonEnd (condition, body, other_arm) ->
    let checked_condition = check_expr condition envs special_blocks in
    let t, _ = checked_condition in
    if t <> RBool
    then raise (Failure (format_ifelif_error condition t))
    else (
      let checked_body = check_block_list body envs special_blocks func_ret_type in
      let _, _, _, checked_other_arm =
        check_block other_arm envs special_blocks func_ret_type
      in
      ( envs
      , special_blocks
      , func_ret_type
      , SElifNonEnd (checked_condition, checked_body, checked_other_arm) ))
  | ElifEnd (condition, body) ->
    let checked_condition = check_expr condition envs special_blocks in
    let t, _ = checked_condition in
    if t <> RBool
    then raise (Failure (format_ifelif_error condition t))
    else (
      let checked_body = check_block_list body envs special_blocks func_ret_type in
      envs, special_blocks, func_ret_type, SElifEnd (checked_condition, checked_body))
  | ElseEnd body ->
    let checked_body = check_block_list body envs special_blocks func_ret_type in
    envs, special_blocks, func_ret_type, SElseEnd checked_body
  | While (condition, body) ->
    let checked_condition = check_expr condition envs special_blocks in
    let t, _ = checked_condition in
    if t <> RBool
    then
      raise
        (Failure
           ("Expression: '" ^ string_of_expr condition ^ "' has type "
          ^ string_of_resolved_type t ^ ", but while loop conditions must be bool"))
    else (
      let updated_special_blocks =
        StringSet.add "break" (StringSet.add "continue" special_blocks)
      in
      let checked_body =
        check_block_list body envs updated_special_blocks func_ret_type
      in
      envs, updated_special_blocks, func_ret_type, SWhile (checked_condition, checked_body))
  | For (loop_var, iterable, body) ->
    (* Throw an error if the loop variable has been previously defined *)
    (match StringMap.find_opt loop_var envs.var_env with
     | Some _ -> raise (Failure ("Loop variable " ^ loop_var ^ " previously defined"))
     | None -> ());
    let checked_iterable = check_expr iterable envs special_blocks in
    let t, _ = checked_iterable in
    (match t with
     | RList _ | RTuple _ ->
       let new_var_env = var_dec_helper loop_var RInt envs in
       let updated_envs = { envs with var_env = new_var_env } in
       let updated_special_blocks =
         StringSet.add "break" (StringSet.add "continue" special_blocks)
       in
       let checked_body =
         check_block_list body updated_envs updated_special_blocks func_ret_type
       in
       ( envs
       , updated_special_blocks
       , func_ret_type
       , SFor (loop_var, checked_iterable, checked_body) )
     | _ ->
       raise
         (Failure
            ("Expression '" ^ string_of_expr iterable ^ "' has type "
           ^ string_of_resolved_type t ^ " and is not iterable")))
  | Break ->
    if StringSet.mem "break" special_blocks
    then envs, special_blocks, func_ret_type, SBreak
    else raise (Failure "Unallowed break statement")
  | Continue ->
    if StringSet.mem "continue" special_blocks
    then envs, special_blocks, func_ret_type, SContinue
    else raise (Failure "Unallowed continue statement")
  | ReturnUnit ->
    (match StringSet.find_opt "ReturnUnit" special_blocks with
     | None -> raise (Failure "Unallowed return statement")
     | Some _ -> envs, special_blocks, func_ret_type, SReturnUnit)
  | ReturnVal return_expr ->
    (match StringSet.find_opt "ReturnVal" special_blocks with
     | None -> raise (Failure "Unallowed return statement")
     | Some _ ->
       let checked_expr = check_expr return_expr envs special_blocks in
       let t, _ = checked_expr in
       let rfunc_ret_type = resolve_typ func_ret_type envs in
       if t <> rfunc_ret_type
       then
         raise
           (Failure
              ("Expression '" ^ string_of_expr return_expr ^ "' has type "
             ^ string_of_resolved_type t ^ " but expected " ^ string_of_type func_ret_type
              ))
       else envs, special_blocks, func_ret_type, SReturnVal checked_expr)
  | Expr expr ->
    let checked_expr = check_expr expr envs special_blocks in
    envs, special_blocks, func_ret_type, SExpr checked_expr

and check_block_list block_list envs special_blocks func_ret_type =
  match block_list with
  | [] -> []
  | curr_block :: rest ->
    let updated_envs, updated_special_blocks, _, sblock =
      check_block curr_block envs special_blocks func_ret_type
    in
    sblock :: check_block_list rest updated_envs updated_special_blocks func_ret_type
;;

let check block_list =
  (* 4 separate hash maps for variables, function definitions, user defined types and enums *)
  let initial_envs : envs =
    { var_env = StringMap.empty
    ; func_env = StringMap.empty
    ; udt_env = StringMap.empty
    ; enum_env = StringMap.empty
    }
  in

  (* Add "print" function *)
  let new_func_env = func_def_helper Sast.print_func_name [] RUnit initial_envs in
  let envs = { initial_envs with func_env = new_func_env } in
  (* Add "len" function *)
  let new_func_env2 = func_def_helper Sast.len_func_name [] RInt envs in
  let envs2 = { envs with func_env = new_func_env2 } in

  (* Add "input" function *)
  let new_func_env3 = func_def_helper Sast.input_func_name [] RString envs2 in
  let envs3 = { envs2 with func_env = new_func_env3 } in

  (* Special blocks are limited to return, continue, break, wildcard.
   We need this to indicate whether these symbols are allowed in their current context.
   For example, a return is only allowed inside a function defintion and a break is only allowed inside a loop 
   I should really come up with a better name for this *)
  let special_blocks = StringSet.empty in
  check_block_list block_list envs3 special_blocks Unit
;;
