open Ast
open Sast
open Utils
module StringMap = Map.Make (String)
module StringSet = Set.Make (String)

(* Type definitions for function signatures, user-defined types, and enums *)
type func_sig =
  { args : (string * typ) list
  ; rtyp : typ
  }

type udt_def =
  { members : (string * typ) list
  ; methods : string list
  }

type enum_def = enum_variant list

type envs =
  { var_env : typ StringMap.t
  ; func_env : func_sig StringMap.t
  ; udt_env : udt_def StringMap.t
  ; enum_env : enum_def StringMap.t
  }

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
  then raise (Failure (var_name ^ "already exists"))
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
    (string_of_type t)

and check_binop expr e1 binop e2 envs special_blocks =
  let t1, e1' = check_expr e1 envs special_blocks in
  let t2, e2' = check_expr e2 envs special_blocks in
  let rtype = get_binop_return_type expr t1 binop t2 in
  rtype, SBinop ((t1, e1'), binop, (t2, e2'))

and format_binop_error expr t1 t2 =
  Printf.sprintf
    "Expression: \'%s\' LHS: %s, RHS: %s"
    (string_of_expr expr)
    (string_of_type t1)
    (string_of_type t2)

and get_binop_return_type expr t1 binop t2 =
  match binop with
  | Add | Sub | Mult | Div | Mod | Exp ->
    (match t1, t2 with
     | Int, Int -> Int
     | Float, Float -> Float
     | _, _ -> raise (Failure (format_binop_error expr t1 t2)))
  | Equal | Neq ->
    if t1 = t2 then Bool else raise (Failure (format_binop_error expr t1 t2))
  | Less | Leq | Greater | Geq ->
    (match t1, t2 with
     | Int, Int -> Bool
     | Float, Float -> Bool
     | _, _ -> raise (Failure (format_binop_error expr t1 t2)))
  | And | Or ->
    (match t1, t2 with
     | Bool, Bool -> Bool
     | _, _ -> raise (Failure (format_binop_error expr t1 t2)))
  | Cons ->
    (match t1, t2 with
     | t1', List t2' when t1' = t2' -> List t1'
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

and check_expr expr envs special_blocks =
  match expr with
  | Literal i -> Int, SLiteral i
  | BoolLit b -> Bool, SBoolLit b
  | FloatLit f -> Float, SFloatLit f
  | CharLit c -> Char, SCharLit c
  | StringLit s -> String, SStringLit s
  | Unit -> Unit, SUnit
  | Id id_name ->
    let t = find_var id_name envs.var_env in
    t, SId id_name
  | Tuple expr_list ->
    let sexpr_list = List.map (fun e -> check_expr e envs special_blocks) expr_list in
    let typs, _ = List.split sexpr_list in
    Tuple typs, STuple sexpr_list
  | List expr_list ->
    let sexpr_list = List.map (fun e -> check_expr e envs special_blocks) expr_list in
    let typs, _ = List.split sexpr_list in
    (match typs with
     | [] ->
       raise (Failure "Cannot infer type on empty list") (* TODO: Allow empty lists *)
     | first_typ :: rest ->
       if List.for_all (fun x -> x = first_typ) rest
       then List first_typ, SList sexpr_list
       else raise (Failure "Lists must only have 1 type"))
  | UDTInstance (udt_name, udt_members) ->
    let udt_def = find_udt udt_name envs.udt_env in
    (* udt_def is a (string * typ) list *)
    let def_names, def_types = List.split udt_def.members in
    let instance_names, instance_exprs = List.split udt_members in
    if def_names = instance_names
    then (
      (* this means the order of members in the instance must in the same order as definition *)
      let sexpr_list =
        List.map (fun e -> check_expr e envs special_blocks) instance_exprs
      in
      let instance_types, _ = List.split sexpr_list in
      if def_types = instance_types
      then (
        let skv_list = List.combine instance_names sexpr_list in
        UserType udt_name, SUDTInstance (udt_name, skv_list))
      else raise (Failure ("Incorrect types used to instantiate " ^ udt_name)))
    else raise (Failure ("Incorrect ordering when instantiating " ^ udt_name))
  | Binop (e1, binop, e2) -> check_binop expr e1 binop e2 envs special_blocks
  | Unop (e, unop) ->
    let t, e' = check_expr e envs special_blocks in
    if t <> Bool
    then raise (Failure "Trying to do NOT on a non-boolean expression")
    else Bool, SUnop ((t, e'), unop)
  | UnopSideEffect (id_name, side_effect_op) ->
    let typ = find_var id_name envs.var_env in
    if typ = Int || typ = Float
    then typ, SUnopSideEffect (id_name, side_effect_op)
    else raise (Failure "Trying to do increment/decrement on a non-numeric expression")
  | FunctionCall (func_name, func_args) ->
    let sfunc_args = List.map (fun arg -> check_expr arg envs special_blocks) func_args in
    let t = find_func func_name envs.func_env in
    (* t is return type of this function call *)
    t.rtyp, SFunctionCall (func_name, sfunc_args)
  | UDTAccess (id_name, udt_accessed_member) ->
    let udt_typ = find_var id_name envs.var_env in
    let udt_def = find_udt (string_of_type udt_typ) envs.udt_env in
    (match udt_accessed_member with
     | UDTVariable udt_var ->
       (match List.assoc_opt udt_var udt_def.members with
        | Some accessed_type -> accessed_type, SUDTAccess (id_name, SUDTVariable udt_var)
        | None -> raise (Failure (udt_var ^ "is not in " ^ string_of_type udt_typ)))
     | UDTFunction udt_func ->
       (match List.find_opt (fun x -> x = fst udt_func) udt_def.methods with
        | Some _ ->
          let func_sig = find_func (fst udt_func) envs.func_env in
          let _, def_arg_types = List.split func_sig.args in
          let sexpr_list =
            List.map (fun e -> check_expr e envs special_blocks) (snd udt_func)
          in
          let arg_types, _ = List.split sexpr_list in
          if arg_types = def_arg_types
          then func_sig.rtyp, SUDTAccess (id_name, SUDTFunction (fst udt_func, sexpr_list))
          else raise (Failure "Incorrect types passed to this method")
        | None ->
          raise
            (Failure (fst udt_func ^ "is not a method bound to " ^ string_of_type udt_typ))))
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
  | EnumAccess (enum_name, variant) ->
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
    else Int, SEnumAccess (enum_name, variant)
  | Index (e1, e2) ->
    let t1, e1' = check_expr e1 envs special_blocks in
    let t2, e2' = check_expr e2 envs special_blocks in
    (match t1 with
     | List x -> x, SIndex ((t1, e1'), (t2, e2'))
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
    then Unit, SWildcard
    else raise (Failure "Unallowed wildcard")
  | TypeCast (target_typ, e) ->
    (* There is some really weird type casting allowed. Might need further discussion *)
    let t, e' = check_expr e envs special_blocks in
    let sexpr = t, e' in
    let _ =
      match t, target_typ with
      (* we can throw away the result of the match because we have target_typ *)
      | Int, Int -> Int
      | Int, Float -> Float
      | Int, Bool -> Bool
      | Int, String -> String
      | Int, Char -> Char
      | Float, Int -> Int
      | Float, Bool -> Bool
      | Float, String -> String
      | Bool, Int -> Int
      | Bool, Float -> Float
      | Bool, String -> String
      | Char, Int -> Int
      | Char, String -> String
      | String, Int -> Int
      | String, Bool -> Bool
      | String, Char -> Char
      | _, _ -> raise (Failure "Invalid type casting")
    in
    target_typ, STypeCast (target_typ, sexpr)

and check_block block envs special_blocks func_ret_type =
  match block with
  | MutDeclTyped (var_name, t, e) ->
    let annotated_e = check_expr e envs special_blocks in
    let typ, _ = annotated_e in
    if typ <> t
    then
      raise
        (Failure
           (var_name ^ " is supposed to have type " ^ string_of_type t
          ^ " but expression has type " ^ string_of_type typ))
    else (
      let new_var_env = var_dec_helper var_name t envs in
      let updated_envs = { envs with var_env = new_var_env } in
      updated_envs, special_blocks, func_ret_type, SMutDeclTyped (var_name, t, annotated_e))
  | MutDeclInfer (var_name, e) ->
    let annotated_e = check_expr e envs special_blocks in
    let typ, _ = annotated_e in
    let new_var_env = var_dec_helper var_name typ envs in
    let updated_envs = { envs with var_env = new_var_env } in
    updated_envs, special_blocks, func_ret_type, SMutDeclTyped (var_name, typ, annotated_e)
  | DeclTyped (var_name, t, e) ->
    let annotated_e = check_expr e envs special_blocks in
    let typ, _ = annotated_e in
    if typ <> t
    then
      raise
        (Failure
           (var_name ^ " is supposed to have type " ^ string_of_type t
          ^ " but expression has type " ^ string_of_type typ))
    else (
      let new_var_env = var_dec_helper var_name t envs in
      let updated_envs = { envs with var_env = new_var_env } in
      updated_envs, special_blocks, func_ret_type, SDeclTyped (var_name, t, annotated_e))
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
    let new_func_env = func_def_helper func_name func_args rtyp envs in
    (* add function name to environment *)
    let updated_envs1 = { envs with func_env = new_func_env } in
    let new_var_env = add_func_args func_args updated_envs1 in
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
    ( updated_envs2
    , updated_special_blocks
    , rtyp
    , SFunctionDefinition (rtyp, func_name, func_args, checked_func_body) )
  | BoundFunctionDefinition (rtyp, func_name, func_args, func_body, bound_type) ->
    let new_func_env = func_def_helper func_name func_args rtyp envs in
    (* add function name to environment *)
    let updated_envs1 = { envs with func_env = new_func_env } in
    let new_var_env = add_func_args func_args updated_envs1 in
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
    let new_udt_env = add_bound_func_def func_name (string_of_type bound_type) envs in
    let updated_envs3 = { updated_envs2 with udt_env = new_udt_env } in
    ( updated_envs3
    , updated_special_blocks
    , rtyp
    , SBoundFunctionDefinition (rtyp, func_name, func_args, checked_func_body, bound_type)
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
    let initial_udt_def = { members = udt_members; methods = [] } in
    (* follow type definition udt_def above *)
    let new_udt_env = udt_def_helper udt_name initial_udt_def envs in
    let updated_envs = { envs with udt_env = new_udt_env } in
    updated_envs, special_blocks, func_ret_type, SUDTDef (udt_name, udt_members)
  | IfEnd (condition, body) ->
    let checked_condition = check_expr condition envs special_blocks in
    let t, _ = checked_condition in
    if t <> Bool
    then raise (Failure (format_ifelif_error condition t))
    else (
      let checked_body = check_block_list body envs special_blocks func_ret_type in
      envs, special_blocks, func_ret_type, SIfEnd (checked_condition, checked_body))
  | IfNonEnd (condition, body, other_arm) ->
    let checked_condition = check_expr condition envs special_blocks in
    let t, _ = checked_condition in
    if t <> Bool
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
    if t <> Bool
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
    if t <> Bool
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
    if t <> Bool
    then
      raise
        (Failure
           ("Expression: '" ^ string_of_expr condition ^ "' has type " ^ string_of_type t
          ^ ", but while loop conditions must be bool"))
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
     | List _ | Tuple _ ->
       let new_var_env = var_dec_helper loop_var Int envs in
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
            ("Expression '" ^ string_of_expr iterable ^ "' has type " ^ string_of_type t
           ^ " and is not iterable")))
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
       if t <> func_ret_type
       then
         raise
           (Failure
              ("Expression '" ^ string_of_expr return_expr ^ "' has type "
             ^ string_of_type t ^ "but expected " ^ string_of_type func_ret_type))
       else envs, special_blocks, func_ret_type, SReturnVal checked_expr)
  | Expr expr ->
    let checked_expr = check_expr expr envs special_blocks in
    envs, special_blocks, func_ret_type, SExpr checked_expr

and check_block_list block_list envs special_blocks func_ret_type =
  match block_list with
  | [] -> []
  | curr_block :: rest ->
    let updated_envs, updated_special_blocks, updated_func_ret_type, sblock =
      check_block curr_block envs special_blocks func_ret_type
    in
    sblock
    :: check_block_list rest updated_envs updated_special_blocks updated_func_ret_type
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
  let new_func_env = func_def_helper Sast.print_func_name [] Ast.Unit initial_envs in
  let envs = { initial_envs with func_env = new_func_env } in
  (* Add "len" function *)
  let new_func_env2 = func_def_helper Sast.len_func_name [] Ast.Int envs in
  let envs2 = { envs with func_env = new_func_env2 } in

  (* Add "input" function *)
  let new_func_env3 = func_def_helper Sast.input_func_name [] Ast.String envs2 in
  let envs3 = { envs2 with func_env = new_func_env3 } in

  (* Special blocks are limited to return, continue, break, wildcard.
   We need this to indicate whether these symbols are allowed in their current context.
   For example, a return is only allowed inside a function defintion and a break is only allowed inside a loop 
   I should really come up with a better name for this *)
  let special_blocks = StringSet.empty in
  check_block_list block_list envs3 special_blocks Unit
;;
