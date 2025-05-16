open Sast
module StringSet = Set.Make (String)

let rec unbind_expr se =
  let t', sexpr' = se in
  match sexpr' with
  | SLiteral | SBoolLit | SFloatLit | SCharLit | SStringLit | SUnit | SEnumAccess -> t', sexpr'
  | SBinop (se1, binop, se2) -> 
  | SUnop (se, unop) -> 
  | SUnopSideEffect | SMatch | SWildcard -> failwith("Dropping")
  | SFunctionCall (func_name, func_args) ->
  | SId id -> 
  | STuple -> 
  | SFunctionCall
  | SUDTInstance
  | SUDTAccess
  | SUDTStaticAccess
  | SIndex
  | SList
  | STypeCast 

and unbind_block sblk =
  match sblk with
  | 

and unbind_block_list sblk_list variables =
  match sblk_list with
  | [] -> []
  | sblk :: rest -> 
    let unbound_sblk, updated_variables = unbind_block sblk in
    unbound_sblk :: unbind_block_list rest updated_variables


;;
