open Ast

type sexpr = typ * sx

and sx =
  | SLiteral of int
  | SBoolLit of bool
  | SFloatLit of float
  | SCharLit of char
  | SStringLit of string
  | SUnit
  | SId of string
  | STuple of sexpr list
  | SBinop of sexpr * op * sexpr
  | SUnop of sexpr * op (* this is for not *)
  | SUnopSideEffect of string * op (* this is for postincr, postdecr, preincr, postdecr *)
  | SFunctionCall of sfunc
  | SUDTInstance of string * skv_list
  | SUDTAccess of string * sudt_access
  | SUDTStaticAccess of string * sfunc
  | SEnumAccess of string * string
  | SIndex of sexpr * sexpr
  | SList of sexpr list
  | SMatch of sexpr * (pattern * sexpr) list
  | SWildcard
  | STypeCast of typ * sexpr

and sfunc = string * sexpr list
and skv_list = (string * sexpr) list (* for user defined types *)

and sudt_access =
  | SUDTVariable of string
  | SUDTFunction of sfunc

type senum_variant =
  | SEnumVariantDefault of string
  | SEnumVariantExplicit of string * int

type sblock =
  | SMutDeclTyped of string * typ * sexpr
  | SDeclTyped of string * typ * sexpr
  | SAssign of sexpr * assign_op * sexpr
  | SFunctionDefinition of
      typ * string * formal list * sblock list (* rtyp, func_name, func_args, func_body *)
  | SBoundFunctionDefinition of
      typ
      * string
      * formal list
      * sblock list
      * typ (* rtyp, func_name, func_args, func_body, bound_type *)
  | SEnumDeclaration of string * senum_variant list
  | SUDTDef of string * (string * typ) list
  | SIfEnd of sexpr * sblock list
  | SIfNonEnd of sexpr * sblock list * sblock
  | SElifNonEnd of sexpr * sblock list * sblock
  | SElifEnd of sexpr * sblock list
  | SElseEnd of sblock list
  | SWhile of sexpr * sblock list
  | SFor of string * sexpr * sblock list
  | SBreak
  | SContinue
  | SReturnUnit
  | SReturnVal of sexpr
  | SExpr of sexpr

(* Name of our "print" function *)
let print_func_name = "print"

(* Name of our "len" function *)
let len_func_name = "len"

(* Name of our "input" function *)
let input_func_name = "input"

type sprogram = { body : sblock list }
