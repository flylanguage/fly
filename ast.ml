(* Abstract syntax tree *)

type bop = Add | Sub | Mult | Div | Mod | Exp | Equal | Neq 
| Less | More | And | Or | Leq | Geq (* | Fdiv *)

type typ = Int | Bool (* | Float | Char *)

type expr =
    IntLit of int
(*| FloatLit of float *)
(*| CharLit of char *)
  | BoolLit of bool
  | Id of string
  (*| List of expr list *)
  | Binop of expr * bop * expr
  (* what about unary operations Not | Preincr | Postincr | Predecr | Postdecr ? *)
  | Assign of string * expr
  | Walrus of string * expr
  | Call of string * expr list (* function call *)

(* int x: name binding *)
type bind = typ * string