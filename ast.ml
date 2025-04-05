(* Abstract syntax tree *)

type bop = Add | Sub | Mult | Div | Mod | Exp | Equal | Neq 
| Less | More | And | Or | Leq | Geq (* | assign? | Fdiv *)

type uop = Not | Preincr | Postincr | Predecr | Postdecr