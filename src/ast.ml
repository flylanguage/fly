type program = Program

type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq | And | Or | Not

type typ = Int | Bool | Char | Float | String | List | Tuple | Unit | UserType

type expr =
    Literal of int
  | BoolLit of bool
  | FloatLit of float
  | CharLit of char
  | StringLit of string
  | ListLit of expr list
  | TupleLit of expr list (* this is probably definitely wrong? *)
  | Id of string
  | Binop of expr * op * expr
  | Unop of op * expr (* this is for the not operator *)
  | Assign of string * expr
  | Call of string * expr list (* string for function name and list of exprs for arguments to pass to the function *)
  | UserTypeInstance of kv_list

(* type kv_list = ??? *)

type stmt =
    Block of stmt list
  | Expr of expr
  | If of expr * stmt * stmt
  | While of expr * stmt
  (* return *)
  | Return of expr (* expr for the expression to return *)

(* int x: name binding *)
type bind = typ * string

(* func_def: ret_typ fname formals locals body. This is for function declaration *)
type func_def = {
  rtyp: typ; (* return type *)
  fname: string; (* function name *)
  formals: bind list; (* list of arguments, bind is a tuple with type of the argument and name of that argument *)
  locals: bind list; (* local variables *)
  body: stmt list;
}