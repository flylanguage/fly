type op =
  | Add
  | Sub
  | Mult
  | Div
  | Mod
  | Exp
  | Preincr
  | Postincr
  | Predecr
  | Postdecr
  | Equal
  | Neq
  | Less
  | Leq
  | Greater
  | Geq
  | And
  | Or
  | Not
  | Cons

type pattern =
  | PLiteral of int
  | PBoolLit of bool
  | PFloatLit of float
  | PCharLit of char
  | PStringLit of string
  | PId of string
  | PWildcard
  | PEmptyList
  | PCons of pattern * pattern
  | PEnumAccess of string * string

type assign_op =
  | IdentityAssign
  | PlusAssign
  | MinusAssign
  | MultAssign
  | DivAssign

type typ =
  | Int
  | Bool
  | Char
  | Float
  | String
  | List of typ
  | Tuple of typ list
  | Unit
  | UserType of string

type expr =
  | Literal of int
  | BoolLit of bool
  | FloatLit of float
  | CharLit of char
  | StringLit of string
  | Unit
  | Id of string
  | Tuple of expr list
  | Binop of expr * op * expr
  | Unop of expr * op (* this is for not *)
  | UnopSideEffect of string * op (* this is for postincr, postdecr, preincr, postdecr *)
  | FunctionCall of func
  | UDTInstance of string * kv_list
  | UDTAccess of string * udt_access
  | UDTStaticAccess of string * func
  | EnumAccess of string * string
  | Index of expr * expr
  | List of expr list
  | Match of expr * (pattern * expr) list
  | Wildcard
  | TypeCast of typ * expr

and func = string * expr list
and kv_list = (string * expr) list (* for user defined types *)

and udt_access =
  | UDTVariable of string
  | UDTFunction of func

type formal = string * typ

type enum_variant =
  | EnumVariantDefault of string
  | EnumVariantExplicit of string * int

type block =
  | MutDeclTyped of string * typ * expr
  | MutDeclInfer of string * expr
  | DeclTyped of string * typ * expr
  | DeclInfer of string * expr
  | Assign of expr * assign_op * expr
  | FunctionDefinition of
      typ * string * formal list * block list (* rtyp, func_name, func_args, func_body *)
  | BoundFunctionDefinition of
      typ
      * string
      * formal list
      * block list
      * typ (* rtyp, func_name, func_args, func_body, bound_type *)
  | EnumDeclaration of string * enum_variant list
  | UDTDef of string * (string * typ) list
  | IfEnd of expr * block list
  | IfNonEnd of expr * block list * block
  | ElifNonEnd of expr * block list * block
  | ElifEnd of expr * block list
  | ElseEnd of block list
  | While of block list
  | For of string * expr * block list
  | Break
  | Continue
  | ReturnUnit
  | ReturnVal of expr
  | Expr of expr

type program = { body : block list }
