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

type assign_op =
  | IdentityAssign
  | PlusAssign
  | MinusAssign

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
  | TupleElements of expr list
  | Id of string
  | Binop of expr * op * expr
  | Unop of expr * op (* this is for not *)
  | UnopSideEffect of string * op (* this is for postincr, postdecr, preincr, postdecr *)
  | Assign of string * assign_op * expr
  | Call of string * expr list
    (* string for function name and list of exprs for arguments to pass to the function *)
  | UDTInstance of string * kv_list
    (* typ to indicate the exact user defined type and kv_list for member variables *)
  | UDTAccess of string * string
  | UDTStaticAccess of string * string
  | EnumAccess of string * string
  | IndexingList of string * expr
  | ListElements of expr list
  | Match of expr * (pattern * expr) list
  | Wildcard

and kv_list = (string * expr) list (* for user defined types *)

type enum_variant =
  | EnumVariantDefault of string
  | EnumVariantExplicit of string * int


type block =
  | MutDeclTyped of string * typ * expr
  | MutDeclInfer of string * expr
  | DeclTyped of string * typ * expr
  | DeclInfer of string * expr
  | Assign of string * assign_op * expr
  | FunctionDefinition of typ * string * (string * typ) list * block list (* rtyp, func_name, func_args, func_body *)
  | BoundFunctionDefinition of typ * string * (string * typ) list * block list * typ (* rtyp, func_name, func_args, func_body, bound_type *)
  | Call of string * expr list
  | EnumDeclaration of string * enum_variant list
  | UDTDef of string * (string * typ) list
  | IfEnd of expr * block list
  | IfNonEnd of expr * block list * block
  | ElifNonEnd of expr * block list * block
  | ElifEnd of expr * block list
  | ElseEnd of block list
  | While of expr * block list
  | Break
  | Continue
  | ReturnUnit
  | ReturnVal of expr

type program = { body : block list }
