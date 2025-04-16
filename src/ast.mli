type typ =
  | Int
  | Bool
  | Char
  | Float
  | String
  | Unit
  | List of typ
  | Tuple of typ
(* | UserType of typ *)

(* type expr = MatchMap of expr * expr *)
type expr = string
type matcher = MatchMap of expr * expr

type stmt =
  | Decl of string * typ * expr
  | Match of expr * matcher

type program = Program
