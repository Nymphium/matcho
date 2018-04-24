type e =
  | Int of int
  | Bool of bool
  | BinOp of binops * e * e
  | UniOp of uniops * e
  | Var of string
  | Lam of string * e
  | App of e * e
  | Let of string * e * e
  | Letrec of string * e * e
  | Data of string * e list
  | Case of case
  | Match of e * case list
and binops = [
  | `Add
  | `Sub
  | `Mul
  | `Div
  | `Lt
  | `Gt
  | `Eq
  | `Neq
  | `And
  | `Or
]
and uniops = [
  | `Not
  | `Minus
]
and case =
  | P of pat * e (* `pat` -> `e` *)
  | Pwhen of pat * e * e (* `pat` when `e` -> `e` *)
and pat =
  | Pint of int
  | Pbool of bool
  | Pvar of string
  | Pignore
  | Pdata of string * pat list
and v =
  | Vint of int
  | Vbool of bool
  | Vlam of string * env ref * e
  | Vdata of string * v list
  | Vcase of case
and env = (string * v) list
[@@deriving show { with_path = false }]

