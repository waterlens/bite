type ty =
  | TyUnit
  | TyInt
  | TyFloat
  | TyBool
  | TyStr
  | TyHole
  | Ident of string
  | QuotedIdent of string
  | TyApp of ty * ty list
  | TyProd of ty list
  | TySum of ty list
  | TyArrow of ty * ty * ty
  | TyConstr of ty * ty
[@@deriving show]

type binary_op =
  | Add
  | Sub
  | Mul
  | Div
  | Mod
  | Eq
  | Neq
  | Gt
  | Ge
  | Lt
  | Le
  | LAnd
  | LOr
[@@deriving show]

type unary_op = LNot | Plus | Minus [@@deriving show]

type literal =
  | IntLiteral of int
  | FloatLiteral of float
  | BoolLiteral of bool
  | StringLiteral of string
[@@deriving show]

type prog = Prog of item list

and item =
  | Module of { name : string option; items : item list }
  | Use of path
  | Func of {
      name : string;
      ty_param : ty list;
      ty : ty;
      tm_param : string option list;
      body : stmt list;
    }
  | TyDef of ty_def
  | Eff of { name : string; ty_param : ty list; handlers : handler_sig list }

and handler_sig = { name : string; ty_param : ty list; ty : ty }

and ty_def =
  | Enum of {
      name : string;
      ty_param : ty list;
      ctors : (string * ty * string option list) list;
    }
  | Record of { name : string; ty_param : ty list; fields : (string * ty) list }
  | Synonym of { name : string; ty_param : ty list; ty : ty }

and path = (string * ty list option) list

and stmt =
  | Empty
  | Item of item
  | Bind of { name : string; mut : bool; ty : ty; init : expr }
  | Expr of expr
  | Ctl of ctl

and ctl =
  | Resume of expr
  | If of expr * stmt list * stmt list option
  | While of expr * stmt list
  | Try of stmt list * (string * ty * eff_handler list) list
  | Ret of expr

and eff_handler = {
  h_name : string;
  h_ty_arg : ty list;
  h_ty : ty;
  h_tm_arg : string list;
  h_stmt : stmt list;
}

and field = Named of string | Ordinal of int

and expr =
  | BinaryExpr of binary_op * expr * expr
  | UnaryExpr of unary_op * expr
  | Literal of literal
  | ArrayExpr of expr list
  | CallExpr of expr * expr list
  | FieldExpr of expr * field
  | PathExpr of path
  | IndexExpr of expr * expr
  | RecordExpr of string * ty list option * (string * expr option) list
  | TupleExpr of expr list
  | BlockExpr of stmt list
[@@deriving show]
