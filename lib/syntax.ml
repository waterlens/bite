open Types

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

type complex_ty_param = GenericVar of string | HandlerVar of string * ty
[@@deriving show]

type simple_ty_param = string [@@deriving show]

type prog = Prog of item list

and item =
  | Func of {
      name : string;
      generic_param : complex_ty_param list;
      param : (string * ty) list;
      ty_ann : ty * ty option; (* result ty * raised ty *)
      body : stmt list;
    }
  | TyDef of tydef
  | Eff of { name : string; generic_param : simple_ty_param list; op : op_sig }

and op_sig = {
  op_name : string;
  op_generic_param : complex_ty_param list;
  op_param : ty list;
  op_ty_ann : ty * ty option;
}

and tydef =
  | Enum of {
      name : string;
      generic_param : simple_ty_param list;
      ctors : ctor list;
    }
  | Record of {
      name : string;
      generic_param : simple_ty_param list;
      fields : (string * ty) list;
    }
  | Synonym of { name : string; generic_param : simple_ty_param list; ty : ty }

and ctor = { ctor_name : string; ctor_params : ty list }

and stmt =
  | Empty
  | Bind of { name : string; ty : ty; init : expr }
  | Expr of expr
  | Ctl of ctl

and ctl =
  | Resume of expr
  | If of expr * stmt list * stmt list option
  | While of expr * stmt list
  | Try of stmt list * eff_handler list
  | Ret of expr

and eff_handler = {
  eff_name : string;
  eff_ty_ann : ty;
  handler_name : string;
  handler_ty_ann : ty * ty option;
  handler_generic_param : complex_ty_param list;
  handler_arg : (string * ty) list;
  handler_stmt : stmt list;
}

and field = Named of string | Ordinal of int

and expr =
  | BinaryExpr of binary_op * expr * expr
  | UnaryExpr of unary_op * expr
  | Literal of literal
  | ArrayExpr of expr list
  | CallExpr of expr * expr list
  | FieldExpr of expr * field
  | IndexExpr of expr * expr
  | TupleExpr of expr list
  | BlockExpr of stmt list
  | VarExpr of string
  | GenericExpr of expr * ty list
[@@deriving show]
