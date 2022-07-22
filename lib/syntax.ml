type ty =
  | Unit
  | Int
  | Float
  | Bool
  | Str
  | Hole
  | Ident of string
  | QuotedIdent of string
  | App of ty * ty list
  | Prod of ty list
  | Sum of ty list
  | Arrow of ty * ty * ty
  | Constr of ty * ty

type prog = Prog of item list

and item =
  | Module of { name : string; items : item list }
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
      ctors : string * ty * string option list;
    }
  | Record of { name : string; ty_param : ty list; fields : string * ty }
  | Synonym of { name : string; ty_param : ty list; ty : ty }

and path = (string * ty list) list

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
  handler_name : string;
  handler_ty_arg : ty list option;
  handler_ty : ty option;
  handler_tm_arg : string list;
  handler_stmt : stmt list;
}

and expr = unit