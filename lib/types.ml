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
  | TyArray of ty
  | TyVar of tvar ref
  | TyAbs of ty
and tvar = 
  | Generic of int
  | Link of ty
  | Bound of int
  | Unbound of int
[@@deriving show]
