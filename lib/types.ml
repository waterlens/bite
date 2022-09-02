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
[@@deriving show]
