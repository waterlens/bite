open Syntax

exception Error of string

let combine_arithmetic_operands_type ty1 ty2 =
  match (ty1, ty2) with
  | TyInt, TyInt | TyFloat, TyFloat -> Some ty1
  | _ -> None

let combine_logical_operands_type ty1 ty2 =
  match (ty1, ty2) with TyBool, TyBool -> Some TyBool | _ -> None

let combine_relational_operands_type ty1 ty2 =
  match (ty1, ty2) with
  | TyInt, TyInt | TyBool, TyBool | TyFloat, TyFloat -> Some TyBool
  | _ -> None

let combine_equality_operands_type ty1 ty2 =
  match (ty1, ty2) with
  | TyInt, TyInt
  | TyBool, TyBool
  | TyFloat, TyFloat
  | TyStr, TyStr
  | TyUnit, TyUnit ->
      Some TyBool
  | _ -> None

let rec type_check env expr =
  match expr with
  | BinaryExpr (op, lhs, rhs) -> (
      let ty1 = type_check env lhs in
      let ty2 = type_check env rhs in
      match op with
      | Add | Sub | Mul | Div | Mod -> type_check_arithmetic_expr op ty1 ty2
      | LAnd | LOr -> type_check_logical_expr op ty1 ty2
      | Eq | Neq -> type_check_equality_expr op ty1 ty2
      | Le | Lt | Ge | Gt -> type_check_relational_expr op ty1 ty2)
  | UnaryExpr (op, expr) -> (
      let ty = type_check env expr in
      match (ty, op) with
      | TyInt, Plus
      | TyInt, Minus
      | TyFloat, Plus
      | TyFloat, Minus
      | TyBool, LNot ->
          ty
      | _ -> raise @@ Error "type check failed")
  | Literal (IntLiteral _) -> TyInt
  | Literal (FloatLiteral _) -> TyFloat
  | Literal (StringLiteral _) -> TyStr
  | _ -> TyUnit

and type_check_arithmetic_expr op ty1 ty2 =
  match combine_arithmetic_operands_type ty1 ty2 with
  | Some ty -> (
      match op with
      | Add | Sub | Mul -> ty
      | Div | Mod -> ty
      | _ -> raise @@ Error "not an arithmetic operator")
  | None -> raise @@ Error "type check failed"

and type_check_logical_expr op ty1 ty2 =
  match combine_logical_operands_type ty1 ty2 with
  | Some ty -> (
      match op with
      | LAnd | LOr -> ty
      | _ -> raise @@ Error "not an logical operator")
  | None -> raise @@ Error "type check failed"

and type_check_relational_expr op ty1 ty2 =
  match combine_relational_operands_type ty1 ty2 with
  | Some ty -> (
      match op with
      | Lt | Le | Gt | Ge -> ty
      | _ -> raise @@ Error "not an relational operator")
  | None -> raise @@ Error "type check failed"

and type_check_equality_expr op ty1 ty2 =
  match combine_relational_operands_type ty1 ty2 with
  | Some ty -> (
      match op with
      | Eq | Neq -> ty
      | _ -> raise @@ Error "not an equality operator")
  | None -> raise @@ Error "type check failed"