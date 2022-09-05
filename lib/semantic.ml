open Syntax
open Types
open Errors
open Scope
open Trie

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

let type_eq env t1 t2 = true

type bfunction = { name : string }

type bmodule = {
  name : string;
  functions : unit list;
  types : unit list;
  effects : unit list;
  mods : bmodule list;
}

type mod_env = {
  mods : (string, bmodule) trie;
  uses : (string, bmodule) trie list;
  curmod : bmodule * string list;
}

type ty_env = { types : (string, ty) scope }
type ctor_env = { ctors: (string, ty) scope }

let create_module menv name =
  let new_mod = { name; functions = []; types = []; effects = []; mods = [] } in
  match menv.curmod with
  | _, path ->
      let new_path = name :: path in
      let rev_path = List.rev new_path in
      let curmod = (new_mod, new_path) in
      let mods = Trie.insert menv.mods rev_path new_mod in
      let uses = menv.mods :: menv.uses in
      { curmod; mods; uses }

let update_use menv path =
  let { mods; uses; curmod } = menv in
  let last_use = List.hd uses in
  let used_mod = Trie.find last_use path in
  let new_use = Trie.insert last_use path used_mod in
  let uses = new_use :: List.tl uses in
  { mods; uses; curmod }

let used_path path =
  let f = function
    | s, None -> s
    | _, Some _ -> raise @@ Error "use path can't have type parameters"
  in
  List.map f path

let rec extend_ty_env_with_ty_param_from n tenv ty_param =
  match ty_param with
  | [] -> tenv
  | x :: xs ->
      (match x with
      | QuotedIdent s | Ident s -> Scope.insert tenv.types s (TyVar n)
      | _ -> raise @@ Error "type parameter is not a variable");
      extend_ty_env_with_ty_param_from (n + 1) tenv xs

let extend_ty_env_with_ty_param = extend_ty_env_with_ty_param_from 0

let rec subst_named_ty tenv ty allow_hole =
  let subst ty = subst_named_ty tenv ty allow_hole in
  let substs tys = subst_named_ty_list tenv tys allow_hole in
  match ty with
  | TyUnit | TyInt | TyFloat | TyBool | TyStr -> ty
  | TyHole ->
      if allow_hole then ty else raise @@ Error "hole type is not allowed"
  | Ident s | QuotedIdent s -> Scope.lookup tenv.types s
  | TyApp (t1, t2) -> TyApp (subst t1, substs t2)
  | TyProd tls -> TyProd (substs tls)
  | TySum tls -> TyProd (substs tls)
  | TyArrow (t1, t2, t3) -> TyArrow (subst t1, subst t2, subst t3)
  | TyConstr (t1, t2) -> TyConstr (subst t1, subst t2)
  | TyArray t -> TyArray (subst t)
  | TyVar _ | TyAbs _ -> ty

and subst_named_ty_list tenv tys allow_hole =
  List.map (fun ty -> subst_named_ty tenv ty allow_hole) tys

let split_ctor_param_ty ctor_ty =
  match ctor_ty with
  | TyArrow (a, _, _) -> a
  | _ -> raise @@ Error "unexpected ctor type"

let mk_sum_ty tenv ty_param ctors =
  let tenv = { types = Scope.entry tenv.types } in
  let tenv = extend_ty_env_with_ty_param tenv ty_param in
  let tys = List.map split_ctor_param_ty ctors in
  let tys = subst_named_ty_list tenv tys true in
  TySum tys

let extend_name_env_with_ctor nenv ty_param ctors =
  ()                                          

let rec walk_prog env prog = match prog with Prog p -> walk_items env p

and update_env_with_type_def tenv = function
  | Enum { name; ty_param; ctors } -> tenv
  (* let { types } = tenv in
       Scope.insert types name
  *)
  | _ -> tenv

and walk_items env is =
  match is with
  | [] -> ()
  | item :: rest -> (
      match item with
      | Module { name; items } ->
          let new_env =
            create_module env (Option.value name ~default:"<empty>")
          in
          walk_items new_env items;
          walk_items env rest
      | Use path ->
          let new_env = update_use env (used_path path) in
          walk_items new_env rest
      | Func { name; ty_param; ty; tm_param; body } -> ()
      | TyDef tydef ->
          let new_env = update_env_with_type_def env tydef in
          walk_items new_env rest
      | Eff eff -> ()
      | _ -> ())

and type_check_expr env expr =
  match expr with
  | BinaryExpr (op, lhs, rhs) -> (
      let ty1 = type_check_expr env lhs in
      let ty2 = type_check_expr env rhs in
      match op with
      | Add | Sub | Mul | Div | Mod -> type_check_arithmetic_expr op ty1 ty2
      | LAnd | LOr -> type_check_logical_expr op ty1 ty2
      | Eq | Neq -> type_check_equality_expr op ty1 ty2
      | Le | Lt | Ge | Gt -> type_check_relational_expr op ty1 ty2)
  | UnaryExpr (op, expr) -> (
      let ty = type_check_expr env expr in
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
  | ArrayExpr elems -> type_check_array_expr env elems
  | CallExpr (f, args) -> TyUnit
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

and type_check_array_expr env elems =
  match elems with
  | [] -> TyHole
  | x :: xs ->
      let t1 = type_check_expr env x in
      let t2 = type_check_array_expr env xs in
      if type_eq env t1 t2 then t1
      else raise @@ Error "inconsistent types of the array elements"
