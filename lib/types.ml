type ty =
  | TyUnit
  | TyInt
  | TyFloat
  | TyBool
  | TyStr
  | TyHole
  | TyApp of ty * ty list
  | TyProd of ty list
  | TySum of ty list
  | TyEnum of (string * ty) list (* labeled *)
  | TyArrow of ty * ty
  | TyArray of ty
  | TyVar of tvar ref
  | TyLam of ty
  | TyLabelLam of ty * ty
  | TyCont of ty * ty
  | TyEff of ty * ty
  | TyEffOp of ty

and tvar =
  | Generic of int
  | Link of ty [@printer fun fmt _ -> fprintf fmt "<ty>"]
  | Named of string
[@@deriving show]

let rec real = function TyVar { contents = Link ty } -> ty | _ as ty -> ty

let lambda_depth_of_ty, lambda_depth_of_ty_list =
  let rec lambda_depth_of_ty_tail acc =
    let ld = lambda_depth_of_ty_tail in
    let lds = lambda_depth_of_ty_list_tail in
    function
    | TyApp (t, ts) -> max (ld acc t) (lds acc ts)
    | TyProd ts -> lds acc ts
    | TySum ts -> lds acc ts
    | TyEnum nts ->
        List.fold_left (fun acc (_, ty) -> max acc (ld acc ty)) 0 nts
    | TyArrow (t1, t2) -> max (ld acc t1) (ld acc t2)
    | TyArray t -> ld acc t
    | TyLam ty -> ld (acc + 1) ty
    | TyLabelLam (t1, t2) -> max (ld (acc + 1) t1) (ld (acc + 1) t2)
    | TyCont (t1, t2) -> max (ld acc t1) (ld acc t2)
    | TyEff (t1, t2) -> max (ld acc t1) (ld acc t2)
    | TyEffOp t -> ld acc t
    | TyUnit | TyInt | TyFloat | TyBool | TyStr | TyHole | TyVar _ -> acc
  and lambda_depth_of_ty_list_tail acc tys =
    List.fold_left (fun a b -> max a (lambda_depth_of_ty_tail acc b)) 0 tys
  in
  (lambda_depth_of_ty_tail 0, lambda_depth_of_ty_list_tail 0)

let rec map_ty_var f =
  let f1 ty = map_ty_var f ty in
  let f2 tys = map_ty_var_list f tys in
  function
  | TyApp (t, ts) -> TyApp (f1 t, f2 ts)
  | TyProd ts -> TyProd (f2 ts)
  | TySum ts -> TySum (f2 ts)
  | TyEnum nts -> TyEnum (List.map (fun (x, y) -> (x, f1 y)) nts)
  | TyArrow (t1, t2) -> TyArrow (f1 t1, f1 t2)
  | TyArray t -> TyArray (f1 t)
  | TyLam ty -> TyLam (f1 ty)
  | TyLabelLam (t1, t2) -> TyLabelLam (f1 t1, f1 t2)
  | TyCont (t1, t2) -> TyCont (f1 t1, f1 t2)
  | TyEff (t1, t2) -> TyEff (f1 t1, f1 t2)
  | TyEffOp t -> TyEffOp (f1 t)
  | TyVar v -> TyVar (f v)
  | (TyUnit | TyInt | TyFloat | TyBool | TyStr | TyHole) as t -> t

and map_ty_var_list f =
  let rec map_ty_var_list_tail acc = function
    | [] -> List.rev acc
    | x :: xs ->
        let x = map_ty_var f x in
        map_ty_var_list_tail (x :: acc) xs
  in
  map_ty_var_list_tail []

let map_named_ty f =
  map_ty_var (function
    | { contents = Named s } as tvar -> f tvar s
    | _ as tvar -> tvar)

let shift n =
  map_ty_var (function
    | { contents = Generic id } -> ref @@ Generic (id + n)
    | _ as t -> t)

let subst_n n t1 t2 =
  map_ty_var
    (function
      | { contents = Generic id } as t ->
          if id == n - 1 then ref @@ Link t2 else t
      | _ as t -> t)
    t1

exception NotApplicable
exception LabelIsNotEffOp

let apply, is_eff_op_ty =
  let rec apply_n n t1 t2 =
    match t1 with
    | TyVar { contents = Link ty } -> apply_n n ty t2
    | TyLam ty -> subst_n n ty t2
    | TyLabelLam (et, ty) ->
        if is_eff_op_ty et then subst_n n ty t2 else raise LabelIsNotEffOp
    | _ -> raise NotApplicable
  and apply t ts =
    let n = ref @@ lambda_depth_of_ty t in
    List.fold_left
      (fun f arg ->
        let id = !n in
        n := id - 1;
        apply_n id f arg)
      t ts
  and is_eff_op_ty = function
    | TyApp (t, ts) -> is_eff_op_ty @@ apply t ts
    | TyVar { contents = Link ty } -> is_eff_op_ty ty
    | TyEffOp _ -> true
    | _ -> false
  in
  (apply, is_eff_op_ty)

let rec skip_polymorphism = function
  | TyLabelLam (_, t) -> skip_polymorphism t
  | TyLam t -> skip_polymorphism t
  | TyVar { contents = Link t } -> skip_polymorphism t
  | _ as t -> t
