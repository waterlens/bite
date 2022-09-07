type ty =
  | TyUnit
  | TyInt
  | TyFloat
  | TyBool
  | TyStr
  | TyHole
  | TyIdent of string
  | TyApp of ty * ty list
  | TyProd of ty list
  | TySum of ty list
  | TyEnum of (string * ty) list (* labeled *)
  | TyArrow of ty * ty
  | TyArray of ty
  | TyVar of tvar ref
  | TyLam of ty
  | TyLamWithConstr of ty * ty
  | TyCont of ty * ty
  | TyEff of ty * ty
  | TyEffOp of ty

and tvar = Generic of int | Link of ty | Unbound of int [@@deriving show]

let rec unlink = function
  | TyVar ({ contents = Link ty } as tvar) ->
      let ty = unlink ty in
      tvar := Link ty;
      ty
  | _ as ty -> ty

let rec has_named_ty = function
  | TyIdent _ -> true
  | TyApp (t, ts) -> has_named_ty t || has_named_in_list ts
  | TyProd ts -> has_named_in_list ts
  | TySum ts -> has_named_in_list ts
  | TyEnum nts ->
      List.fold_left (fun acc (_, ty) -> acc || has_named_ty ty) false nts
  | TyArrow (t1, t2) -> has_named_ty t1 || has_named_ty t2
  | TyArray t -> has_named_ty t
  | TyLam ty -> has_named_ty ty
  | TyLamWithConstr (t1, t2) -> has_named_ty t1 || has_named_ty t2
  | TyCont (t1, t2) -> has_named_ty t1 || has_named_ty t2
  | TyEff (t1, t2) -> has_named_ty t1 || has_named_ty t2
  | TyEffOp t -> has_named_ty t
  | TyUnit | TyInt | TyFloat | TyBool | TyStr | TyHole | TyVar _ -> false

and has_named_in_list = function
  | [] -> false
  | x :: xs -> if has_named_ty x then true else has_named_in_list xs

let rec map_named_ty f =
  let f1 ty = map_named_ty f ty in
  let f2 tys = map_named_ty_list f tys in
  function
  | TyIdent s -> f s
  | TyApp (t, ts) -> TyApp (f1 t, f2 ts)
  | TyProd ts -> TyProd (f2 ts)
  | TySum ts -> TySum (f2 ts)
  | TyEnum nts -> TyEnum (List.map (fun (x, y) -> (x, f1 y)) nts)
  | TyArrow (t1, t2) -> TyArrow (f1 t1, f1 t2)
  | TyArray t -> TyArray (f1 t)
  | TyLam ty -> TyLam (f1 ty)
  | TyLamWithConstr (t1, t2) -> TyLamWithConstr (f1 t1, f1 t2)
  | TyCont (t1, t2) -> TyCont (f1 t1, f1 t2)
  | TyEff (t1, t2) -> TyEff (f1 t1, f1 t2)
  | TyEffOp t -> TyEffOp (f1 t)
  | (TyUnit | TyInt | TyFloat | TyBool | TyStr | TyHole | TyVar _) as t -> t

and map_named_ty_list f =
  let rec subst_named_ty_list acc = function
    | [] -> List.rev acc
    | x :: xs ->
        let x = map_named_ty f x in
        subst_named_ty_list (x :: acc) xs
  in
  subst_named_ty_list []

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
    | TyLamWithConstr (t1, t2) -> max (ld (acc + 1) t1) (ld (acc + 1) t2)
    | TyCont (t1, t2) -> max (ld acc t1) (ld acc t2)
    | TyEff (t1, t2) -> max (ld acc t1) (ld acc t2)
    | TyEffOp t -> ld acc t
    | TyUnit | TyInt | TyFloat | TyBool | TyStr | TyHole | TyVar _ | TyIdent _
      ->
        acc
  and lambda_depth_of_ty_list_tail acc tys =
    List.fold_left (fun a b -> max a (lambda_depth_of_ty_tail acc b)) 0 tys
  in
  (lambda_depth_of_ty_tail 0, lambda_depth_of_ty_list_tail 0)
