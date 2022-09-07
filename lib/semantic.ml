open Syntax
open Errors
open Scope
open Types

type context = {
  mutable ty_ctx : (string, ty) scope;
  mutable fn_ctx : unit;
  mutable eff_ctx : (string, ty) scope;
  mutable eff_op_ctx : (string, ty) scope;
  mutable eff_to_eff_op_map : (string, string) scope;
}

exception Unreachable

let ty_annotation_to_ty = function t, None -> t | t, Some eff -> TyEff (t, eff)

let bind_simple_generic_parameters n gp init =
  let len = n + List.length gp in
  let gp_table = Hashtbl.create len in
  let cnt = ref (len - 1) in
  let add_to_table name =
    let id = !cnt in
    cnt := id - 1;
    Hashtbl.add gp_table name id
  in
  List.iter (fun name -> add_to_table name) gp;
  let kernel_ty =
    Types.map_named_ty
      (fun name ->
        match Hashtbl.find_opt gp_table name with
        | Some id -> TyVar (ref @@ Generic id)
        | None -> TyIdent name)
      init
  in
  List.fold_left (fun t1 _ -> TyLam t1) kernel_ty @@ List.rev gp

let bind_complex_generic_parameters n gp init =
  let len = n + List.length gp in
  let gp_table = Hashtbl.create len in
  let cnt = ref (len - 1) in
  let add_to_table name =
    let id = !cnt in
    cnt := id - 1;
    Hashtbl.add gp_table name id
  in
  let _, gp =
    List.fold_left_map
      (fun n -> function
        | HandlerVar (name, ty) ->
            let ty =
              Types.map_named_ty
                (fun name ->
                  match Hashtbl.find_opt gp_table name with
                  | Some id -> TyVar (ref @@ Generic (id - n))
                  | None -> TyIdent name)
                ty
            in
            add_to_table name;
            (n + 1, HandlerVar (name, ty))
        | GenericVar name as ty ->
            add_to_table name;
            (n + 1, ty))
      0 gp
  in
  let kernel_ty =
    Types.map_named_ty
      (fun name ->
        match Hashtbl.find_opt gp_table name with
        | Some id -> TyVar (ref @@ Generic id)
        | None -> TyIdent name)
      init
  in
  List.fold_left
    (fun t1 var ->
      match var with
      | HandlerVar (_, t2) -> TyLamWithConstr (t2, t1)
      | GenericVar _ -> TyLam t1)
    kernel_ty
  @@ List.rev gp

let eff_op_type op =
  let { op_generic_param; op_param; op_ty_ann; _ } = op in
  let t2 = ty_annotation_to_ty op_ty_ann in
  let t1 = TyProd op_param in
  let t = TyArrow (t1, t2) in
  let depth = lambda_depth_of_ty t in
  let t = TyEffOp (bind_complex_generic_parameters depth op_generic_param t) in
  t

let eff_type generic_param t =
  let depth = lambda_depth_of_ty t in
  let t = bind_simple_generic_parameters depth generic_param t in
  t

let rec walk_prog_phase_1 ctx = function
  | Prog [] -> ()
  | Prog (x :: xs) ->
      walk_item_phase_1 ctx x;
      walk_prog_phase_1 ctx (Prog xs)

and walk_item_phase_1 ctx = function
  | Func _ as f -> walk_func_phase_1 ctx f
  | TyDef t -> walk_tydef_phase_1 ctx t
  | Eff _ as e -> walk_eff_phase_1 ctx e

and walk_func_phase_1 ctx = function
  | Func { name; generic_param; param; ty_ann; body } -> ()
  | _ -> raise Unreachable

and walk_tydef_phase_1 ctx = function
  | Enum _ -> ()
  | Synonym _ -> ()
  | Record _ -> ()

and walk_eff_phase_1 ctx = function
  | Eff { name; generic_param; op } ->
      let op_type = eff_op_type op in
      let eff_type = eff_type generic_param op_type in
      Scope.insert ctx.eff_ctx name eff_type;
      Scope.insert ctx.eff_op_ctx op.op_name op_type;
      ()
  | _ -> raise Unreachable
