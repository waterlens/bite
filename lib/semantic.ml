open Syntax
open Errors
open Scope
open Types

exception Unreachable
exception NotImplemented
exception UnresolvedType of string

type context = {
  mutable fn_ty_ctx : (string, ty) scope;
  mutable eff_ty_ctx : (string, ty) scope;
  mutable eff_op_to_eff_map : (string, string) scope;
  mutable binding_ty_ctx : (string, ty) scope;
  mutable cur_cont_ty : (unit, ty) scope;
}

let empty_context =
  {
    fn_ty_ctx = Scope.empty;
    eff_ty_ctx = Scope.empty;
    eff_op_to_eff_map = Scope.empty;
    binding_ty_ctx = Scope.empty;
    cur_cont_ty = Scope.empty;
  }

let dbg = Printf.printf "%s\n"

let show_context ctx =
  let print_tbl f1 f2 tbl =
    dbg "{";
    Hashtbl.iter
      (fun x y -> Printf.printf "        %s->\n%s\n" (f1 x) (f2 y))
      tbl;
    dbg "}"
  in
  let print_scope scope f1 f2 =
    dbg "    [";
    List.iter (fun tbl -> print_tbl f1 f2 tbl) scope;
    dbg "    ]"
  in
  let id x = x in
  let show_ty x = show_ty x in
  dbg "fn_ty_ctx:";
  print_scope ctx.fn_ty_ctx id show_ty;
  dbg "eff_ty_ctx:";
  print_scope ctx.eff_ty_ctx id show_ty;
  dbg "eff_op_to_eff_map:";
  print_scope ctx.eff_op_to_eff_map id id;
  dbg "binding_ty_ctx:";
  print_scope ctx.binding_ty_ctx id show_ty;
  ()

let entry_context ctx =
  ctx.fn_ty_ctx <- Scope.entry ctx.fn_ty_ctx;
  ctx.eff_ty_ctx <- Scope.entry ctx.eff_ty_ctx;
  ctx.eff_op_to_eff_map <- Scope.entry ctx.eff_op_to_eff_map;
  ctx.binding_ty_ctx <- Scope.entry ctx.binding_ty_ctx

let single_level_context =
  let ctx = empty_context in
  entry_context @@ empty_context;
  ctx

let entry_binding_context ctx =
  ctx.binding_ty_ctx <- Scope.entry ctx.binding_ty_ctx

let leave_binding_context ctx =
  ctx.binding_ty_ctx <- Scope.leave ctx.binding_ty_ctx

let scoped_binding_context ctx f =
  entry_binding_context ctx;
  f ();
  leave_binding_context ctx

let lookup_in_type_ctx ctx name =
  match Scope.lookup_opt ctx.fn_ty_ctx name with
  | Some _ as ty -> ty
  | None -> (
      match Scope.lookup_opt ctx.eff_ty_ctx name with
      | Some _ as ty -> ty
      | None -> None)

let lookup_in_type_and_binding_ctx ctx name =
  match Scope.lookup_opt ctx.fn_ty_ctx name with
  | Some _ as ty -> ty
  | None -> (
      match Scope.lookup_opt ctx.eff_ty_ctx name with
      | Some _ as ty -> ty
      | None -> (
          match Scope.lookup_opt ctx.binding_ty_ctx name with
          | Some _ as ty -> ty
          | None -> None))

let ty_annotation_to_ty = function t, None -> t | t, Some eff -> TyEff (t, eff)

let bind_simple_generic_parameters_inplace n gp init =
  let len = List.length gp in
  let tbl = Hashtbl.create len in
  let cnt = ref (n + len - 1) in
  let ins name =
    let id = !cnt in
    cnt := id - 1;
    Hashtbl.add tbl name id
  in
  List.iter ins gp;
  let elim tvar name =
    match Hashtbl.find_opt tbl name with
    | Some id ->
        tvar := Generic id;
        tvar
    | None -> tvar
  in
  let ty = Types.map_named_ty elim init in
  List.fold_left (fun t1 _ -> TyLam t1) ty @@ List.rev gp

let bind_complex_generic_parameters_inplace n gp init =
  let len = List.length gp in
  let tbl = Hashtbl.create len in
  let cnt = ref (n + len - 1) in
  let ins name =
    let id = !cnt in
    cnt := id - 1;
    Hashtbl.add tbl name id
  in
  let _, gp =
    List.fold_left_map
      (fun n -> function
        | HandlerVar (name, ty) ->
            let ty =
              Types.map_named_ty
                (fun tv name ->
                  match Hashtbl.find_opt tbl name with
                  | Some id ->
                      tv := Generic (id - n);
                      tv
                  | None -> tv)
                ty
            in
            ins name;
            (n + 1, HandlerVar (name, ty))
        | GenericVar name as ty ->
            ins name;
            (n + 1, ty))
      0 gp
  in
  let ty =
    Types.map_named_ty
      (fun tv name ->
        match Hashtbl.find_opt tbl name with
        | Some id ->
            tv := Generic id;
            tv
        | None -> tv)
      init
  in
  List.fold_left
    (fun t1 var ->
      match var with
      | HandlerVar (_, t2) -> TyLabelLam (t2, t1)
      | GenericVar _ -> TyLam t1)
    ty
  @@ List.rev gp

let eff_op_type { op_generic_param; op_param; op_ty_ann; _ } =
  let t2 = ty_annotation_to_ty op_ty_ann in
  let t1 = TyProd op_param in
  let t = TyArrow (t1, t2) in
  let dp = lambda_depth_of_ty t in
  let t =
    TyEffOp (bind_complex_generic_parameters_inplace dp op_generic_param t)
  in
  t

let eff_type generic_param t =
  let dp = lambda_depth_of_ty t in
  let t = bind_simple_generic_parameters_inplace dp generic_param t in
  t

let fn_type generic_param param ty_ann =
  let t2 = ty_annotation_to_ty ty_ann in
  let t1 = TyProd param in
  let t = TyArrow (t1, t2) in
  let dp = lambda_depth_of_ty t in
  let t = bind_complex_generic_parameters_inplace dp generic_param t in
  t

(* In the phase 1, we build the types*)
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
  | Func { name; generic_param; param; ty_ann; _ } ->
      let _, param_ty = List.split param in
      let fn_type = fn_type generic_param param_ty ty_ann in
      Scope.insert ctx.fn_ty_ctx name fn_type;
      ()
  | _ -> raise Unreachable

and walk_tydef_phase_1 _ = function
  | Enum _ -> ()
  | Synonym _ -> ()
  | Record _ -> ()

and walk_eff_phase_1 ctx = function
  | Eff { name; generic_param; op } ->
      let op_type = eff_op_type op in
      let eff_type = eff_type generic_param op_type in
      Scope.insert ctx.eff_ty_ctx name eff_type;
      Scope.insert ctx.eff_op_to_eff_map op.op_name name;
      ()
  | _ -> raise Unreachable

(* In the phase 2, we resolve all types in terms,
   especially those in the function body *)
let resolve_type_inplace f ty =
  let _ =
    Types.map_named_ty
      (fun tvar name ->
        match f name with
        | Some ty ->
            tvar := Link ty;
            tvar
        | None -> raise @@ UnresolvedType name)
      ty
  in
  ()

let resolve_generic_parameters_inplace gp ctx =
  let cnt = ref @@ (List.length gp - 1) in
  let f = lookup_in_type_and_binding_ctx ctx in
  let res = resolve_type_inplace f in
  let ins name =
    let id = !cnt in
    cnt := id - 1;
    Scope.insert ctx.binding_ty_ctx name @@ TyVar (ref @@ Generic id)
  in
  let bind = function
    | HandlerVar (name, ty) ->
        res ty;
        ins name
    | GenericVar name -> ins name
  in
  List.iter bind gp

let rec walk_prog_phase_2 ctx = function
  | Prog [] -> ()
  | Prog (x :: xs) ->
      walk_item_phase_2 ctx x;
      walk_prog_phase_2 ctx (Prog xs)

and walk_item_phase_2 ctx = function
  | Func _ as f -> walk_func_phase_2 ctx f
  | TyDef t -> walk_tydef_phase_2 ctx t
  | Eff _ as e -> walk_eff_phase_2 ctx e

and walk_func_phase_2 ctx = function
  | Func { name; generic_param; param; ty_ann; body } ->
      let scoped f = scoped_binding_context ctx f in
      let f = lookup_in_type_and_binding_ctx ctx in
      let res = resolve_type_inplace f in
      let ins name = Scope.insert ctx.binding_ty_ctx name in
      let bind (name, ty) =
        res ty;
        ins name ty
      in
      let rec res_stmt = function
        | Empty -> ()
        | Bind { ty; name; init } ->
            res_expr init;
            res ty;
            ins name ty
        | Ctl c -> res_ctl c
        | Expr e -> res_expr e
      and res_expr = function
        | BinaryExpr (_, e1, e2) ->
            res_expr e1;
            res_expr e2
        | UnaryExpr (_, e) -> res_expr e
        | Literal _ -> ()
        | ArrayExpr elem -> List.iter res_expr elem
        | CallExpr (f, args) ->
            res_expr f;
            List.iter res_expr args
        | FieldExpr (e, _) -> res_expr e
        | IndexExpr (e, idx) ->
            res_expr e;
            res_expr idx
        | TupleExpr elem -> List.iter res_expr elem
        | BlockExpr stmts -> scoped (fun _ -> List.iter res_stmt stmts)
        | VarExpr _ -> ()
        | GenericExpr (e, tys) ->
            res_expr e;
            List.iter res tys
      and res_ctl = function
        | Resume e -> res_expr e
        | If (cond, t, f) ->
            res_expr cond;
            res_expr @@ BlockExpr t;
            ignore @@ Option.map (fun f -> res_expr @@ BlockExpr f) f
        | While (cond, body) ->
            res_expr cond;
            res_expr @@ BlockExpr body
        | Try (body, h_eff) ->
            scoped (fun _ ->
                List.iter res_eff_handler h_eff;
                res_expr @@ BlockExpr body)
        | Ret e -> res_expr e
      and res_eff_handler
          {
            eff_name;
            eff_ty_ann;
            handler_ty_ann;
            handler_generic_param;
            handler_arg;
            handler_stmt;
            _;
          } =
        bind (eff_name, eff_ty_ann);
        (fun _ ->
          resolve_generic_parameters_inplace handler_generic_param ctx;
          let t, eff = handler_ty_ann in
          res t;
          let _ = Option.map (fun ty -> res ty) eff in
          (fun _ ->
            List.iter bind handler_arg;
            res_expr @@ BlockExpr handler_stmt)
          |> scoped)
        |> scoped
      in
      (* resolve the annotation of the return type*)
      (fun _ ->
        (* resolve the generic parameters *)
        resolve_generic_parameters_inplace generic_param ctx;
        (match ty_ann with
        | t1, Some t2 ->
            res t1;
            res t2
        | t1, None -> res t1);
        (fun _ ->
          (* bind parameters and resolve their types *)
          List.iter bind param;
          (fun (* resolve types in the function body *) _ ->
            List.iter res_stmt body)
          |> scoped)
        |> scoped)
      |> scoped
  | _ -> raise Unreachable

and walk_tydef_phase_2 _ = function
  | Enum _ -> ()
  | Synonym _ -> ()
  | Record _ -> ()

and walk_eff_phase_2 _ = function Eff _ -> () | _ -> raise Unreachable

let report_unresolved_type ty =
  let _ = map_named_ty (fun _ s -> raise @@ UnresolvedType s) ty in
  ()

let flatten_eff_sum ty = match real ty with TySum tys -> tys | _ -> [ ty ]

let combine_param_eff tys =
  let effects = ref [] in
  let tys =
    List.map
      (fun ty ->
        match real ty with
        | TyEff (t, eff) ->
            effects := flatten_eff_sum eff;
            t
        | _ -> ty)
      tys
  in
  (TyProd tys, !effects)

let check_eff_ty_and_op_matched ctx ty name =
  if Types.is_eff_op_ty ty then
    match Scope.lookup_opt ctx.eff_op_to_eff_map name with
    | Some s -> (
        match Scope.lookup_opt ctx.eff_ty_ctx s with
        | Some eff_ty ->
            if eff_ty = ty then ty
            else raise @@ Error (s ^ " has inconsistent effect operation")
        | _ -> raise Unreachable)
    | _ -> raise @@ Error ("unknown effect operation " ^ name)
  else raise @@ Error "not an effect operation"

let rec check_stmt ctx ty = function
  | Empty -> TyUnit
  | Ctl ctl -> check_ctl ctx ty ctl
  | Bind _ as bind -> check_bind ctx ty bind
  | Expr expr -> check_expr ctx ty expr

and infer_stmt ctx = check_stmt ctx TyHole

and check_ctl ctx _ = function
  | If (cond, t, f) ->
      let ct = infer_expr ctx cond in
      if ct <> TyBool then raise @@ Error "condition must have a bool type"
      else ignore @@ infer_stmts ctx t;
      (match f with Some s -> ignore @@ infer_stmts ctx s | _ -> ());
      TyUnit
  | While (cond, body) ->
      let ct = infer_expr ctx cond in
      if ct <> TyBool then raise @@ Error "condition must have a bool type"
      else ignore @@ infer_stmts ctx body;
      TyUnit
  | Resume expr -> TyHole
  | Try (stmts, eff_handlers) ->
      let scoped = scoped_binding_context ctx in
      let ins name = Scope.insert ctx.binding_ty_ctx name in
      let bind_gen =
        List.iter (function HandlerVar (name, ty) -> ins name ty | _ -> ())
      in
      let bind_param = List.iter (fun (name, ty) -> ins name ty) in
      let ins_handler
          {
            eff_name;
            eff_ty_ann;
            handler_name;
            handler_ty_ann;
            handler_generic_param;
            handler_arg;
            handler_stmt;
          } =
        let eff_ty = real eff_ty_ann in
        ignore @@ check_eff_ty_and_op_matched ctx eff_ty handler_name;
        ins eff_name eff_ty;
        (fun _ ->
          bind_gen handler_generic_param;
          (fun _ -> bind_param handler_arg) |> scoped)
        |> scoped
      in
      entry_binding_context ctx;
      leave_binding_context ctx;
      TyHole

and infer_ctl ctx = check_ctl ctx TyHole

and check_bind ctx ty =
  if ty <> TyUnit then
    raise @@ Error "a bind statement can't return a non-unit value"
  else function
    | Bind { name; ty; init } ->
        let init_ty = check_expr ctx ty init in
        if ty = init_ty then (
          Scope.insert ctx.binding_ty_ctx name init_ty;
          TyUnit)
        else
          raise
          @@ Error
               (name ^ "has inconsistent type of type annotaion and initializer")
    | _ -> raise Unreachable

and infer_bind ctx = check_bind ctx TyHole

and check_stmts ctx ty = function
  | [] -> TyUnit
  | [ x ] -> check_stmt ctx ty x
  | x :: xs ->
      let _ = infer_stmt ctx x in
      check_stmts ctx ty xs

and infer_stmts ctx = check_stmts ctx TyHole

and check_expr ctx ty = function
  | CallExpr (f, args) ->
      let fn_ty = infer_expr ctx f in
      let args_ty = List.map (fun arg -> infer_expr ctx arg) args in
      let arg, arg_eff = combine_param_eff args_ty in
      let param, ret, f_eff =
        match fn_ty with
        | TyArrow (a, r) -> (a, r, [])
        | TyEff (TyArrow (a, r), e) -> (a, r, flatten_eff_sum e)
        | _ -> raise @@ Error "not applicable type"
      in
      if arg = param then
        if ty <> TyHole && ty <> ret then
          raise @@ Error "unexpected return value"
        else TyEff (ret, TySum (List.append arg_eff f_eff))
      else raise @@ Error "not compatible arguments"
  | FieldExpr (e, field) ->
      let ty = infer_expr ctx e in
      if Types.is_eff_op_ty ty then
        match field with
        | Named s -> check_eff_ty_and_op_matched ctx ty s
        | Ordinal _ -> raise NotImplemented (* destruct tuple *)
      else raise NotImplemented (* get record field *)
  | Literal l -> (
      l |> function
      | StringLiteral _ -> TyStr
      | FloatLiteral _ -> TyFloat
      | IntLiteral _ -> TyInt
      | BoolLiteral _ -> TyBool)
  | BlockExpr blk ->
      entry_binding_context ctx;
      let ty = infer_stmts ctx blk in
      leave_binding_context ctx;
      ty
  | VarExpr name -> (
      match lookup_in_type_and_binding_ctx ctx name with
      | Some ty -> ty
      | None ->
          raise @@ Error ("unable to find name " ^ name ^ " in current context")
      )
  | GenericExpr (e, tys) ->
      let ty = infer_expr ctx e in
      Types.apply ty tys
  | BinaryExpr (_, _, _) -> raise NotImplemented
  | UnaryExpr (_, _) -> raise NotImplemented
  | ArrayExpr _ -> raise NotImplemented
  | IndexExpr (_, _) -> raise NotImplemented
  | TupleExpr _ -> raise NotImplemented

and infer_expr ctx = check_expr ctx TyHole

let rec walk_prog_phase_3 ctx = function
  | Prog [] -> ()
  | Prog (x :: xs) ->
      walk_item_phase_2 ctx x;
      walk_prog_phase_2 ctx (Prog xs)

and walk_item_phase_3 ctx = function
  | Func _ as f -> walk_func_phase_2 ctx f
  | TyDef t -> walk_tydef_phase_2 ctx t
  | Eff _ as e -> walk_eff_phase_2 ctx e

and walk_func_phase_3 ctx = function
  | Func { name; generic_param; param; ty_ann; body } ->
      let scoped = scoped_binding_context ctx in
      let ins name = Scope.insert ctx.binding_ty_ctx name in
      let ft = Scope.lookup ctx.fn_ty_ctx name in
      let rt = ty_annotation_to_ty ty_ann in
      let bind_gen =
        List.iter (function HandlerVar (name, ty) -> ins name ty | _ -> ())
      in
      let bind_param = List.iter (fun (name, ty) -> ins name ty) in
      (fun _ ->
        (* allow recursive function *)
        ins name ft;
        (fun _ ->
          bind_gen generic_param;
          (fun _ ->
            bind_param param;
            (fun _ ->
              let _ = check_stmts ctx rt body in
              ())
            |> scoped)
          |> scoped)
        |> scoped)
      |> scoped
  | _ -> raise Unreachable

and walk_tydef_phase_3 ctx = function
  | Enum _ -> ()
  | Synonym _ -> ()
  | Record _ -> ()

and walk_eff_phase_3 ctx = function Eff _ -> () | _ -> raise Unreachable

let fix_type_cross_ref ctx =
  let lookup_opt = lookup_in_type_ctx ctx in
  let mk_cross_ref tv name =
    match lookup_opt name with
    | Some ty ->
        tv := Link ty;
        tv
    | None -> tv
  in
  let fix_tbl_item _ ty =
    let _ = Types.map_named_ty mk_cross_ref ty in
    ()
  in
  let fix_single tbl = Hashtbl.iter fix_tbl_item tbl in
  let fix_ctx ctx = List.iter fix_single ctx in
  fix_ctx ctx.fn_ty_ctx;
  fix_ctx ctx.eff_ty_ctx

let pipeline prog =
  let ctx = single_level_context in
  walk_prog_phase_1 ctx prog;
  fix_type_cross_ref ctx;
  walk_prog_phase_2 ctx prog;
  show_context ctx;
  Printf.printf "%s\n" @@ show_prog prog
