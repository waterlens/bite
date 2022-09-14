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
  mutable fn_ty : ty option;
}

let empty_context =
  {
    fn_ty_ctx = Scope.empty;
    eff_ty_ctx = Scope.empty;
    eff_op_to_eff_map = Scope.empty;
    binding_ty_ctx = Scope.empty;
    fn_ty = None;
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
      (fun tvar name ->
        match Hashtbl.find_opt gp_table name with
        | Some id -> ref @@ Generic id
        | None -> tvar)
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
                (fun tv name ->
                  match Hashtbl.find_opt gp_table name with
                  | Some id -> ref @@ Generic (id - n)
                  | None -> tv)
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
      (fun tv name ->
        match Hashtbl.find_opt gp_table name with
        | Some id -> ref @@ Generic id
        | None -> tv)
      init
  in
  List.fold_left
    (fun t1 var ->
      match var with
      | HandlerVar (_, t2) -> TyLabelLam (t2, t1)
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

let fn_type generic_param param ty_ann =
  let t2 = ty_annotation_to_ty ty_ann in
  let t1 = TyProd param in
  let t = TyArrow (t1, t2) in
  let depth = lambda_depth_of_ty t in
  let t = bind_complex_generic_parameters depth generic_param t in
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
  | Func { name; generic_param; param; ty_ann; _ } ->
      let _, param_ty = List.split param in
      let fn_type = fn_type generic_param param_ty ty_ann in
      Scope.insert ctx.fn_ty_ctx name fn_type;
      ()
  | _ -> raise Unreachable

and walk_tydef_phase_1 ctx = function
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

let fix_type_cross_ref ctx =
  let lookup_opt = lookup_in_type_ctx ctx in
  let fix_ctx ctx =
    List.iter
      (fun tbl ->
        Hashtbl.iter
          (fun _ ty ->
            let _ =
              Types.map_named_ty
                (fun tv name ->
                  match lookup_opt name with
                  | Some ty ->
                      tv := Link ty;
                      tv
                  | None -> tv)
                ty
            in
            ())
          tbl)
      ctx
  in
  fix_ctx ctx.fn_ty_ctx;
  fix_ctx ctx.eff_ty_ctx

let report_unresolved_type ty =
  let _ = map_named_ty (fun _ s -> raise @@ UnresolvedType s) ty in
  ()

let rec extend_ctx_with_handler_variables ctx ty hvar =
  let get_cur_hvar _ =
    match hvar with [] -> raise @@ Error "invalid arguments" | h :: _ -> h
  in
  match ty with
  | TyVar { contents = Link ty } ->
      extend_ctx_with_handler_variables ctx ty hvar
  | TyArrow _ -> ()
  | TyLam ty -> extend_ctx_with_handler_variables ctx ty hvar
  | TyLabelLam (t1, t2) ->
      let name = get_cur_hvar () in
      if Types.is_eff_op_ty t1 then (
        Scope.insert ctx.binding_ty_ctx name t1;
        extend_ctx_with_handler_variables ctx t2 (List.tl hvar))
      else raise @@ Error (name ^ "is not an effect operation type")
  | _ -> raise @@ Error "unexpected type"

let rec extend_ctx_with_parameters ctx ty params =
  let extend_ctx t name = Scope.insert ctx.binding_ty_ctx name t in
  match Types.skip_polymorphism ty with
  | TyArrow (TyProd params_ty, _) ->
      List.iter2 (fun ty name -> extend_ctx ty name) params_ty params
  | _ -> raise @@ Error "not an arrow type"

let flatten_eff_sum ty = match unlink ty with TySum tys -> tys | _ -> [ ty ]

let combine_param_eff tys =
  let effects = ref [] in
  let tys =
    List.map
      (fun ty ->
        match unlink ty with
        | TyEff (t, eff) ->
            effects := flatten_eff_sum eff;
            t
        | _ -> ty)
      tys
  in
  (TyProd tys, !effects)

let resolve_type f ctx =
  Types.map_named_ty (fun _ name ->
      match f ctx name with
      | Some ty -> ref @@ Link ty
      | None -> raise @@ UnresolvedType name)

let rec type_of_stmt ctx = function
  | Empty -> TyUnit
  | Ctl ctl -> type_of_ctl ctx ctl
  | Bind _ as bind -> type_of_bind ctx bind
  | Expr expr -> type_of_expr ctx expr

and type_of_ctl ctx = function
  | If (expr, t, f) -> raise NotImplemented
  | While (cond, body) -> raise NotImplemented
  | Resume expr -> TyHole
  | Try (stmts, eff_handlers) -> TyHole
  | Ret expr -> raise NotImplemented

and type_of_bind ctx = function
  | Bind { name; ty; init } ->
      let init_ty = type_of_expr ctx init in
      if
        unlink @@ resolve_type lookup_in_type_and_binding_ctx ctx ty
        == unlink init_ty
      then (
        Scope.insert ctx.binding_ty_ctx name init_ty;
        TyUnit)
      else
        raise
        @@ Error
             (name ^ "has inconsistent type of type annotaion and initializer")
  | _ -> raise Unreachable

and type_of_expr ctx = function
  | CallExpr (f, args) ->
      let fn_ty = type_of_expr ctx f in
      let args_ty = List.map (fun arg -> unlink @@ type_of_expr ctx arg) args in
      let arg, a_eff = combine_param_eff args_ty in
      let param, ret, f_eff =
        match unlink fn_ty with
        | TyArrow (a, r) -> (a, r, [])
        | TyEff (TyArrow (a, r), e) -> (a, r, flatten_eff_sum e)
        | _ -> raise @@ Error "not applicable type"
      in
      if arg == param then TyEff (ret, TySum (List.append a_eff f_eff))
      else raise @@ Error "not compatible arguments"
  | FieldExpr (e, field) ->
      let ty = type_of_expr ctx e in
      if Types.is_eff_op_ty ty then
        match field with
        | Named s -> (
            match Scope.lookup_opt ctx.eff_op_to_eff_map s with
            | Some s -> (
                match Scope.lookup_opt ctx.eff_ty_ctx s with
                | Some eff_ty ->
                    if eff_ty == ty then ty
                    else raise @@ Error (s ^ " has inconsistent effect operation")
                | _ -> raise Unreachable)
            | _ -> raise @@ Error ("unknown effect operation " ^ s))
        | Ordinal _ -> raise NotImplemented
      else raise NotImplemented
  | BinaryExpr (_, _, _) -> raise NotImplemented
  | UnaryExpr (_, _) -> raise NotImplemented
  | Literal l -> (
      l |> function
      | StringLiteral _ -> TyStr
      | FloatLiteral _ -> TyFloat
      | IntLiteral _ -> TyInt
      | BoolLiteral _ -> TyBool)
  | ArrayExpr _ -> raise NotImplemented
  | IndexExpr (_, _) -> raise NotImplemented
  | TupleExpr _ -> raise NotImplemented
  | BlockExpr blk ->
      entry_binding_context ctx;
      let ty = type_of_stmts ctx blk in
      leave_binding_context ctx;
      ty
  | VarExpr name -> (
      match lookup_in_type_and_binding_ctx ctx name with
      | Some ty -> ty
      | None ->
          raise @@ Error ("unable to find name " ^ name ^ " in current context")
      )
  | GenericExpr (e, tys) ->
      let ty = type_of_expr ctx e in
      let ty_arg =
        List.map
          (fun arg ->
            unlink @@ resolve_type lookup_in_type_and_binding_ctx ctx arg)
          tys
      in
      Types.apply ty ty_arg

and type_of_stmts ctx = function
  | [] -> TyUnit
  | [ x ] -> type_of_stmt ctx x
  | x :: xs ->
      let _ = type_of_stmt ctx x in
      type_of_stmts ctx xs

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
  | Func { name; generic_param; param; ty_ann; body } -> (
      match Scope.lookup_opt ctx.fn_ty_ctx name with
      | Some ty ->
          dbg @@ "check " ^ name;
          report_unresolved_type ty;
          dbg @@ "check " ^ name ^ " done";
          entry_binding_context ctx;
          let hvar_list =
            List.filter_map
              (function HandlerVar (name, _) -> Some name | _ -> None)
              generic_param
          in
          extend_ctx_with_handler_variables ctx ty hvar_list;
          entry_binding_context ctx;
          let param_name = List.map (fun (x, _) -> x) param in
          extend_ctx_with_parameters ctx ty param_name;
          ctx.fn_ty <- Some ty;
          let _ = type_of_stmts ctx body in
          ()
      | None -> raise Unreachable)
  | _ -> raise Unreachable

and walk_tydef_phase_2 ctx = function
  | Enum _ -> ()
  | Synonym _ -> ()
  | Record _ -> ()

and walk_eff_phase_2 ctx = function
  | Eff { name; op; _ } -> (
      match Scope.lookup_opt ctx.eff_ty_ctx name with
      | Some ty ->
          dbg @@ "check " ^ name;
          report_unresolved_type ty;
          dbg @@ "check " ^ name ^ " done"
      | None -> raise Unreachable)
  | _ -> raise Unreachable

let pipeline prog =
  let ctx = single_level_context in
  walk_prog_phase_1 ctx prog;
  show_context ctx;
  fix_type_cross_ref ctx;
  show_context ctx;
  walk_prog_phase_2 ctx prog
