(* Control-flow IR *)
type cir = { functions : cir_func list }

and cir_func = {
  fname : string;
  mutable blocks : cir_block list;
  mutable hvars : cir_variable list;
  mutable params : cir_variable list;
  mutable locals : cir_variable list;
  mutable rvar : cir_variable option;
  mutable entry_block : cir_block option;
  mutable ret_block : cir_block option;
}

and cir_block = {
  bid : int;
  bdesc : string;
  mutable stmts : (cir_variable option * cir_value) list;
  mutable terminator : cir_terminator;
}

and cir_insn =
  | Call of cir_func * cir_value list
  | BinaryOp of Syntax.binary_op * cir_value * cir_value
  | UnaryOp of Syntax.unary_op * cir_value
  | MakeTuple of cir_value list
  | MakeTaggedTuple of int * cir_value list
  | MakeArray of cir_value list
  | ExtractTupleField of cir_value * int
  | ExtractArrayElement of cir_value * cir_value
  | Intrinsic of string * cir_value list

and cir_terminator =
  | Branch of cir_value * cir_block * cir_block
  | Return
  | Jump of cir_block
  | Panic

and cir_var_kind = Parameter | LocalVar | TempVar | RetVar
and cir_variable = { vid : int; vkind : cir_var_kind; vty : cir_type }
and cir_type = Types.ty

and cir_value =
  [ `Imm of int
  | `ILit of int
  | `BLit of bool
  | `SLit of string
  | `FLit of float
  | `Unit
  | `Var of cir_variable
  | `Insn of cir_insn ]

type cir_building_ctx = {
  mutable fn_ctx : (string, cir_func) Scope.scope;
  mutable ty_ctx : Semantic.context;
  mutable var_ctx : (string, cir_variable) Scope.scope;
  mutable pos : cir_block;
  mutable var_id : int ref;
  mutable block_id : int ref;
  mutable func : cir_func option;
}

let entry_scope ctx = ctx.var_ctx <- Scope.entry ctx.var_ctx
let leave_scope ctx = ctx.var_ctx <- Scope.leave ctx.var_ctx

let next_id id =
  let n = !id in
  id := n + 1;
  n

let make_variable id kind ty =
  let vid = next_id id in
  let vkind = kind in
  let vty = ty in
  { vid; vkind; vty }

let make_local_variable_in_func ctx kind ty =
  let var = make_variable ctx.var_id kind ty in
  let func = Option.get ctx.func in
  (ignore
  @@
  match kind with
  | LocalVar -> func.locals <- var :: func.locals
  | TempVar -> func.locals <- var :: func.locals
  | Parameter -> ()
  | RetVar -> ());
  var

let append_stmt ctx var value = ctx.pos.stmts <- (var, value) :: ctx.pos.stmts
let change_current_terminator ctx term = ctx.pos.terminator <- term

let link_between blk1 blk2 =
  let old = blk1.terminator in
  blk1.terminator <- Jump blk2;
  old

let branch_among cvar blk1 blk2 blk3 =
  let old = blk1.terminator in
  blk1.terminator <- Branch (cvar, blk2, blk3);
  old

let move_to ctx blk = ctx.pos <- blk

exception NotImplemented

let rec build_expr ctx =
  let build_expr_with_cur_ctx x = build_expr ctx x in
  function
  | Syntax.BinaryExpr (op, e1, e2) ->
      `Insn (BinaryOp (op, build_expr ctx e1, build_expr ctx e2))
  | Syntax.UnaryExpr (op, e) -> `Insn (UnaryOp (op, build_expr ctx e))
  | Syntax.Literal (Syntax.IntLiteral x) -> `ILit x
  | Syntax.Literal (Syntax.BoolLiteral x) -> `BLit x
  | Syntax.Literal (Syntax.StringLiteral x) -> `SLit x
  | Syntax.Literal (Syntax.FloatLiteral x) -> `FLit x
  | Syntax.ArrayExpr elems ->
      `Insn (MakeArray (List.map build_expr_with_cur_ctx elems))
  | Syntax.CallExpr (VarExpr name, args) ->
      let func = Scope.lookup ctx.fn_ctx name in
      `Insn (Call (func, List.map build_expr_with_cur_ctx args))
  | Syntax.CallExpr (GenericExpr (e, tys), args) -> raise NotImplemented
  | Syntax.CallExpr (e, args) -> raise NotImplemented
  | Syntax.FieldExpr (e, field) -> raise NotImplemented
  | Syntax.IndexExpr (e1, e2) ->
      `Insn (ExtractArrayElement (build_expr ctx e1, build_expr ctx e2))
  | Syntax.TupleExpr fields ->
      `Insn (MakeTuple (List.map build_expr_with_cur_ctx fields))
  | BlockExpr stmts -> build_block_expr ctx stmts
  | VarExpr name -> `Var (Scope.lookup ctx.var_ctx name)
  | GenericExpr (e, tys) -> raise NotImplemented

and build_block_expr ctx =
  ignore @@ entry_scope ctx;
  function
  | [] ->
      ignore @@ leave_scope ctx;
      `Unit
  | [ x ] ->
      let var = make_local_variable_in_func ctx TempVar TyHole in
      build_stmt_with_output_var ctx (Some var) x;
      `Var var
  | x :: xs ->
      build_stmt ctx x;
      build_block_expr ctx xs

and build_ctl ctx =
  let make_block terminator desc =
    let bid = next_id ctx.block_id in
    let blk = { bid; stmts = []; terminator; bdesc = desc } in
    (Option.get ctx.func).blocks <- blk :: (Option.get ctx.func).blocks;
    blk
  in
  function
  | Syntax.If (cond, t, f) ->
      let cond_block = make_block Panic "if_cond" in
      let then_block = make_block Panic "if_then" in
      let else_block = Option.map (fun _ -> make_block Panic "if_else") f in
      let end_block = make_block Panic "if_end" in
      let old_last = link_between ctx.pos cond_block in

      move_to ctx cond_block;
      let cond_var = make_local_variable_in_func ctx TempVar TyBool in
      let cond_val = build_expr ctx cond in
      append_stmt ctx (Some cond_var) cond_val;
      ignore
      @@ branch_among (`Var cond_var) ctx.pos then_block
           (Option.value else_block ~default:end_block);

      move_to ctx then_block;
      append_stmt ctx None @@ build_expr ctx @@ Syntax.BlockExpr t;
      if Option.is_some else_block then (
        move_to ctx (Option.get else_block);
        append_stmt ctx None @@ build_expr ctx
        @@ Syntax.BlockExpr (Option.get f);
        ignore @@ link_between ctx.pos end_block);
      ignore @@ link_between ctx.pos end_block;

      move_to ctx end_block;
      change_current_terminator ctx old_last
  | Syntax.While (cond, body) ->
      let cond_block = make_block Panic "while_cond" in
      let end_block = make_block Panic "while_end" in
      let body_block = make_block Panic "while_body" in
      let old_last = link_between ctx.pos cond_block in

      move_to ctx cond_block;
      let cond_var = make_local_variable_in_func ctx TempVar TyBool in
      let cond_val = build_expr ctx cond in
      append_stmt ctx (Some cond_var) cond_val;
      ignore @@ branch_among (`Var cond_var) ctx.pos body_block end_block;

      move_to ctx body_block;
      append_stmt ctx None @@ build_expr ctx @@ Syntax.BlockExpr body;
      ignore @@ link_between ctx.pos cond_block;

      move_to ctx end_block;
      change_current_terminator ctx old_last
  | Syntax.Try _ -> ()
  | Syntax.Resume _ -> ()
  | Syntax.Ret e ->
      let ret_blk = Option.get (Option.get ctx.func).ret_block in
      let value = build_expr ctx e in
      let func = Option.get ctx.func in
      append_stmt ctx (Some (Option.get func.rvar)) value;
      ignore @@ link_between ctx.pos ret_blk;
      let dummy_block = make_block Panic "dummy" in
      move_to ctx dummy_block

and build_stmt_with_output_var ctx out = function
  | Syntax.Ctl ctl -> (
      build_ctl ctx ctl;
      match out with Some var -> append_stmt ctx (Some var) `Unit | None -> ())
  | Syntax.Bind { name; init; ty } ->
      let var = make_local_variable_in_func ctx LocalVar ty in
      let value = build_expr ctx init in
      append_stmt ctx (Some var) value;
      Scope.insert ctx.var_ctx name var
  | Syntax.Expr expr -> append_stmt ctx None @@ build_expr ctx expr
  | Syntax.Empty -> ()

and build_stmt ctx = build_stmt_with_output_var ctx None

let build_func_body ctx body = build_expr ctx @@ BlockExpr body

let make_initial_blocks id =
  let id1 = next_id id in
  let id2 = next_id id in
  let return =
    { bid = id2; stmts = []; terminator = Return; bdesc = "return" }
  in
  let entry =
    { bid = id1; stmts = []; terminator = Jump return; bdesc = "entry" }
  in
  (entry, return)

let make_cir_func_proto name =
  {
    fname = name;
    blocks = [];
    hvars = [];
    params = [];
    locals = [];
    rvar = None;
    entry_block = None;
    ret_block = None;
  }

let build_cir_func ctx name generic_param param ty_ann body =
  ignore @@ entry_scope ctx;
  let proto = Scope.lookup ctx.fn_ctx name in
  let var_id = ref 0 in
  let blk_id = ref 0 in
  ctx.var_id <- var_id;
  ctx.block_id <- blk_id;
  let rty, _ = ty_ann in
  let rvar = make_variable var_id RetVar rty in
  let hvars =
    List.filter_map
      (function
        | Syntax.HandlerVar (name, ty) ->
            let hvar = make_variable var_id Parameter ty in
            Scope.insert ctx.var_ctx name hvar;
            Some hvar
        | GenericVar _ -> None)
      generic_param
  in
  ignore @@ entry_scope ctx;
  let params =
    List.map
      (fun (name, ty) ->
        let param = make_variable var_id Parameter ty in
        Scope.insert ctx.var_ctx name param;
        param)
      param
  in
  let entry, return = make_initial_blocks blk_id in
  proto.blocks <- [ return; entry ];
  proto.hvars <- hvars;
  proto.params <- params;
  proto.entry_block <- Some entry;
  proto.ret_block <- Some return;
  proto.rvar <- Some rvar;
  ctx.pos <- entry;
  ctx.func <- Some proto;
  let v = build_func_body ctx body in
  move_to ctx return;
  append_stmt ctx None v;
  ignore @@ leave_scope ctx;
  ignore @@ leave_scope ctx

let make_cir ty_ctx prog =
  let prog = match prog with Syntax.Prog p -> p in
  let ctx =
    {
      fn_ctx = Scope.entry Scope.empty;
      ty_ctx;
      var_ctx = Scope.empty;
      pos = { bid = 0; stmts = []; terminator = Panic; bdesc = "placeholder" };
      var_id = ref 0;
      block_id = ref 0;
      func = None;
    }
  in
  List.iter
    (function
      | Syntax.Func { name; _ } ->
          Scope.insert ctx.fn_ctx name @@ make_cir_func_proto name
      | _ -> ())
    prog;
  List.iter
    (function
      | Syntax.Func { name; generic_param; param; ty_ann; body } ->
          ignore @@ build_cir_func ctx name generic_param param ty_ann body
      | _ -> ())
    prog;
  { functions = List.of_seq @@ Hashtbl.to_seq_values @@ List.hd ctx.fn_ctx }

let show_cir_variable { vid; vkind; _ } =
  match vkind with
  | LocalVar -> Printf.sprintf "%%%d" vid
  | Parameter -> Printf.sprintf "#%d" vid
  | TempVar -> Printf.sprintf "%%%d" vid
  | RetVar -> Printf.sprintf "^%d" vid

let show_cir_block_label block = Printf.sprintf "B%d" block.bid

let show_cir_binary_op = function
  | Syntax.Add -> "add"
  | Sub -> "sub"
  | Mul -> "mul"
  | Div -> "div"
  | Mod -> "mod"
  | Eq -> "eq"
  | Neq -> "neq"
  | Gt -> "gt"
  | Ge -> "ge"
  | Lt -> "lt"
  | Le -> "le"
  | LAnd -> "logical_and"
  | LOr -> "logical_or"

let show_cir_unary_op = function
  | Syntax.LNot -> "logical_not"
  | Plus -> "pos"
  | Minus -> "neg"

let rec show_cir_value = function
  | `Imm n -> string_of_int n
  | `ILit n -> string_of_int n ^ "i"
  | `FLit n -> string_of_float n ^ "f"
  | `BLit n -> string_of_bool n
  | `SLit _ -> "<str>"
  | `Unit -> "()"
  | `Var v -> show_cir_variable v
  | `Insn insn -> show_cir_insn insn

and show_cir_terminator = function
  | Branch (cond, b1, b2) ->
      Printf.sprintf "br %s %s %s" (show_cir_value cond)
        (show_cir_block_label b1) (show_cir_block_label b2)
  | Jump b -> "jump " ^ show_cir_block_label b
  | Return -> "ret"
  | Panic -> "panic"

and show_cir_insn = function
  | Call (f, args) ->
      Printf.sprintf "%s (%s)" f.fname
      @@ String.concat ","
      @@ List.map show_cir_value args
  | BinaryOp (op, v1, v2) ->
      Printf.sprintf "%s (%s, %s)" (show_cir_binary_op op) (show_cir_value v1)
        (show_cir_value v2)
  | UnaryOp (op, v) ->
      Printf.sprintf "%s (%s)" (show_cir_unary_op op) (show_cir_value v)
  | MakeTuple elems -> "make_tuple"
  | MakeTaggedTuple (tag, elems) -> "make_tagged_tuple"
  | MakeArray elems -> "make_array"
  | ExtractTupleField (v, nth) -> "extract_field"
  | ExtractArrayElement (v, nth) -> "extract_element"
  | Intrinsic (name, args) ->
      Printf.sprintf "!%s (%s)" name
      @@ String.concat ","
      @@ List.map show_cir_value args

let show_cir_stmt (target, value) =
  let var = match target with Some var -> show_cir_variable var | _ -> "_" in
  Printf.sprintf "%s = %s" var @@ show_cir_value value

let show_cir_block { bid; bdesc; stmts; terminator } =
  Printf.sprintf "B%d: /* %s */\n%s%s\n" bid bdesc
    (String.concat "\n"
    @@ List.rev_map (fun stmt -> "    " ^ show_cir_stmt stmt) stmts)
  @@ "\n    "
  ^ show_cir_terminator terminator

let show_cir_func { fname; blocks; hvars; params; locals; _ } =
  Printf.sprintf "fn %s [%s](%s) {\n%s\n\n%s\n}\n" fname
    (String.concat "," @@ List.rev_map show_cir_variable hvars)
    (String.concat "," @@ List.rev_map show_cir_variable params)
    (String.concat "\n"
    @@ List.rev_map (fun var -> "    " ^ show_cir_variable var) locals)
    (String.concat "\n" @@ List.rev_map show_cir_block blocks)

let show_cir cir =
  String.concat "\n" @@ List.rev_map show_cir_func cir.functions
