(* Control-flow IR *)
type cir = { functions : cir_func list }

and cir_func = {
  fname : string;
  mutable blocks : cir_block list;
  mutable hvars : cir_variable list;
  mutable params : cir_variable list;
  mutable locals : cir_variable list;
  rvar : cir_variable;
  mutable entry_block : cir_block;
  mutable ret_block : cir_block;
}

and cir_block = {
  bid : int;
  mutable insns : cir_insn list;
  mutable termiantors : cir_terminator;
}

and cir_insn =
  | CallWithHandlers of {
      name : string;
      hvar : [ `Variable of cir_variable ] list;
      operand : cir_operand list;
    }
  | BinaryOp of Syntax.binary_op * cir_operand * cir_operand
  | UnaryOp of Syntax.unary_op * cir_operand
  | MakeTuple of cir_operand list
  | MakeTaggedTuple of int * cir_operand list
  | Operand of cir_operand
  | ExtractTupleField of cir_operand * int

and cir_terminator =
  | Branch of cir_operand * cir_block * cir_block
  | Return
  | Jump of cir_block
  | Panic

and cir_var_kind = Parameter | LocalVar | TempVar | RetVar
and cir_variable = { vid : int; vkind : cir_var_kind; vty : cir_type }
and cir_type = Types.ty

and cir_operand =
  [ `Imm of int
  | `IntLiteral of int
  | `StringLiteral of string
  | `FloatLiteral of float
  | `Variable of cir_variable
  | `Value of cir_insn ]

type cir_building_ctx = {
  mutable fn_ctx : (string, cir_func) Scope.scope;
  mutable ty_ctx : Semantic.context;
  mutable var_ctx : (string, cir_variable) Scope.scope;
  mutable pos : cir_block;
  func : cir_func;
}

let entry_scope ctx = ctx.var_ctx <- Scope.entry ctx.var_ctx
let leave_scope ctx = ctx.var_ctx <- Scope.leave ctx.var_ctx

let next_id id =
  let n = !id in
  id := n + 1;
  n

let make_cir_variable id kind ty =
  let vid = next_id id in
  let vkind = kind in
  let vty = ty in
  { vid; vkind; vty }

let make_initial_blocks id =
  let id1, id2, id3 = (next_id id, next_id id, next_id id) in
  let return = { bid = id2; insns = []; termiantors = Return } in
  let entry = { bid = id1; insns = []; termiantors = Jump return } in
  let panic = { bid = id3; insns = []; termiantors = Panic } in
  [ entry; return; panic ]

let make_cir_func ctx name generic_param param ty_ann body =
  ignore @@ entry_scope;
  let var_id = ref 1 in
  let blk_id = ref 1 in
  let rty, _ = ty_ann in
  let rvar = make_cir_variable var_id RetVar rty in
  List.iter
    (function
      | Syntax.HandlerVar (name, ty) ->
          Scope.insert ctx.var_ctx name @@ make_cir_variable var_id Parameter ty
      | GenericVar _ -> ())
    generic_param;
  ignore @@ entry_scope;
  List.iter
    (fun (name, ty) ->
      Scope.insert ctx.var_ctx name @@ make_cir_variable var_id Parameter ty)
    param;
  (* TODO *)
  ignore @@ leave_scope;
  ignore @@ leave_scope