open Llvm

type lty =
  | Unit
  | BoxedI64
  | RawI64
  | BoxedF64
  | RawF64
  | RawBool
  | String
  | Array of lty
  | Tuple of lty list
  | Tagged
  | Closure
  | Opaque

exception UnsupportedType of string

let rec cty_to_lty = function
  | Types.TyUnit -> Unit
  | TyInt -> RawI64
  | TyFloat -> RawF64
  | TyBool -> RawBool
  | TyStr -> String
  | TyProd tys -> Tuple (List.map cty_to_lty tys)
  | TyEnum _ -> Tagged
  | TyArray ty -> Array (cty_to_lty ty)
  | TyArrow _ -> Closure
  | TyVar { contents = Link _ } -> Opaque
  | TyHole -> Opaque
  | _ as t -> raise @@ UnsupportedType (Types.show_ty t)

let insert_types ctx =
  let i8 = i8_type ctx in
  let any_ptr = pointer_type i8 in
  let i64 = i64_type ctx in
  let i32 = i32_type ctx in
  let obj = named_struct_type ctx "_object" in
  struct_set_body obj [| i64 |] true;
  let string = named_struct_type ctx "_string" in
  struct_set_body string [| obj; i32; array_type i8 0 |] true;
  let array = named_struct_type ctx "_array" in
  struct_set_body array [| obj; i32; any_ptr; i32 |] true;
  let closure = named_struct_type ctx "_closure" in
  struct_set_body closure [| obj; i32; any_ptr; any_ptr |] true;
  let tuple = named_struct_type ctx "_tuple" in
  struct_set_body tuple [| obj; i32; array_type any_ptr 0 |] true;
  let tagged = named_struct_type ctx "_enum" in
  struct_set_body tagged [| obj; i32; any_ptr |] true

let string ctx = named_struct_type ctx "_string"
let array ctx = named_struct_type ctx "_array"
let closure ctx = named_struct_type ctx "_closure"
let tuple ctx = named_struct_type ctx "_tuple"
let tagged ctx = named_struct_type ctx "_tagged"

let rec lty_to_llty ctx = function
  | Unit -> i1_type ctx
  | BoxedI64 -> pointer_type @@ i64_type ctx
  | RawI64 -> i64_type ctx
  | BoxedF64 -> pointer_type @@ double_type ctx
  | RawF64 -> double_type ctx
  | RawBool -> i1_type ctx
  | String -> double_type ctx
  | Array _ -> array ctx
  | Tuple _ -> tuple ctx
  | Tagged -> tagged ctx
  | Closure -> closure ctx
  | Opaque -> pointer_type @@ i8_type ctx

let cty_to_llty ctx ty = cty_to_lty ty |> lty_to_llty ctx

exception NotImplemented

let codegen =
  let ctx = create_context () in
  let m = create_module ctx "bite module" in
  let builder = builder ctx in
  let conv_ty = cty_to_llty ctx in
  let i1 = i1_type ctx in
  let i64 = i64_type ctx in
  let f64 = double_type ctx in

  let make_allocas symtbl vars =
    List.iter
      (fun Cir.{ vid; vty; _ } ->
        Hashtbl.add symtbl vid @@ build_alloca (conv_ty vty) "" builder)
      vars
  in

  let assign_args_to_locals symtbl locals f bd =
    List.iteri
      (fun idx Cir.{ vid; _ } ->
        let alloca = Hashtbl.find symtbl vid in
        ignore @@ build_store (param f idx) alloca bd)
      locals
  in
  let append_blocks ctx f cir_blocks =
    let blktbl = Hashtbl.create 32 in
    List.iter
      (fun Cir.{ bid; bdesc; _ } ->
        if bdesc = "dummy" then ()
        else
          let bb =
            if bdesc = "entry" then entry_block f else append_block ctx bdesc f
          in
          Hashtbl.add blktbl bid @@ bb)
      cir_blocks;
    blktbl
  in
  let rec codegen cir =
    match cir with
    | Cir.{ functions } ->
        List.iter define_1 functions;
        List.iter define_2 functions;
        m
  and define_1 Cir.{ fname; hvars; params; rvar; _ } =
    let Cir.{ vty; _ } = Option.get rvar in
    let fty =
      function_type (conv_ty vty)
      @@ Array.map (fun Cir.{ vty; _ } -> conv_ty vty)
      @@ Array.of_list @@ List.append hvars params
    in
    ignore @@ define_function fname fty m
  and define_2 Cir.{ fname; blocks; hvars; params; locals; rvar; _ } =
    let symtbl = Hashtbl.create 64 in
    let f = Option.get @@ lookup_function fname m in
    let ll_entry = entry_block f in

    position_at_end ll_entry builder;

    make_allocas symtbl [ Option.get rvar ];
    make_allocas symtbl hvars;
    make_allocas symtbl params;
    make_allocas symtbl locals;

    assign_args_to_locals symtbl (List.append hvars params) f builder;

    let bbtbl = append_blocks ctx f blocks in

    let rec emit_value bd = function
      | `Imm i | `ILit i -> const_int i64 i
      | `BLit b -> const_int i64 (if b then 1 else 0)
      | `SLit _ -> raise NotImplemented
      | `FLit f -> const_float f64 f
      | `Var v -> emit_variable bd v
      | `Insn insn -> emit_insn m bd insn
      | `Unit -> const_int i1 0
    and emit_variable bd Cir.{ vid; _ } =
      build_load (Hashtbl.find symtbl vid) "" bd
    and emit_insn m bd = function
      | Cir.Call ({ fname; _ }, args) ->
          build_call
            (lookup_function fname m |> Option.get)
            (Array.map (fun value -> emit_value bd value) @@ Array.of_list args)
            "" bd
      | Cir.BinaryOp (op, l, r) -> (
          let l = emit_value bd l in
          let r = emit_value bd r in
          let choose ll rr =
            match (classify_type @@ type_of l, classify_type @@ type_of r) with
            | Integer, Integer -> ll ()
            | Double, Double -> rr ()
            | _ -> raise NotImplemented
          in
          match op with
          | Add ->
              choose
                (fun _ -> build_add l r "" bd)
                (fun _ -> build_fadd l r "" bd)
          | Sub ->
              choose
                (fun _ -> build_sub l r "" bd)
                (fun _ -> build_fsub l r "" bd)
          | Mul ->
              choose
                (fun _ -> build_mul l r "" bd)
                (fun _ -> build_fmul l r "" bd)
          | Div ->
              choose
                (fun _ -> build_sdiv l r "" bd)
                (fun _ -> build_fdiv l r "" bd)
          | Mod ->
              choose
                (fun _ -> build_srem l r "" bd)
                (fun _ -> build_frem l r "" bd)
          | Eq ->
              choose
                (fun _ -> build_icmp Icmp.Eq l r "" bd)
                (fun _ -> build_fcmp Fcmp.Oeq l r "" bd)
          | Neq ->
              choose
                (fun _ -> build_icmp Icmp.Ne l r "" bd)
                (fun _ -> build_fcmp Fcmp.One l r "" bd)
          | Gt ->
              choose
                (fun _ -> build_icmp Icmp.Sgt l r "" bd)
                (fun _ -> build_fcmp Fcmp.Ogt l r "" bd)
          | Ge ->
              choose
                (fun _ -> build_icmp Icmp.Sge l r "" bd)
                (fun _ -> build_fcmp Fcmp.Oge l r "" bd)
          | Lt ->
              choose
                (fun _ -> build_icmp Icmp.Slt l r "" bd)
                (fun _ -> build_fcmp Fcmp.Olt l r "" bd)
          | Le ->
              choose
                (fun _ -> build_icmp Icmp.Sle l r "" bd)
                (fun _ -> build_fcmp Fcmp.Ole l r "" bd)
          | LAnd | LOr -> raise NotImplemented)
      | Cir.UnaryOp (op, v) -> (
          let v = emit_value bd v in
          match op with
          | LNot -> (
              match classify_type @@ type_of v with
              | Integer -> build_icmp Icmp.Eq (const_int (type_of v) 0) v "" bd
              | Double ->
                  build_fcmp Fcmp.Oeq (const_float (type_of v) 0.) v "" bd
              | _ -> raise NotImplemented)
          | Plus -> v
          | Minus -> (
              match classify_type @@ type_of v with
              | Integer -> build_neg v "" bd
              | Double -> build_fneg v "" bd
              | _ -> raise NotImplemented))
      | _ -> raise NotImplemented
    in

    let emit_stmts cirblk llblk =
      let Cir.{ stmts; _ } = cirblk in
      let bd = builder_at_end ctx llblk in
      List.iter
        (fun (lhs, rhs) ->
          let rhs = emit_value bd rhs in
          if Option.is_some lhs then
            let Cir.{ vid; _ } = Option.get lhs in
            let lhs = Hashtbl.find symtbl vid in
            ignore @@ build_store rhs lhs bd)
        stmts
    in
    let emit_all_stmts () =
      List.iter
        (fun blk ->
          let Cir.{ bid; bdesc; _ } = blk in
          if bdesc <> "dummy" then emit_stmts blk @@ Hashtbl.find bbtbl bid)
        blocks
    in
    let emit_terminator cirblk llblk =
      let Cir.{ terminator; _ } = cirblk in
      let bd = builder_at_end ctx llblk in
      match terminator with
      | Return -> ignore @@ build_ret (emit_variable bd @@ Option.get rvar) bd
      | Panic -> ignore @@ build_unreachable bd
      | Jump { bid; _ } -> ignore @@ build_br (Hashtbl.find bbtbl bid) bd
      | Branch (cond, Cir.{ bid = t; _ }, Cir.{ bid = f; _ }) ->
          let v = emit_value bd cond in
          let cond =
            match classify_type @@ type_of v with
            | Integer -> build_icmp Icmp.Ne (const_int (type_of v) 0) v "" bd
            | Double -> build_fcmp Fcmp.One (const_float (type_of v) 0.) v "" bd
            | _ -> raise NotImplemented
          in
          ignore
          @@ build_cond_br cond (Hashtbl.find bbtbl t) (Hashtbl.find bbtbl f) bd
    in
    let emit_all_terminators () =
      List.iter
        (fun blk ->
          let Cir.{ bid; bdesc; _ } = blk in
          if bdesc <> "dummy" then emit_terminator blk @@ Hashtbl.find bbtbl bid)
        blocks
    in
    emit_all_stmts ();
    emit_all_terminators ()
  in
  codegen
