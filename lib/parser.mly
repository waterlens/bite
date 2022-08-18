%{
open Syntax
%}

%nonassoc "="
%left "||"
%left "&&"
%left "==" "!="
%nonassoc "<" "<=" ">" ">="
%left "+" "-"
%left "*" "/" "%"
%nonassoc prec_unary
%left "."
%nonassoc "(" "["

%start entry
%type <prog> entry

%%
entry:
| "<shebang>"? xs = item* TK_EOF { Prog xs }

item:
| x = mod_definition { x }
| x = use_directive { x }
| x = fn_definition { x }
| x = type_definition { x }
| x = effect_definition { x }

// Syntax elements of modules
mod_definition:
| "mod" name = "<id>"?  items = block(item)
  {
    Module { name; items }
  }

// Synatx elements of use directive
use_directive:
| "use" x = path
  {
    Use x
  }

// Synatx elements of function definiton
fn_definition:
| sign = fn_signature body = block(stmt)
  {
    let (name, ty_param, ty, tm_param) = sign in
    Func { name; ty_param; ty; tm_param; body }
  }

fn_signature:
| "fn" name = identifier ty_param = loption(generic_signature)
  params = delimited_split_list("(", parameter_signature, ")")
  ty = type_annotation?
  {
    let (idents, tys) =  List.split params in
    let (t1, t2) = Option.value ty ~default:(TyHole, TyHole) in
    let ty = TyArrow (TyProd tys, t1, t2) in
    (name, ty_param, ty, idents)
  }

%inline generic_signature:
| xs = delimited_split_list("[", type_constraint, "]")
  {
    xs
  }

parameter_signature:
| x = identifier ":" t = composite_type
  {
    (Some x, t)
  }
| t = composite_type
  {
    (None, t)
  }

type_annotation:
| ":" t = incomplete_type
  {
    t
  }

parameter_declaration:
| x = identifier t = preceded(":", composite_type)?
  {
    (x, Option.value t ~default:TyHole)
  }

// Syntax elements of type definition (specifically record type, enum type and synonym)
type_definition:
| x = enum_type_definition { TyDef x }
| x = record_type_definition { TyDef x }
| x = synonym_type_definition { TyDef x }

enum_type_definition:
| "enum" name = identifier ty_param = loption(generic_signature) ctors = block(constructor)
  {
    Enum { name; ty_param; ctors }
  }

constructor:
| x = identifier params = loption(delimited_split_list("(", parameter_signature, ")"))
  {
    let (idents, tys) = List.split params in
    let ty = TyArrow (TyProd tys, TyHole, TyUnit) in
    (x, ty, idents)
  }

record_type_definition:
| "record" name = identifier ty_param = loption(generic_signature) fields = block(field)
  {
    Record { name; ty_param; fields }
  }

field:
| x = identifier ":" ty = composite_type
  {
    (x, ty)
  }

synonym_type_definition:
| "synonym" name = identifier ty_param = loption(generic_signature) "=" ty = composite_type
  {
    Synonym { name; ty_param; ty }
  }

// Syntax elements of effect definition
effect_definition:
| "effect" name = identifier ty_param = loption(generic_signature) handlers = block(effect_raiser_signature)
  {
    Eff { name; ty_param; handlers }
  }

effect_raiser_signature:
| name = identifier ty_param = loption(generic_signature)
  params = delimited_split_list("(", parameter_signature, ")")
  ty = type_annotation?
  {
    let (_, tys) =  List.split params in
    let (t1, t2) = Option.value ty ~default:(TyHole, TyHole) in
    let ty = TyArrow (TyProd tys, t1, t2) in
    { name; ty_param; ty }
  }

// Syntax elements of expressions and statements
stmt:
| ";"               { Empty }
| x = item ";"      { Item x }
| x = decl_stmt     { x }
| x = expr_stmt     { Expr x }
| x = ctl_stmt      { Ctl x }

decl_stmt:
| "let" name = identifier ty = preceded(":", composite_type)? "=" init = expr ";"
  {
    Bind { name; ty = Option.value ty ~default:TyHole; init; mut = false }
  }
| "var" name = identifier ty = preceded(":", composite_type)? "=" init = expr ";"
  {
    Bind { name; ty = Option.value ty ~default:TyHole; init; mut = true }
  }

expr_stmt:
| x = expr ";"
  {
    x
  }
| xs = block(stmt)
  {
    BlockExpr xs
  }

ctl_stmt:
| "resume" x = expr_stmt
  {
    Resume x
  }
| "if" "(" cond = expr ")" t = block(stmt) f = else_clause?
  {
    If (cond, t, f)
  }
| "while" "(" cond = expr ")" body = block(stmt)
  {
    While (cond, body)
  }
| "try" body = block(stmt) xs = with_clause*
  {
    Try (body, xs)
  }
| "return" x = expr ";"
  {
    Ret x
  }

with_clause:
| "with" x = effect_handler_definition { x }

effect_handler_definition:
| name = identifier ty = type_annotation?
  xs = block(effect_handler_signature) {
    let (ty1, _) = (Option.value ty ~default:(TyHole, TyHole)) in
    (name, ty1, xs)
  }

effect_handler_signature:
| h_name = identifier h_ty_arg = loption(generic_signature)
  params = delimited_split_list("(", parameter_declaration, ")")
  ret_ty = type_annotation? h_stmt = block(stmt)
  {
    let (h_tm_arg, tys) = List.split params in
    let (ret_ty, eff_ty) = Option.value ret_ty ~default:(TyHole, TyHole) in
    let h_ty = TyArrow (TyProd tys, ret_ty, eff_ty) in
    { h_name; h_ty_arg; h_ty; h_tm_arg; h_stmt }
  }

else_clause:
| "else" xs = block(stmt)
  {
    xs
  }

expr:
| "(" x = expr ")"
  { x }
| x = expr op = binary_op y = expr
  { BinaryExpr (op, x, y) }
| op = unary_op x = expr %prec prec_unary
  { UnaryExpr (op, x) }
| x = literal
  { Literal x }
| xs = array_expr
  { ArrayExpr xs }
| x = call_expr
  { x }
| x = field_expr
  { x }
| x = path_expr
  { PathExpr x}
| x = index_expr
  { x }
| x = record_expr
  { x }
| x = tuple_expr
  { TupleExpr x }

array_expr:
| "[" xs = separated_nonempty_list(",", expr) "]"
  { xs }

field_expr:
| x = expr "." y = identifier
  { FieldExpr (x, Named y) }
| x = expr "." y = "<int>"
  { FieldExpr (x, Ordinal y) }

path_expr:
| x = path
  { x }

index_expr:
| x = expr "[" y = expr "]"
  { IndexExpr (x, y) }

call_expr:
| f = expr xs = delimited_split_list("(", expr, ")")
  { CallExpr (f, xs) }

generic_arguments:
| xs = delimited_split_list(":[", composite_type, "]")
  { xs }

record_expr:
| x = identifier y = generic_arguments? z = block(terminated(field_initializer, ","?))
  { RecordExpr (x, y, z) }

field_initializer:
| "." x = identifier y = preceded("=", expr)?
  { (x, y) }

%inline tuple_expr:
| "(" ")"                                           { [] }
| "(" x = expr "," ")"                              { [x] }
| "(" x = expr xs = preceded(",", expr)+ ")"        { x :: xs }

// Some basic syntax elements

%inline unary_op:
| "!"   { LNot }
| "+"   { Plus }
| "-"   { Minus }

%inline binary_op:
| "+"   { Add }
| "-"   { Sub }
| "*"   { Mul }
| "/"   { Div }
| "%"   { Mod }
| "=="  { Eq }
| "!="  { Neq }
| ">"   { Gt }
| ">="  { Ge }
| "<"   { Lt }
| "<="  { Le }
| "&&"  { LAnd }
| "||"  { LOr }

identifier:
| x = "<id>"  { x }

quoted_identifier:
| x = "<qid>"   { x }

literal:
| x = "<int>"           { IntLiteral x }
| x = "<float>"         { FloatLiteral x }
| x = "<string>"        { StringLiteral x }
| x = "<bool>"          { BoolLiteral x}

hole:
| "_" { TyHole }

path:
| xs = separated_nonempty_list("::", pair(identifier, generic_arguments?))
  { xs }

delimited_split_list(x, param, y):
| x xs = separated_list(",", param) y { xs }

block(content):
| "{" xs = content* "}"   { xs }

// Syntax elements of types

primitive_type:
| "unit"    { TyUnit }
| "int"     { TyInt }
| "float"   { TyFloat }
| "bool"    { TyBool }
| "str"     { TyStr }

type_variable:
| x = identifier        { Ident x }
| x = quoted_identifier { QuotedIdent x }

atomic_type:
| t = primitive_type
  { t }
| t = type_variable
  { t }
| "(" t = composite_type ")"
  { t }
| hole
  { TyHole }

instantiated_type:
| x = atomic_type args = loption(delimited_split_list("[", composite_type, "]"))
  { TyApp (x, args) }

product_type:
| xs = separated_nonempty_list("*", instantiated_type)
  { TyProd xs }

sum_type:
| xs = separated_nonempty_list("|", product_type)
  { TySum xs }

raised_type:
| "~" t = sum_type
  { t }

arrow_type:
| t = sum_type
  { t }
| t1 = sum_type "->" t2 = arrow_type t3 = raised_type?
  {
    TyArrow (t1, t2, (Option.value t3 ~default:TyHole))
  }

composite_type:
| t = arrow_type
  { t }

incomplete_type: 
| t = composite_type sfix = raised_type?
  { 
    (t, Option.value sfix ~default:TyHole)
  }


type_constraint:
| x = type_variable ":" y = composite_type
  { TyConstr (x, y) }
| x = composite_type
  { x }
