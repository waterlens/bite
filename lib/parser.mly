%{
open Syntax
open Types
%}

%left "||"
%left "&&"
%left "==" "!="
%nonassoc "<" "<=" ">" ">="
%left "+" "-"
%left "*" "/" "%"
%nonassoc prec_unary
%left "."
%nonassoc "(" "["
%nonassoc ":["

%start entry
%type <prog> entry

%%
entry:
| "<shebang>"? xs = item* TK_EOF { Prog xs }

item:
| x = fn_definition { x }
| x = type_definition { x }
| x = effect_definition { x }

// Synatx elements of function definiton
fn_definition:
| sign = fn_signature body = block(stmt)
  {
    let (name, generic_param, param, ty_ann) = sign in
    Func { name; generic_param; param; ty_ann; body }
  }

fn_signature:
| "fn" name = identifier
  generic_param = loption(complex_generic_signature)
  param = delimited_split_list("(", parameter_declaration, ")")
  ty_ann = type_annotation
  { (name, generic_param, param, ty_ann) }

simple_generic_signature:
| xs = delimited_split_list("[", generic_var, "]")
  { xs }

complex_generic_signature:
| xs = delimited_split_list("[", generic_var_or_handler_var, "]")
  { xs }

generic_var:
| x = identifier
  { x }

generic_var_or_handler_var:
| x = generic_var
  { GenericVar x  }
| x = identifier ":" t = composite_type
  { HandlerVar (x, t) }

type_annotation:
| ":" t1 = composite_type t2 = preceded("~", composite_type)?
  { (t1, t2) }

parameter_declaration:
| x = identifier t = preceded(":", composite_type)
  { (x, t) }


// Syntax elements of type definition (specifically record type, enum type and synonym)
type_definition:
| x = enum_type_definition { TyDef x }
| x = record_type_definition { TyDef x }
| x = synonym_type_definition { TyDef x }

enum_type_definition:
| "enum" name = identifier
  generic_param = loption(simple_generic_signature)
  ctors = block(constructor)
  { Enum { name; generic_param; ctors } }

constructor:
| ctor_name = identifier
  ctor_params = loption(delimited_split_list("(", composite_type, ")"))
  { { ctor_name; ctor_params } }

record_type_definition:
| "record" name = identifier generic_param = loption(simple_generic_signature) fields = block(field)
  { Record { name; generic_param; fields } }

field:
| x = identifier ":" ty = composite_type
  { (x, ty) }

synonym_type_definition:
| "synonym" name = identifier generic_param = loption(simple_generic_signature) "=" ty = composite_type
  { Synonym { name; generic_param; ty } }

// Syntax elements of effect definition
effect_definition:
| "effect" name = identifier
  generic_param = loption(simple_generic_signature) 
  "{" op = effect_operation_signature "}"
  {
    Eff { name; generic_param; op }
  }

effect_operation_signature:
| op_name = identifier
  op_generic_param = loption(complex_generic_signature)
  op_param = delimited_split_list("(", composite_type, ")")
  op_ty_ann = type_annotation
  { { op_name; op_generic_param; op_param; op_ty_ann } }

// Syntax elements of expressions and statements
stmt:
| ";"               { Empty }
| x = decl_stmt     { x }
| x = expr_stmt     { Expr x }
| x = ctl_stmt      { Ctl x }

decl_stmt:
| "let" name = identifier ty = preceded(":", composite_type) "=" init = expr ";"
  { Bind { name; ty; init } }

expr_stmt:
| x = expr ";"
  { x }
| xs = block(stmt)
  { BlockExpr xs }

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
| eff_name = identifier ":" eff_ty_ann = composite_type
  "{" h = operation_handler "}"
  {
    let (handler_name, handler_ty_ann, handler_generic_param, handler_arg, handler_stmt) = h in
    { eff_name; eff_ty_ann; handler_name; handler_ty_ann; handler_generic_param; handler_arg; handler_stmt }
  }

operation_handler:
| handler_name = identifier
  handler_generic_param = loption(complex_generic_signature)
  handler_arg = delimited_split_list("(", parameter_declaration, ")")
  handler_ty_ann = type_annotation
  handler_stmt = block(stmt)
  { (handler_name, handler_ty_ann, handler_generic_param, handler_arg, handler_stmt) }

else_clause:
| "else" xs = block(stmt)
  { xs }

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
| x = index_expr
  { x }
| x = tuple_expr
  { TupleExpr x }
| x = identifier
  { VarExpr x }
| x = expr ga = generic_args
  { GenericExpr (x, ga) }

array_expr:
| "[" xs = separated_list(",", expr) "]"
  { xs }

field_expr:
| x = expr "." y = identifier
  { FieldExpr (x, Named y) }
| x = expr "." y = "<int>"
  { FieldExpr (x, Ordinal y) }

index_expr:
| x = expr "[" y = expr "]"
  { IndexExpr (x, y) }

generic_args:
| ":["
    ga = separated_nonempty_list(",", composite_type)
  "]"
  { ga }

call_expr:
| f = expr
  xs = delimited_split_list("(", expr, ")")
  { CallExpr (f, xs) }

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

literal:
| x = "<int>"           { IntLiteral x }
| x = "<float>"         { FloatLiteral x }
| x = "<string>"        { StringLiteral x }
| x = "<bool>"          { BoolLiteral x}

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
| x = identifier
  { TyIdent x }

atomic_type:
| t = primitive_type
  { t }
| t = type_variable
  { t }
| "(" t = composite_type ")"
  { t }
| "_"
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

arrow_type:
| t = sum_type
  { t }
| t1 = sum_type "->" t2 = arrow_type
  { TyArrow (t1, t2, None) }

composite_type:
| t = arrow_type
  { t }
