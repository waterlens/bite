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
%type <unit> entry

%%
entry:
| "<shebang>"? item* TK_EOF
    {}

item:
| mod_definition
    {}
| use_directive
    {}
| fn_definition
    {}
| type_definition
    {}
| effect_definition
    {}

// Syntax elements of modules
mod_definition:
| "mod" "<id>"? block(item) {}

// Synatx elements of use directive
use_directive:
| "use" path {}

// Synatx elements of function definiton
fn_definition:
| fn_signature block(stmt) {}

fn_signature:
| "fn" identifier generic_signature?
  delimited_split_list("(", parameter_signature, ")")
  type_annotation? {}

%inline generic_signature:
| delimited_split_list("[", type_constraint, "]") {}

parameter_signature:
| identifier ":" composite_type   {}
| composite_type {}

type_annotation:
| ":" composite_type                 {}

parameter_declaration:
| identifier preceded(":", composite_type)?     {}

// Syntax elements of type definition (specifically record type, enum type and synonym)
type_definition:
| enum_type_definition {}
| record_type_definition {}
| synonym_type_definition {}

enum_type_definition:
| "enum" identifier generic_signature? block(constructor) {}

constructor:
| identifier delimited_split_list("(", parameter_signature, ")")? {}

record_type_definition:
| "record" identifier generic_signature? block(field) {}

field:
| identifier type_annotation {}

synonym_type_definition:
| "synonym" identifier generic_signature? "=" composite_type {}

// Syntax elements of effect definition
effect_definition:
| "effect" identifier generic_signature? block(effect_raiser_signature) {}

effect_raiser_signature:
| identifier generic_signature?
  delimited_split_list("(", parameter_signature, ")")
  type_annotation? {}

// Syntax elements of expressions and statements
stmt:
| ";"           {}
| item ";"      {}
| decl_stmt     {}
| expr_stmt     {}
| ctl_stmt      {}

decl_stmt:
| "let" identifier type_annotation? "=" expr ";"    {}
| "var" identifier type_annotation? "=" expr ";"    {}

expr_stmt:
| expr ";"             {}
| block(stmt)          {}

ctl_stmt:
| "resume" expr_stmt                    {}
| "if" "(" expr ")" block(stmt) else_clause?    {}
| "while" "(" expr ")" block(stmt)              {}
| "try" block(stmt) with_clause*        {}
| "return" expr ";"                     {}

with_clause:
| "with" effect_handler_definition {}

effect_handler_definition:
| identifier type_annotation?
  block(effect_handler_signature) {}

effect_handler_signature:
| identifier generic_signature?
  delimited_split_list("(", parameter_declaration, ")")
  type_annotation? block(stmt) {}

else_clause:
| "else" block(stmt) {}

expr:
| "(" expr ")"                              {}
| expr binary_op expr                       {}
| unary_op expr %prec prec_unary            {}
| literal                                   {}
| array_expr                                {}
| call_expr                                 {}
| field_expr                                {}
| path_expr                                 {}
| index_expr                                {}
| record_expr                               {}
| tuple_expr                                {}

array_expr:
| "[" separated_nonempty_list(",", expr) "]" {}

field_expr:
| expr "." identifier                       {}
| expr "." "<int>"                          {}

path_expr:
| separated_nonempty_list("::", pair(identifier, generic_arguments?))
  {}

index_expr:
| expr "[" expr "]"                                 {}

call_expr: 
| expr delimited_split_list("(", expr, ")")         {}

generic_arguments:
| delimited_split_list(":[", composite_type, "]")   {}

record_expr:
| identifier generic_arguments? block(terminated(field_initializer, ","?))
  {}

field_initializer:
| "." identifier preceded("=", expr)?               {}

%inline tuple_expr:
| "(" ")"                                           {}
| "(" expr "," ")"                                  {}
| "(" expr preceded(",", expr)+ ")"                 {}

// Some basic syntax elements

%inline unary_op:
| "!"   {}
| "+"   {}
| "-"   {}

%inline binary_op:
| "="   {}
| "+"   {}
| "-"   {}
| "*"   {}
| "/"   {}
| "%"   {}
| "=="  {}
| "!="  {}
| ">"   {}
| ">="  {}
| "<"   {}
| "<="  {}
| "&&"  {}
| "||"  {}

identifier:
| "<id>"    {}

quoted_identifier:
| "<qid>"   {}

literal:
| "<int>"                   {}
| "<float>"                 {}
| "<string>"                {}
| "<bool>"                  {}

hole:
| "_" {}

path:
| separated_nonempty_list("::", identifier) {}

delimited_split_list(x, param, y):
| x separated_list(",", param) y {}

block(content):
| "{" content* "}"   {}   

// Syntax elements of types

primitive_type:
| "unit"    {}
| "int"     {}
| "float"   {}
| "bool"    {}
| "str"     {}

type_variable:
| identifier        {}
| quoted_identifier {}

atomic_type:
| primitive_type                                {}
| type_variable                                 {}
| "(" composite_type ")"                        {}
| hole                                          {}

instantiated_type:
| atomic_type delimited_split_list("[", composite_type, "]")? {}

product_type:
| separated_nonempty_list("*", instantiated_type)  {}

sum_type:
| separated_nonempty_list("|", product_type) {}

raised_type:
| "~" sum_type {}

composite_type:
| separated_nonempty_list("->", sum_type) raised_type? {}

type_constraint:
| type_variable ":" composite_type  {}
| composite_type                    {}
