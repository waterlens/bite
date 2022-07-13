%left "||"
%left "&&"
%left "==" "!="
%nonassoc "<" "<=" ">" ">="
%left "+" "-"
%left "*" "/" "%"
%nonassoc prec_unary
%left "."
%nonassoc "("

%start entry
%type <unit> entry

%%
entry:
| "<shebang>"? item* TK_EOF
    {}

%inline item:
| fn_definition
    {}
| class_definition
    {}
| interface_definition
    {}

fn_definition:
| fn_signature fn_body {}

class_definition:
| class_signature class_body {}

interface_definition:
| interface_signature interface_body {}

fn_signature:
| "fn" identifier generic_signature? parameter_list type_annotation? {}

parameter_list:
| "(" separated_list(",", parameter_declaration) ")" {}

parameter_declaration:
| composite_type                    {}
| identifier ":" composite_type     {}

class_signature:
| "class" identifier generic_signature? type_annotation? {}

interface_signature:
| "interface" identifier generic_signature? {}

type_annotation:
| ":" type_signature {}

generic_signature:
| "<" separated_list(",", quoted_type) ">" {}

type_signature:
| composite_type {}

primitive_type:
| "unit"    {}
| "int"     {}
| "float"   {}
| "bool"    {}
| "str"     {}

quoted_type:
| "'" identifier    {}

atomic_type:
| primitive_type            {}
| quoted_type               {}
| identifier                {}
| "(" composite_type ")"    {}
| hole                      {}

instantiated_type:
| atomic_type quoted_type* {}

product_type:
| separated_nonempty_list("*", instantiated_type)  {}

sum_type:
| separated_nonempty_list("|", product_type) {}

raised_type:
| "~" sum_type {}

composite_type:
| separated_nonempty_list("->", sum_type) raised_type? {}

fn_body:
| block {}

class_body:
| "{" member* "}" {}

interface_body:
| "{" interface_member* "}" {}

stmt:
| ";"           {}
| item          {}
| decl_stmt     {}
| expr_stmt     {}
| ctl_stmt      {}

decl_stmt:
| "let" identifier type_annotation? "=" expr ";"    {}
| "var" identifier type_annotation? "=" expr ";"    {}

expr_stmt:
| expr ";"             {}
| block                {}

ctl_stmt:
| "resume" expr_stmt            {}
| "if" expr block else_clause?  {}
| "while" expr block            {}
| "try" block with_clause*      {}

with_clause:
| "with" fn_signature block     {}

else_clause:
| "else" block {}

block:
| "{" stmt* "}"   {}   

member:
| var_definition    {}
| fn_definition     {}

interface_member:
| fn_signature      {}

var_definition:
| "var" identifier type_annotation {}

expr:
| "(" expr ")"                              {}
| expr binary_op expr                       {}
| unary_op expr %prec prec_unary            {}
| literal                                   {}
| array_expr                                {}
| call_expr                                 {}
| field_expr                                {}
| path_expr                                 {}

array_expr:
| "[" separated_nonempty_list(",", expr) "]" {}

field_expr:
| expr "." identifier                       {}

path_expr:
| identifier generic_arguments?             {}

%inline call_expr: 
| expr "(" separated_list(",", expr) ")"    {}

generic_arguments:
| "::" "<" separated_list(",", composite_type) ">" {}

%inline unary_op:
| "!"   {}
| "+"   {}
| "-"   {}

%inline binary_op:
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

literal:
| "<int>"                   {}
| "<float>"                 {}
| "<string>"                {}
| "<bool>"                  {}

hole:
| "_" {}