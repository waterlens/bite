%left "||"
%left "&&"
%left "==" "!="
%left "<" "<=" ">" ">="
%left "+" "-"
%left "*" "/" "%"
%nonassoc UNARY
%left "."

%start entry
%type <unit> entry

%%
entry:
| definition* TK_EOF
    {}

definition:
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
| "fn" "<id>" generic_signature? "(" ")" type_specialization? {}

class_signature:
| "class" "<id>" generic_signature? type_specialization? {}

interface_signature:
| "interface" "<id>" generic_signature? {}

fn_body:
| "{" "}" {}

class_body:
| "{" "}" {}

interface_body:
| "{" "}" {}

type_specialization:
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
| "'" "<id>"        {}

atomic_type:
| primitive_type    {}
| quoted_type       {}
| "<id>"            {}

instantiated_type:
| atomic_type quoted_type* {}

product_type:
| separated_nonempty_list("*", instantiated_type)  {}

sum_type:
| separated_nonempty_list("|", product_type) {}

composite_type:
| separated_nonempty_list("->", sum_type) option("~") {}

expr:
| expr binary_op expr {}
| unary_op expr %prec UNARY {}

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
| "."   {}