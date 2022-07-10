/* Literals and Identifier */
%token <int>    TK_INT_LITERAL
%token <float>  TK_FLOAT_LITERAL
%token <string> TK_STR_LITERAL
%token <bool>   TK_BOOL_LITERAL
%token <string> TK_ID

/* Keywords */
%token TK_IF
%token TK_ELSE
%token TK_WHILE
%token TK_FOR
%token TK_LET
%token TK_VAR
%token TK_RESUME
%token TK_WITH
%token TK_VOID
%token TK_INT
%token TK_FLOAT
%token TK_BOOL
%token TK_CLASS
%token TK_INTERFACE
%token TK_FUNCTION
%token TK_TRY
%token TK_RAISE

/* Symbols */
%token TK_COLON
%token TK_L_PAREN
%token TK_R_PAREN
%token TK_L_BRACE
%token TK_R_BRACE
%token TK_L_BRACKET
%token TK_R_BRACKET

%token TK_EOF

%start entry
%type <unit> entry

%%
entry:
    TK_EOF          {}
;