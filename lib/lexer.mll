{
open Tokens
open Lexing

type error =
  | UnexpectedToken of string
  | UnexpectedEof of string 
  | InvalidStringLiteral

exception Error of error * position

let convert_escaped c =
  match c with
  | 'n' -> '\n'
  | 't' -> '\t'
  | 'r' -> '\r'
  | _   -> c

let keywords_table = Hashtbl.of_seq @@ List.to_seq
    [ ("if"         , TK_IF)
    ; ("else"       , TK_ELSE)
    ; ("while"      , TK_WHILE)
    ; ("for"        , TK_FOR)
    ; ("match"      , TK_MATCH)
    ; ("let"        , TK_LET)
    ; ("var"        , TK_VAR)
    ; ("resume"     , TK_RESUME)
    ; ("unit"       , TK_UNIT)
    ; ("int"        , TK_INT)
    ; ("float"      , TK_FLOAT)
    ; ("bool"       , TK_BOOL)
    ; ("str"        , TK_STR)
    ; ("record"     , TK_RECORD)
    ; ("enum"       , TK_ENUM)
    ; ("synonym"    , TK_SYNONYM)
    ; ("class"      , TK_CLASS)
    ; ("interface"  , TK_INTERFACE)
    ; ("fn"         , TK_FUNCTION)
    ; ("effect"     , TK_EFFECT)
    ; ("try"        , TK_TRY)
    ; ("raise"      , TK_RAISE)
    ; ("type"       , TK_TYPE)
    ; ("return"     , TK_RETURN)
    ; ("with"       , TK_WITH)
    ; ("use"        , TK_USE)
    ; ("mod"        , TK_MODULE)
    ]

let sops_table = Hashtbl.of_seq @@ List.to_seq
    [ ('.'  , TK_DOT)
    ; (','  , TK_COMMA)
    ; (':'  , TK_COLON)
    ; (';'  , TK_SEMICOLON)
    ; ('^'  , TK_CARET)
    ; ('_'  , TK_UNDERSCORE)
    ; ('#'  , TK_HASH)
    ; ('('  , TK_L_PAREN)
    ; (')'  , TK_R_PAREN)
    ; ('{'  , TK_L_BRACE)
    ; ('}'  , TK_R_BRACE)
    ; ('['  , TK_L_BRACKET)
    ; (']'  , TK_R_BRACKET)
    ; ('+'  , TK_ADD)
    ; ('-'  , TK_SUB)
    ; ('*'  , TK_MUL)
    ; ('/'  , TK_DIV)
    ; ('%'  , TK_MOD)
    ; ('='  , TK_ASGN)
    ; ('<'  , TK_LT)
    ; ('>'  , TK_GT)
    ; ('!'  , TK_LNOT)
    ; ('|'  , TK_OR)
    ; ('\'' , TK_SQUOTE)
    ; ('~'  , TK_TILDE)
    ]

let mops_table = Hashtbl.of_seq @@ List.to_seq
    [ ("<-"   , TK_L_ARROW)
    ; ("->"   , TK_R_ARROW)
    ; ("=>"   , TK_D_ARROW)
    ; ("=="   , TK_EQ)
    ; ("!="   , TK_NEQ)
    ; (">="   , TK_GE)
    ; ("<="   , TK_LE)
    ; ("&&"   , TK_LAND)
    ; ("||"   , TK_LOR)
    ; ("::"   , TK_DCOLON)
    ; (":["   , TK_COLONBRACKET)
    ]
}

let newline       = "\n"
let whitespace    = ['\t' ' ']+
let ddigit        = ['0'-'9']
let hdigit        = ['0'-'9' 'a'-'f' 'A'-'F']
let alpha         = ['a'-'z' 'A'-'Z']
let alphanum      = alpha | ddigit
let identfier     = (alpha | '_') (alphanum | '_') *  
let int_lit       = ddigit+
let float_lit     = ddigit+? '.' ddigit+
let escaped       = ['\\' '\'' '\"' 'n' 't' 'r' ' ']
let bool_lit      = "true" | "false"
let sops          = ['.' ',' ':' ';' '^' '_' '#' '(' ')' '{' '}' '[' ']' '+' '-' '*' '/' '%' '=' '>' '<' '!' '|' '\'' '~']
let mops          = "<-" | "->" | "=>" | "==" | "!=" | ">=" | "<=" | "&&" | "||" | "::" | ":["
let shebang       = "#!" [^ '\r' '\n']* newline

rule tokenize = parse
  | eof             { TK_EOF }
  | whitespace      { tokenize lexbuf }
  | newline         { new_line lexbuf; tokenize lexbuf }
  | "//"            { comment lexbuf }
  | shebang   as x  { new_line lexbuf; TK_SHEBANG x }
  | float_lit as x  { TK_FLOAT_LITERAL (float_of_string x) }
  | int_lit   as x  { TK_INT_LITERAL (int_of_string x) }
  | bool_lit  as x  { TK_BOOL_LITERAL (bool_of_string x) }
  | "\""            { 
                      let buf = Buffer.create 64 in
                        string buf lexbuf;
                        TK_STR_LITERAL (Buffer.contents buf)
                    }
  | identfier as x  { 
                      match Hashtbl.find_opt keywords_table x with
                      | Some token -> token
                      | None       -> TK_ID x
                    }
  | '\'' (identfier as x)
                    {
                      TK_QID x
                    }
   (* if Option.get raises an exn, it's must be a internal error *)
  | mops as op      { Option.get @@ Hashtbl.find_opt mops_table op}
  | sops as op      { Option.get @@ Hashtbl.find_opt sops_table op }
  | _               { raise @@ Error (UnexpectedToken "unexpected token", lexeme_start_p lexbuf) }

and comment = parse
  | newline     { new_line lexbuf; tokenize lexbuf }
  | _           { comment lexbuf }

and string buffer = parse
  | '\"'                { () }
  | '\\' (escaped as c) { Buffer.add_char buffer (convert_escaped c) ; string buffer lexbuf }
  | '\\' _ | newline    { raise @@ Error (InvalidStringLiteral, lexeme_start_p lexbuf) }
  | _ as c              { Buffer.add_char buffer c; string buffer lexbuf }
  | eof                 { raise @@ Error (UnexpectedEof "string literal doesn't terminate", lexeme_start_p lexbuf)}