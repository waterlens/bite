{
open Parser
open Lexing

type error =
  | UnexpectedToken of string
  | UnexpectedEof of string 
  | InvalidStringLiteral

exception SyntaxError of error * position

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
    ; ("let"        , TK_LET)
    ; ("var"        , TK_VAR)
    ; ("resume"     , TK_RESUME)
    ; ("void"       , TK_VOID)
    ; ("int"        , TK_INT)
    ; ("float"      , TK_FLOAT)
    ; ("bool"       , TK_BOOL)
    ; ("class"      , TK_CLASS)
    ; ("interface"  , TK_INTERFACE)
    ; ("fn"         , TK_FUNCTION)
    ; ("try"        , TK_TRY)
    ; ("raise"      , TK_RAISE)
    ]
}

let newline       = '\r'?'\n'
let whitespace    = ['\t' ' ']+
let ddigit        = ['0'-'9']
let hdigit        = ['0'-'9' 'a'-'f' 'A'-'F']
let alpha         = ['a'-'z' 'A'-'F']
let alphanum      = alpha | ddigit
let identfier     = (alpha | '_') (alphanum | '_') *  
let int_lit       = ddigit+
let float_lit     = ddigit+? '.' ddigit+
let escaped       = ['\\' '\'' '\"' 'n' 't' 'r' ' ']
let bool_lit      = "true" | "false"

rule tokenize = parse
  | eof             { TK_EOF }
  | whitespace      { tokenize lexbuf }
  | newline         { new_line lexbuf; tokenize lexbuf }
  | "//"            { comment lexbuf }
  | float_lit as x  { TK_FLOAT_LITERAL (float_of_string x) }
  | int_lit as x    { TK_INT_LITERAL (int_of_string x) }
  | bool_lit as x   { TK_BOOL_LITERAL (bool_of_string x) } 
  | "\""            
                    { 
                      let buf = Buffer.create 64 in
                        string buf lexbuf;
                        TK_STR_LITERAL (Buffer.contents buf)
                    }
  | identfier as x  { 
                      match Hashtbl.find_opt keywords_table x with
                      | Some token -> token
                      | None       -> TK_ID x
                    }
  | _               { TK_EOF (* TODO *) }

and comment = parse
  | newline     { new_line lexbuf; tokenize lexbuf }
  | _           { comment lexbuf }

and string buffer = parse
  | '\"'                { () }
  | '\\' (escaped as c) { Buffer.add_char buffer (convert_escaped c) ; string buffer lexbuf }
  | '\\' _ | newline    { raise (SyntaxError (InvalidStringLiteral, lexeme_start_p lexbuf)) }
  | _ as c              { Buffer.add_char buffer c; string buffer lexbuf }
  | eof                 { raise (SyntaxError (UnexpectedEof ("string literal doesn't terminate"), lexeme_start_p lexbuf))}