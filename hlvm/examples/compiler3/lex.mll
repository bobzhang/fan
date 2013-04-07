{
  open Parse

  let ident = function
    | "let" -> LET
    | "rec" -> REC
    | "in" -> IN
    | "if" -> IF
    | "then" -> THEN
    | "else" -> ELSE
    | "begin" -> OPEN
    | "end" -> CLOSE
    | "match" -> MATCH
    | "with" -> WITH
    | "type" -> TYPE
    | "of" -> OF
    | id -> IDENT id
}

let digit = ['0'-'9']
let exponent = ['e' 'E'] ['+' '-']? digit+
let floating = digit* '.' digit+ | digit+ '.' digit*
let ident = ['A'-'Z' 'a'-'z'] ['A'-'Z' 'a'-'z' '0'-'9' '_' '\'']*

rule token = parse
  | [' ' '\t']       { token lexbuf }
  | '\n'             { Lexing.new_line lexbuf; token lexbuf }
  | floating as s    { FLOAT s }
  | digit+ as s 'L'  { INT64 s }
  | digit+ as s      { INT s }
  | ident as s       { ident s }
  | "'\\n'"          { CHAR '\n' }
  | "'\\t'"          { CHAR '\t' }
  | ''' (_ as c) ''' { CHAR c }
  | "(*"             { comment lexbuf; token lexbuf }
  | "::"             { CONS }
  | ";;"             { SEMISEMI }
  | "<="             { LE }
  | "<>"             { NE }
  | ">="             { GE }
  | "->"             { RIGHTARROW }
  | "<-"             { LEFTARROW }
  | "&&"             { AND }
  | "||"             { OR }
  | "_"              { UNDERSCORE }
  | '"'              { string lexbuf }
  | '('              { OPEN }
  | ')'              { CLOSE }
  | '['              { SQOPEN }
  | ']'              { SQCLOSE }
  | '|'              { PIPE }
  | ','              { COMMA }
  | "+."             { PLUS }
  | "-."             { MINUS }
  | "*."             { TIMES }
  | "/."             { DIVIDE }
  | '+'              { PLUS }
  | '-'              { MINUS }
  | '*'              { TIMES }
  | '/'              { DIVIDE }
  | '%'              { MOD }
  | '<'              { LT }
  | '='              { EQ }
  | '>'              { GT }
  | ':'              { COLON }
  | ';'              { SEMI }
  | '.'              { DOT }
  | eof              { raise End_of_file }

and string = parse
  | (([^ '"'] | "\\\"")* as str) '"' { STRING str }
  | eof              { invalid_arg "EOF inside string" }

and comment = parse
  | "*)"             { () }
  | "(*"             { comment lexbuf; comment lexbuf }
  | _                { comment lexbuf }
  | eof              { invalid_arg "EOF inside comment" }
