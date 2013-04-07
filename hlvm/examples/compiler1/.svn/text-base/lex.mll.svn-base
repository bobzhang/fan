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
  | digit+ as s      { INT s }
  | ident as s       { ident s }
  | "'\\n'"          { CHAR '\n' }
  | "'\\t'"          { CHAR '\t' }
  | ''' (_ as c) ''' { CHAR c }
  | "::"             { CONS }
  | ";;"             { SEMISEMI }
  | '('              { OPEN }
  | ')'              { CLOSE }
  | '|'              { PIPE }
  | ','              { COMMA }
  | '+'              { PLUS }
  | '-'              { MINUS }
  | '*'              { TIMES }
  | '/'              { DIVIDE }
  | "<="             { LE }
  | '<'              { LT }
  | '='              { EQ }
  | "<>"             { NE }
  | ">="             { GE }
  | '>'              { GT }
  | ':'              { COLON }
  | ';'              { SEMI }
  | eof              { EOF }
