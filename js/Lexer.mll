{
type token =
  | ID of (string)
  | AND
  | OR
  | NOT
  | OP
  | CL
  | EOF
}

rule lex = parse
| [' ' '\t']      { lex lexbuf }
| "and"           { AND }
| "or"            { OR }
| "not"           { NOT }
| "("             { OP }
| ")"             { CL }
| ['A'-'Z' 'a'-'z' '_']['0'-'9' 'A'-'Z' 'a'-'z' '_']* as s { ID (s) }
| eof             { EOF }

    
    
