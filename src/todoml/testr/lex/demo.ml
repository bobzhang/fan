#import Fan.Lang.Lex;;
{:with_stru|lex:
let rec token =  {|
  | eof  ->  exit 0
  | "\"" ->  string lexbuf
  | "(*" ->  comment 0 lexbuf
  | _    ->  token lexbuf |}
and string = {|
  | "\"" ->  () 
  | eof  ->  failwith "string not finished"
  | _    ->  string lexbuf |}
and comment n = {|
  | "*)" ->
     if n <> 0 then comment (n-1) lexbuf
  | eof  -> failwith "comment not finished"
  | _    -> comment n lexbuf |}
|};;
