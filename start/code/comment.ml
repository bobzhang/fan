let depth = ref 0
    
let rec f  = {:lexer|
 | "(*" -> comment lexbuf
 | '"' -> (print_char '"'; string lexbuf)
 | _ as c -> (print_char c; f lexbuf)
 | ! -> exit 0
|}
and comment  = {:lexer|
 | "*)" ->
     if !depth = 0 then f lexbuf
     else begin
       decr depth;
       comment lexbuf
     end
 | "(*" -> incr depth
 | _ -> comment lexbuf
 | ! -> failwith "unterminated comment"
|}
and string = {:lexer|
 | '"' -> (print_char '"'; f lexbuf)
 | _ as c -> (print_char c; string lexbuf)
 | ! -> failwith "unterminated string"
|}

let _ = f (Lexing.from_channel (open_in "comment.ml"));;    

(* (* nested *))*)
(* local variables: *)
(* compile-command: "ocamlc -annot -pp 'fan.native' comment.ml -o comment && ./comment" *)
(* end: *)
