(* #filter "ulex";; *)
(* let regexp number = [ '0'-'9' ]+; *)
import Fan.Lang.Lex;
let rec token =  {:lex|
  | eof -> exit 0
  | "\"" -> string lexbuf
  | "(*" ->  comment 0 lexbuf
  | _ -> token lexbuf |}
and string = {:lex|
  | "\"" ->  () 
  | eof -> failwith "string not finished"
  | _ -> string lexbuf
|}
and comment n = {:lex|
  | "*)" -> if n <> 0 then comment (n-1) lexbuf
  | eof -> failwith "comment not finished"
  | _ -> let _lexeme = Ulexing.lexeme lexbuf in
    comment n lexbuf |};

(* | "<utf8>" -> begin enc := Ulexing.Utf8; token enc lexbuf end *)
(* | "<latin1>" -> begin enc := Ulexing.Latin1; token enc lexbuf end *)
(* | xml_letter+ -> Printf.sprintf "word(%s)" (Ulexing.utf8_lexeme lexbuf) *)
(* | number -> "number" *)
(* | [1234-1246] -> "bla" *)
(* | "(" ->  begin *)
(*     Ulexing.rollback lexbuf; (\* Puts the lexeme back into the buffer *\) *)
(*     {:lex| |"(" [^ '(']* ")" -> Ulexing.utf8_lexeme lexbuf |} lexbuf *)
(*     (\* Note the use of an inline lexer *\) *)
(* end *)
(* | ' ' -> "whitespace" *)
  
(* {:reg| number:[ '0'-'9' ]+ |}; *)
(* let () = *)
(*   let enc = ref Ulexing.Ascii in *)
(*   let lexbuf = Ulexing.from_var_enc_string enc *)
(*       "<utf8>中华人民共和国<latin1>abcd<utf8>大团结<utf8> ghso(\*comment*\)(中国)" *)
(*       (\* "abc<latin1>é<utf8>Ã©(abc)(def)ghi" *\) in *)
(*   try *)
(*     while true do *)
(*       let r = token enc lexbuf in *)
(*       Printf.printf "%s\n" r *)
(*     done *)
(*   with *)
(*     [ Ulexing.Error -> *)
(* 	Printf.eprintf *)
(* 	"Lexing error at offset %i\n" (Ulexing.lexeme_end lexbuf) *)
(*     | Ulexing.InvalidCodepoint i -> *)
(* 	Printf.eprintf *)
(* 	"Invalid code point %i at offset %i\n" i (Ulexing.lexeme_end lexbuf)]; *)
