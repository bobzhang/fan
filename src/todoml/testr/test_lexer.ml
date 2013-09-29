let rec translate   =
  {:lexer|
  |"current_directory" -> begin
      print_string (Sys.getcwd ());
      translate lexbuf
  end
  | _ as c -> begin
      print_char c;
      translate lexbuf;
  end
  | ! -> exit 0
  |}
;;



(* let _ = *)
(*   let chan = open_in "testr/test_lexer.ml"  in *)
(*   translate (Lexing.from_channel chan) *)

(* (\* #filter "lift"    ;; *\) *)

let num_lines = ref 0
let num_chars = ref 0

let rec count =
  {:lexer|
  | '\n' ->
      (incr num_lines;
       incr num_chars;
       count lexbuf)
  | _ -> (incr num_chars; count lexbuf)
  | ! -> ()
  |}

let _ =
  begin
    (count (Lexing.from_channel (open_in "testr/test_lexer.ml")));
    Format.printf "%d %d\n" !num_lines !num_chars;
  end
;;

{:regexp|
  digit =  ['0'-'9'];
  id = ['a'-'z'] ['a'-'z' '0'-'9']*
|};;


open Printf
let rec toy_lang =
  {:lexer|
  | digit+ as inum ->
      (printf "integer: %s (%d)\n" inum (int_of_string inum);
       toy_lang lexbuf
       )
  | digit+ '.' digit* as fnum -> 
      (printf "float: %s (%f)\n" fnum (float_of_string fnum);
       toy_lang lexbuf
      )
  | "if" | "then" | "else" | "begin" | "end" | "let" | "in"
  | "function" as word ->
      (printf "keyword: %s\n" word ;
       toy_lang lexbuf)
  | id as text ->
      (printf "identifier:%s\n" text; toy_lang lexbuf)
  | '+'|'-'|'*'|'/' as op ->
      (printf "operator: %c\n" op;
       toy_lang lexbuf
      )
  | '{' [! '\n'] * '}' ->
      (toy_lang lexbuf)
  | [ ' ' '\t' '\n'] ->
      (toy_lang lexbuf)
  | _ as c ->
      (printf "Unrecognized character: %c\n" c; toy_lang lexbuf)
  | ! -> ()
  |}
(* 132 23+ 232+ *)    
let  _ =
  toy_lang (Lexing.from_channel (open_in "testr/test_lexer.ml"))
    
