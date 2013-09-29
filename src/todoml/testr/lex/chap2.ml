#filter "trash_nothing";;
open LibUtil;
(* current_directoryhaha*)
let rec translate = with "lex" {|
 | "current_directory" -> begin 
   print_string (Sys.getcwd ());
   translate lexbuf
 end
 | _  ->
     let x = Ulexing.lexeme_char lexbuf 0 in
     let _ = print_char (Char.chr x) in
     translate lexbuf
 | eof  -> begin print_endline "finished"; exit 0 end
|};

let f file =
  let chan = open_in file in
  let lexbuf = Ulexing.from_latin1_channel chan in
  translate lexbuf;
(*

f __FILE__;
*)
let with_latin1_file file  f =
  let chan = open_in file in
  let lexbuf = Ulexing.from_latin1_channel chan in
  finally (fun _ -> close_in chan) f lexbuf;

let num_lines = ref 0;

  
let num_chars = ref 0;

let rec count = with "lex"{|
  | '\n' -> begin
    incr num_lines;
    incr num_chars;
    count lexbuf;
  end
  | _  -> begin
    incr num_chars;
    count lexbuf;
    end
| eof -> ()
|};

let main  = begin 
  with_latin1_file __FILE__  count ;
  Printf.printf "# of lines = %d, # of chars = %d\n" !num_lines !num_chars
end;

{:lex.regexp|
digit:['0'-'9'];
id:['a'-'z']['a'-'z' '0'-'9']*  
|};

let rec toy_lang = with "lex" {|
  |  digit+ ->
      let s =  Ulexing.latin1_lexeme lexbuf in
      let inum = int_of_string <| s in 
      let _ = Format.printf "integer: %s (%d)\n" s inum in
      toy_lang lexbuf
  | digit+ '.' digit*  -> 
      let s = Ulexing.latin1_lexeme lexbuf in
      let inum = float_of_string s in
      let _ = Format.printf "float: %s (%f)\n" s inum in
      toy_lang lexbuf
  | ("if" |"then"|"begin"|"end"|"let"|"in"|"function") ->
      let s = Ulexing.latin1_lexeme lexbuf in
      let _ = Format.printf "keyword: %s\n" s in
      toy_lang lexbuf
  | id  ->
      let s = Ulexing.latin1_lexeme lexbuf in
      let _ = Format.printf "identfier: %s\n" s in 
      toy_lang lexbuf
  | ('+'|'-'|'*'|'/') ->
      let s = Ulexing.latin1_lexeme lexbuf in
      let _ = Format.printf "operator: %s\n" s in
      toy_lang lexbuf
  | ('{' [^ '\n']* '}' | [ ' ' '\t' '\n'] )->
      toy_lang lexbuf
  | _ ->
      toy_lang lexbuf
  | eof ->
      let _ = Format.printf "end" in
      ()
  |};

(* with_latin1_file __FILE__ toy_lang; *)
(*
  32 32.0 if then else
 *)

(* test precedence *)
let token = with "lex" {|
  | "ding"  -> print_endline "Ding"
  | ['a'-'z'] + ->
      let s = Ulexing.latin1_lexeme lexbuf in
      print_endline ("Word: " ^ s)
|};


let _ = begin 
  token <| Ulexing.from_latin1_string "dingdog"; (*dingdog*)
  token <| Ulexing.from_latin1_string "ding"; (*Ding*)
end;

(* FIXME support sub-languages lex.latin1; lex.utf8; lex.utf16
   rec --> f lexbuf ?
 *)
#default_quotation "lex"  ;;


let rec delete_zap_me = with "lex" {|
  |"zap me" ->
      delete_zap_me lexbuf
  | "\"" ->
      (* let _ = Ulexing.rollback lexbuf in *)
      let _ = remove_string lexbuf in
      delete_zap_me lexbuf
  | [' ' '\t'] ->
      let _ = Ulexing.rollback lexbuf in
      let _ = compress lexbuf  in
      delete_zap_me lexbuf
  | "capture" -> (* capture *)
      let (l,a,b)  = Ulexing.line_info lexbuf  in
      let e =
        Format.sprintf
          (* {:str|File "%s", line %d,  |} *)
          "File \"%s\", line %d, chaaracters %d-%d"
          __FILE__ l  a b in
      Format.eprintf "%s capture @." e 
      (* failwithf "(%d,%d)" a b  *)
  | '\n' -> begin
      Ulexing.new_line lexbuf;
      print_char '\n';
      delete_zap_me lexbuf;
  end
  | _  -> let c = Ulexing.latin1_lexeme_char lexbuf 0 in
    begin print_char c; delete_zap_me lexbuf end
  | eof -> ()
|}
and remove_string = with "lex" {|
  | "\""  -> ()
  | '\n' -> begin Ulexing.new_line lexbuf; remove_string lexbuf end
  | ("\\\"" | _ ) -> remove_string lexbuf
  | eof ->   failwithf "\" expected"
|}
and compress = {|
  | [' ' '\t']+ ->  begin print_char ' '; (* compress  lexbuf *) end
  | [' ' '\t']+ '\n' -> (* compress lexbuf *) begin
      Ulexing.new_line lexbuf;
      ()
  end
  | (_|eof) -> failwithf "unexpected in compress"
|};
 
  
with_latin1_file __FILE__ delete_zap_me;
