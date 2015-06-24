let rec filter fmt = %lex{
| '[' ['0'-'9']+ ',' ['0'-'9']+ '+' ['0'-'9']+ ']'   %{
  begin 
    Format.pp_print_string fmt "[]"; filter fmt lexbuf
  end
}
| _ as c %{ 
  begin 
    Format.pp_print_char fmt c; filter fmt lexbuf
  end
  }
| eof %{()}
}


(* let () =  *)
(*   (filter Format.std_formatter @@ Lexing.from_string "(xx.ml[1,0+0]..[1,0+12])") *)

let () = 
  let in_chan = stdin in
  let lex_buf = Lexing.from_channel in_chan in
  let () = filter Format.std_formatter lex_buf in
  close_in in_chan
(* local variables: *)
(* compile-command: "ocamlbuild  -cflags '-dsource -w -40'  -pp fan  diff_filter.byte " *)
(* end: *)
