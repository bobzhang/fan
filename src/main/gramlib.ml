
let lex_string loc str = Lex_fan.from_stream  loc (Streamf.of_string str)



let parse_string_eoi ?(lexer=Lex_fan.from_stream) ?(loc=Locf.string_loc) (entry:'a Gramf.t)
    str =
  str
   |> Streamf.of_string
   |> lexer loc
   |> Gramf.parse_tokens_eoi entry
    
let parse ?(lexer= Lex_fan.from_stream) (entry:'a Gramf.t) loc cs =
  (* Gramf.parse_origin_tokens entry @@ lexer loc cs  *)
  Gramf.parse_tokens_eoi entry @@ lexer loc cs 
;;

let token_stream_of_string s =
    lex_string Locf.string_loc s

(** utilities for parsing *)      
let parse_include_file entry =
  let dir_ok file dir = Sys.file_exists (dir ^ file) in
  fun file ->
    let file =
      try (List.find (dir_ok file) ( "./" :: !Configf.include_dirs )) ^ file
      with | Not_found -> file  in
    let ch = open_in file in
    let st = Streamf.of_channel ch in
      parse entry (Locf.mk file) st

let parse_string_of_entry ?(loc=Locf.mk "<string>") entry  s =
  try parse_string_eoi entry  ~loc s  with
    Locf.Exc_located(loc, e) -> begin
      Format.eprintf "%s" (Printexc.to_string e);
      Locf.error_report (loc,s);
      Locf.raise loc e ;
  end


(* (\* [eoi_entry] could be improved   *\) *)
(* let eoi_entry entry = *)
(*   let entry_eoi = (Gramf.mk (Gramf.name entry ^ "_eoi")) in *)
(*   begin *)
(*     %extend{ entry_eoi: [  entry as x; EOI %{x} ] } ; *)
(*     entry_eoi *)
(*   end *)



(* local variables: *)
(* compile-command: "cd .. && pmake main_annot/gramlib.cmo" *)
(* end: *)
