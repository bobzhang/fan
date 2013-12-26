




    
let parse ?(lexer= Lex_fan.from_stream) (entry:'a Gramf.t) loc
    (cs:char Streamf.t) =
  cs
   |>  lexer loc
   |> Gramf.parse_tokens_eoi entry


let parse_string_eoi
    ?lexer
    ?(loc=Locf.string_loc)
    (entry:'a Gramf.t)
    (str:string) =
  str
   |> Streamf.of_string
   |> parse ?lexer entry loc 

(** utilities for parsing *)      

let parse_include_file entry =
  let dir_ok file dir = Sys.file_exists (dir ^ file) in
  fun file ->
    let file =
      try List.find (dir_ok file) ( "./" :: !Configf.include_dirs ) ^ file
      with Not_found -> file  in
    let ch = open_in file in
    let st = Streamf.of_channel ch in
      parse entry (Locf.mk file) st





(* local variables: *)
(* compile-command: "cd .. && pmake main_annot/gramlib.cmo" *)
(* end: *)
