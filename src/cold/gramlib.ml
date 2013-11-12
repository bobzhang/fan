let lex_string loc str = Flex_lib.from_stream loc (Streamf.of_string str)
let parse_string ?(lexer= Flex_lib.from_stream)  ?(loc= Locf.string_loc) 
  (entry : 'a Gramf.t) str =
  (((str |> Streamf.of_string) |> (lexer loc)) |>
     (Tokenf.filter (Gramf.filter_of_gram entry)))
    |> (Gramf.parse_origin_tokens entry)
let parse (entry : 'a Gramf.t) loc cs =
  Gramf.parse_origin_tokens entry
    (Tokenf.filter (Gramf.filter_of_gram entry) (Flex_lib.from_stream loc cs))
let token_stream_of_string s = lex_string Locf.string_loc s
let parse_include_file entry =
  let dir_ok file dir = Sys.file_exists (dir ^ file) in
  fun file  ->
    let file =
      try (List.find (dir_ok file) ("./" :: (!Configf.include_dirs))) ^ file
      with | Not_found  -> file in
    let ch = open_in file in
    let st = Streamf.of_channel ch in parse entry (Locf.mk file) st
let parse_string_of_entry ?(loc= Locf.mk "<string>")  entry s =
  try parse_string entry ~loc s
  with
  | Locf.Exc_located (loc,e) ->
      (Format.eprintf "%s" (Printexc.to_string e);
       Locf.error_report (loc, s);
       Locf.raise loc e)
let eoi_entry entry =
  let g = Gramf.gram_of_entry entry in
  let entry_eoi = Gramf.mk_dynamic g ((Gramf.name entry) ^ "_eoi") in
  Gramf.extend_single (entry_eoi : 'entry_eoi Gramf.t )
    (None,
      ((None, None,
         [{
            symbols =
              [Nterm (Gramf.obj (entry : 'entry Gramf.t ));
              Token
                ({ descr = { tag = `EOI; word = Any; tag_name = "EOI" } } : 
                Tokenf.pattern )];
            annot = "x\n";
            fn =
              (Gramf.mk_action
                 (fun ~__fan_1:_  ~__fan_0:(x : 'entry)  (_loc : Locf.t)  ->
                    (x : 'entry_eoi )))
          }]) : Gramf.olevel ));
  entry_eoi
