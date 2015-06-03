let parse ?(lexer= Lex_fan.from_stream)  =
  function
  | (entry : 'a Gramf.t) ->
      (function
       | loc ->
           (function
            | (cs : char Streamf.t) ->
                (cs |> (lexer loc)) |> (Gramf.parse_tokens_eoi entry)))
let parse_string_eoi ?lexer  ?(loc= Locf.string_loc)  =
  function
  | (entry : 'a Gramf.t) ->
      (function
       | (str : string) ->
           (str |> Streamf.of_string) |> (parse ?lexer entry loc))
let parse_include_file =
  function
  | entry ->
      let dir_ok =
        function | file -> (function | dir -> Sys.file_exists (dir ^ file)) in
      (function
       | file ->
           let file =
             try
               (List.find (dir_ok file) ("./" :: (!Configf.include_dirs))) ^
                 file
             with | Not_found  -> file in
           let ch = open_in file in
           let st = Streamf.of_channel ch in parse entry (Locf.mk file) st)
