open Format
include Gdefs
include Gentry
let mk_action = Gaction.mk
type action = Gaction.t 
let default_keywords =
  ["&&";
  "#";
  "!";
  "-.";
  "-";
  "+";
  "_";
  ">]";
  "??";
  "||";
  "<";
  "~";
  ",";
  "|]";
  "->";
  "..";
  ")";
  "=";
  ":";
  "|";
  "[<";
  "==";
  ".";
  "{<";
  ">}";
  ":>";
  "*";
  "<-";
  "&";
  ";;";
  "{";
  "'";
  ";";
  "[";
  "}";
  "[|";
  "[^";
  "`";
  "::";
  "]";
  "[>";
  ":=";
  "(";
  "?";
  ">";
  "[";
  "functor";
  "private";
  "sig";
  "include";
  "exception";
  "inherit";
  "and";
  "when";
  "then";
  "initializer";
  "in";
  "downto";
  "as";
  "function";
  "begin";
  "class";
  "do";
  "end";
  "assert";
  "external";
  "virtual";
  "to";
  "try";
  "struct";
  "else";
  "val";
  "constraint";
  "type";
  "new";
  "of";
  "done";
  "for";
  "fun";
  "method";
  "mutable";
  "lazy";
  "with";
  "if";
  "while";
  "rec";
  "object";
  "or";
  "match";
  "open";
  "module";
  "let"]
let gkeywords = ref (Setf.String.of_list default_keywords)
let gram =
  {
    annot = "Fan";
    gfilter =
      { kwds = (Setf.String.of_list default_keywords); filter = None }
  }
let filter = Tokenf.filter gram.gfilter
let create_lexer ?(filter= None)  ~annot  ~keywords  () =
  { annot; gfilter = { kwds = (Setf.String.of_list keywords); filter } }
let mk f = mk_dynamic gram f
let get_filter () = gram.gfilter
let token_stream_of_string s = lex_string Locf.string_loc s
let debug_origin_token_stream (entry : 'a t) tokens =
  (parse_origin_tokens entry tokens : 'a )
let debug_filtered_token_stream entry tokens =
  filter_and_parse_tokens entry tokens
let parse_string_safe ?(loc= Locf.string_loc)  entry s =
  try parse_string entry ~loc s
  with
  | Locf.Exc_located (loc,e) ->
      (eprintf "%s" (Printexc.to_string e);
       Locf.error_report (loc, s);
       Locf.raise loc e)
let sfold0 = Gfold.sfold0
let sfold1 = Gfold.sfold1
let sfold0sep = Gfold.sfold0sep
let sfold1sep = Gfold.sfold1sep
let find_level ?position  (entry : Gdefs.entry) =
  let (_,f,_) = Ginsert.find_level ?position entry entry.levels in f
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
      (eprintf "%s" (Printexc.to_string e);
       Locf.error_report (loc, s);
       Locf.raise loc e)
let wrap_stream_parser ?(loc= Locf.mk "<stream>")  p s =
  try p ~loc s
  with
  | Locf.Exc_located (loc,e) ->
      (eprintf "error: %s" (Locf.to_string loc); Locf.raise loc e)
