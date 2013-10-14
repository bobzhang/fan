open Format
include Gstructure
include Gentry
include Gstru
module Action = Gaction
let default_keywords =
  ["&&";
  "functor";
  "||";
  "private";
  "sig";
  "include";
  "exception";
  "inherit";
  "<";
  "~";
  "and";
  "when";
  ",";
  "mod";
  "then";
  "|]";
  "initializer";
  "#";
  "!";
  "-.";
  "_";
  ">]";
  "??";
  "in";
  "->";
  "downto";
  "lsr";
  "as";
  "function";
  "begin";
  "..";
  ")";
  "=";
  ":";
  "|";
  "[<";
  "class";
  "==";
  ".";
  "{<";
  "land";
  ">}";
  "lxor";
  "do";
  "end";
  "assert";
  "external";
  "+";
  "virtual";
  "to";
  "try";
  ":>";
  "lsl";
  "struct";
  "else";
  "*";
  "val";
  "constraint";
  "type";
  "new";
  "of";
  "<-";
  "done";
  "for";
  "&";
  ";;";
  "{";
  "fun";
  "method";
  "'";
  ";";
  "mutable";
  "lazy";
  "[";
  "}";
  "[|";
  "with";
  "[^";
  "`";
  "::";
  "]";
  "asr";
  "[>";
  ":=";
  "if";
  "while";
  "rec";
  "parser";
  "object";
  "or";
  "-";
  "(";
  "match";
  "open";
  "module";
  "?";
  ">";
  "let";
  "lor";
  "["]
let gkeywords = ref (Setf.String.of_list default_keywords)
let rec fan_filter (__strm : _ Fstream.t) =
  match Fstream.peek __strm with
  | Some (#Ftoken.space_token,_) -> (Fstream.junk __strm; fan_filter __strm)
  | Some x ->
      (Fstream.junk __strm;
       (let xs = __strm in
        Fstream.icons x (Fstream.slazy (fun _  -> fan_filter xs))))
  | _ -> Fstream.sempty
let rec ignore_layout: Ftoken.filter =
  fun (__strm : _ Fstream.t)  ->
    match Fstream.peek __strm with
    | Some (#Ftoken.space_token,_) ->
        (Fstream.junk __strm; ignore_layout __strm)
    | Some x ->
        (Fstream.junk __strm;
         (let xs = __strm in
          Fstream.icons x (Fstream.slazy (fun _  -> ignore_layout xs))))
    | _ -> Fstream.sempty
let gram =
  {
    annot = "Fan";
    gfilter =
      { kwds = (Setf.String.of_list default_keywords); filter = fan_filter }
  }
let filter = FanTokenFilter.filter gram.gfilter
let create_lexer ?(filter= ignore_layout)  ~annot  ~keywords  () =
  { annot; gfilter = { kwds = (Setf.String.of_list keywords); filter } }
let mk f = mk_dynamic gram f
let of_parser name strm = of_parser gram name strm
let get_filter () = gram.gfilter
let token_stream_of_string s = lex_string FLoc.string_loc s
let debug_origin_token_stream (entry : 'a t) tokens =
  (parse_origin_tokens entry (Fstream.map (fun t  -> (t, FLoc.ghost)) tokens) : 
  'a )
let debug_filtered_token_stream entry tokens =
  filter_and_parse_tokens entry
    (Fstream.map (fun t  -> (t, FLoc.ghost)) tokens)
let parse_string_safe ?(loc= FLoc.string_loc)  entry s =
  try parse_string entry ~loc s
  with
  | FLoc.Exc_located (loc,e) ->
      (eprintf "%s" (Printexc.to_string e);
       FLoc.error_report (loc, s);
       FLoc.raise loc e)
let sfold0 = Gfold.sfold0
let sfold1 = Gfold.sfold1
let sfold0sep = Gfold.sfold0sep
let sfold1sep = Gfold.sfold1sep
let eoi_entry entry =
  let open! Gstru in
    let g = gram_of_entry entry in
    let entry_eoi = mk_dynamic g ((name entry) ^ "_eoi") in
    extend_single (entry_eoi : 'entry_eoi t )
      (None,
        (None, None,
          [([`Snterm (obj (entry : 'entry t ));
            `Stoken
              (((function | `EOI -> true | _ -> false)), (`Vrn "EOI"),
                "`EOI")],
             ("x\n",
               (mk_action
                  (fun (__fan_1 : [> Ftoken.t])  (x : 'entry) 
                     (_loc : FLoc.t)  ->
                     match __fan_1 with
                     | `EOI -> (x : 'entry_eoi )
                     | _ -> failwith "x\n"))))]));
    entry_eoi
let find_level ?position  (entry : Gstructure.entry) =
  match entry.desc with
  | Dparser _ -> invalid_arg "Fgram.find_level"
  | Dlevels levs ->
      let (_,f,_) = Ginsert.find_level ?position entry levs in f
let parse_include_file entry =
  let dir_ok file dir = Sys.file_exists (dir ^ file) in
  fun file  ->
    let file =
      try
        (List.find (dir_ok file) ("./" :: (Configf.include_dirs.contents))) ^
          file
      with | Not_found  -> file in
    let ch = open_in file in
    let st = Fstream.of_channel ch in parse entry (FLoc.mk file) st
let error_report (loc,s) =
  prerr_endline (FLoc.to_string loc);
  (let (start_bol,stop_bol,start_off,stop_off) =
     let open FLoc in
       ((start_bol loc), (stop_bol loc), (start_off loc), (stop_off loc)) in
   let abs_start_off = start_bol + start_off in
   let abs_stop_off = stop_bol + stop_off in
   let err_location =
     String.sub s abs_start_off ((abs_stop_off - abs_start_off) + 1) in
   prerr_endline (sprintf "err: ^%s^" err_location))
let parse_string_of_entry ?(loc= FLoc.mk "<string>")  entry s =
  try parse_string entry ~loc s
  with
  | FLoc.Exc_located (loc,e) ->
      (eprintf "%s" (Printexc.to_string e);
       error_report (loc, s);
       FLoc.raise loc e)
let wrap_stream_parser ?(loc= FLoc.mk "<stream>")  p s =
  try p ~loc s
  with
  | FLoc.Exc_located (loc,e) ->
      (eprintf "error: %s" (FLoc.to_string loc); FLoc.raise loc e)