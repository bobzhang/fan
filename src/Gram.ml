open LibUtil;
open Format;
open Grammar;
include Entry;
include Structure;

let gram =
  let gkeywords = Hashtbl.create 301 in {
  gkeywords = gkeywords;
  gfilter = FanTokenFilter.mk ~is_kwd:(Hashtbl.mem gkeywords);
  glexer = FanLexUtil.mk ();
};
  
let create_gram () =
  let gkeywords = Hashtbl.create 301 in {
  gkeywords = gkeywords;
  gfilter = FanTokenFilter.mk ~is_kwd:(Hashtbl.mem gkeywords);
  glexer = FanLexUtil.mk ();
};

(* let copy {egram:}   *)

(* FIXME duplicate some code from Entry *)
  
let mk = mk_dynamic gram;

let of_parser name strm = of_parser gram name strm;

let get_filter () = gram.gfilter;

let lex loc cs = gram.glexer loc cs;
  
let lex_string loc str = lex loc (XStream.of_string str);
  
let filter ts =  FanTokenFilter.filter gram.gfilter ts;

let token_stream_of_string s =  s |> lex_string FanLoc.string_loc |> filter;
  
(* let filter_and_parse_tokens entry ts = parse_origin_tokens entry (filter ts); *)
  

let parse entry loc cs =
  let lexer = entry.egram.glexer in
  let filter = entry.egram.gfilter in
  let filter ts =  FanTokenFilter.filter filter ts in
  parse_origin_tokens entry (filter (lexer loc cs))
  (* filter_and_parse_tokens entry (lex loc cs) *);
  
let parse_string entry loc str =
  let lexer = entry.egram.glexer in
  let filter = entry.egram.gfilter in
  let filter ts = FanTokenFilter.filter filter ts in
  parse_origin_tokens entry (filter (lexer loc (XStream.of_string str)))
  (* filter_and_parse_tokens entry *)
  (*   (entry.egram.glexer loc cs) *)
(* (lex_string loc str) *);
  
let debug_origin_token_stream (entry:t 'a) tokens : 'a =
  parse_origin_tokens entry (XStream.map (fun t -> (t,FanLoc.ghost)) tokens);
  
let debug_filtered_token_stream entry tokens =
  filter_and_parse_tokens entry (XStream.map (fun t -> (t,FanLoc.ghost)) tokens);

(* with a special exception handler *)  
let parse_string_safe entry loc s =
  try
    parse_string entry loc s
  with
    [FanLoc.Exc_located(loc, e) -> begin
      eprintf "%s" (Printexc.to_string e);
      FanLoc.error_report (loc,s);
      FanLoc.raise loc e ;
    end ];
    
let wrap_stream_parser  p loc s =
  try p loc s
  with
    [FanLoc.Exc_located(loc,e) -> begin
      eprintf "error: %s@." (FanLoc.to_string loc) ;
      FanLoc.raise loc e;
    end ];
    
let parse_file_with ~rule file  =
  if Sys.file_exists file then
    let ch = open_in file in
    let st = XStream.of_channel ch in 
    parse rule (FanLoc.mk file) st
  else  failwithf "@[file: %s not found@]@." file;
  
let delete_rule = Delete.delete_rule;

let srules e rl =
    `Stree (List.fold_right (Insert.insert_production_in_tree e)  rl DeadEnd);
    
let sfold0 = Fold.sfold0;
let sfold1 = Fold.sfold1;
let sfold0sep = Fold.sfold0sep;
  (* let sfold1sep = Fold.sfold1sep; *)
let extend = Insert.extend;
let levels_of_entry = Insert.levels_of_entry;
  
let eoi_entry entry =
  let g = gram_of_entry entry in 
  let entry_eoi = (mk_dynamic g (name entry ^ "_eoi")) in
  begin
    {:extend| entry_eoi: [  entry{x}; `EOI -> x ] |} ;
    entry_eoi
  end;

let find_level ?position entry =
  match entry.edesc with
  [Dparser _ -> invalid_arg "Gram.find_level"
  |Dlevels levs ->
      let (_,f,_) = Insert.find_level ?position entry levs in
      f None None
  ];





















