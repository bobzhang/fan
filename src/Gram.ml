open LibUtil;
open Format;
open Grammar;
include Entry;
include Structure;


let default_keywords =
  ["&&"; "functor"; "||"; "private"; "sig"; "include";
   "exception"; "inherit"; "<"; "~"; "and"; "ELSE";
   "when"; ","; "mod"; "then"; "|]"; "initializer";
   "#"; "import"; "!"; "-." ; "_"; ">]" ; "??" ; "in"
     ; "->"; "downto"; "lsr"; "as"; "function"; "begin";
   ".."; ")"; "="; ":"; "|"; "[<"; "ENDIF";
   "class"; "=="; "."; "{<"; "land"; ">}"; "lxor"; "do";
   "end"; "assert"; "external"; "THEN"; "+"; "virtual";
   "to"; "try"; ":>"; "lsl"; "struct"; "else"; "*"; "val"
     ; "IFNDEF"; "constraint"; "type"; "new"; "of";
   "<-"; "done"; "for"; "&"; ";;"; "{"; "fun"; "method"
     ; "'"; ";"; "mutable"; "UNDEF"; "lazy"; "["; "}";
   "[|"; "with"; "[^"; "`"; "::"; "]"; "asr"; "[>";
   ":="; "DEFINE"; "if"; "while"; "IN"; "IFDEF"; "END"
     ; "rec"; "parser"; "object"; "or"; "-"; "("; "match"
     ; "open"; "module"; "INCLUDE"; "?"; ">"; "let"; "lor"; "[="];

let gkeywords = ref (SSet.of_list default_keywords);
  

(* let gfilter  *)



let gram =  {
  annot="Fan";
  gkeywords;
  gfilter =  FanTokenFilter.mk ~is_kwd:(fun x -> SSet.mem x !gkeywords);
};

let filter = FanTokenFilter.filter gram.gfilter;
  
let create_gram ~annot ~keywords () =
  let v = ref (SSet.of_list keywords) in
  {annot;
   gkeywords = v ;
     gfilter = FanTokenFilter.mk ~is_kwd:(fun x -> SSet.mem x !v)
   };


let name_of_entry {ename;_} = ename;  


(* FIXME duplicate some code from Entry *)

let glexer = FanLexUtil.mk ();

(* UNfiltered token stream *)
let lex  loc cs =  glexer loc cs;
let lex_string loc str = lex  loc (XStream.of_string str);
let parse_origin_tokens entry ts = Action.get (action_parse entry ts);


(* filter *)
(* let filter keywords ts = *)
(*   (FanTokenFilter.filter *)
(*      (FanTokenFilter.mk ~is_kwd:(fun x -> SSet.mem x keywords)) ts); *)

(* filtering using the [entrance entry]'s filter '*)
let filter_and_parse_tokens entry ts =
  parse_origin_tokens entry (FanTokenFilter.filter entry.egram.gfilter  ts);

let parse entry loc cs = filter_and_parse_tokens entry (lex loc cs);

let parse_string entry loc str =
  filter_and_parse_tokens entry (lex_string loc str);
  
let mk = mk_dynamic gram;

let of_parser name strm = of_parser gram name strm;

let get_filter () = gram.gfilter;

(* let lex loc cs = gram.glexer loc cs; *)
  
let lex_string loc str = lex loc (XStream.of_string str);
  
(* let filter ts =  FanTokenFilter.filter gram.gfilter ts; *)

let token_stream_of_string s =
  s |>  lex_string FanLoc.string_loc;

  
let parse entry loc cs =
  parse_origin_tokens entry
    (FanTokenFilter.filter entry.egram.gfilter
       (glexer loc cs));

  
let parse_string ?(loc=FanLoc.string_loc) entry  str =
  parse_origin_tokens entry
    (FanTokenFilter.filter entry.egram.gfilter
       (glexer loc (XStream.of_string str)));
  
let debug_origin_token_stream (entry:t 'a) tokens : 'a =
  parse_origin_tokens entry (XStream.map (fun t -> (t,FanLoc.ghost)) tokens);
  
let debug_filtered_token_stream entry tokens =
  filter_and_parse_tokens entry (XStream.map (fun t -> (t,FanLoc.ghost)) tokens);

(* with a special exception handler *)  
let parse_string_safe ?(loc=FanLoc.string_loc) entry  s =
  try
    parse_string entry ~loc s
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
    
(* let parse_file_with ~rule file  = *)
(*   if Sys.file_exists file then *)
(*     let ch = open_in file in *)
(*     let st = XStream.of_channel ch in  *)
(*     parse rule (FanLoc.mk file) st *)
(*   else  failwithf "@[file: %s not found@]@." file; *)
  
let delete_rule = Delete.delete_rule;

(* FIXME [srules] the productions are also scanned  *)  
let srules rl =
    `Stree (List.fold_right Insert.add_production   rl DeadEnd);
    
let sfold0 = Fold.sfold0;
let sfold1 = Fold.sfold1;
let sfold0sep = Fold.sfold0sep;
let sfold1sep = Fold.sfold1sep;
let extend = Insert.extend;
let extend_single = Insert.extend_single;  
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
  |Dlevels levs -> let (_,f,_) = Insert.find_level ?position entry levs in f];





















