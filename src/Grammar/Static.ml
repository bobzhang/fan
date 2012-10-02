open LibUtil;
open Format;
module Make (Lexer : FanSig.Lexer)
= struct
  module Structure = Structure.Make Lexer;
  module Delete = Delete.Make Structure;
  module Insert = Insert.Make Structure;
  module Fold = Fold.Make Structure;
  module Tools = Tools.Make Structure;
  
  include Structure;

  let gram =
    let gkeywords = Hashtbl.create 301 in
    {
      gkeywords = gkeywords;
      gfilter = Token.Filter.mk (Hashtbl.mem gkeywords);
      glexer = Lexer.mk ();
      warning_verbose = ref True; (* FIXME *)
      error_verbose = FanConfig.verbose
    };

  module Entry = struct
    module E = Entry.Make Structure;
    type t 'a = E.t 'a;
    let mk = E.mk gram;
    let of_parser name strm = E.of_parser gram name strm;
    let setup_parser = E.setup_parser;
    let name = E.name;
    let print = E.print;
    let clear = E.clear;
    let dump = E.dump;

    let obj x = x;
  end;
  let trace_parser = Entry.E.trace_parser;
    
  let get_filter () = gram.gfilter;

  let lex loc cs = gram.glexer loc cs;

  let lex_string loc str = lex loc (Stream.of_string str);

  let filter ts = Tools.keep_prev_loc (Token.Filter.filter gram.gfilter ts);

  let parse_tokens_after_filter entry ts = Entry.E.parse_tokens_after_filter entry ts;

  let parse_tokens_before_filter entry ts = parse_tokens_after_filter entry (filter ts);

  let parse entry loc cs = parse_tokens_before_filter entry (lex loc cs);

  let parse_string entry loc str = parse_tokens_before_filter entry (lex_string loc str);

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
       end 
      ];

  let parse_file_with ~rule file  =
    if Sys.file_exists file then
      let ch = open_in file in
      let st = Stream.of_channel ch in 
      parse rule (FanLoc.mk file) st
    else  failwithf "@[file: %s not found@]@." file;
    
  let delete_rule = Delete.delete_rule;

  let srules e rl =
    Stree (List.fold_left (flip (uncurry (Insert.insert_tree e))) DeadEnd rl);
  let sfold0 = Fold.sfold0;
  let sfold1 = Fold.sfold1;
  let sfold0sep = Fold.sfold0sep;
  (* let sfold1sep = Fold.sfold1sep; *)

  let extend = Insert.extend;

  (* let eoi_entry entry = *)
  (*   let entry_eoi = Entry.(mk (name entry ^ "_eoi")) in   *)
  (*   let () = EXTEND (gram:t) entry_eoi: *)
  (*     [[ x = entry; `EOI -> x ]]; *)
  (*     END in  *)
  (*   entry_eoi; *)

end;
