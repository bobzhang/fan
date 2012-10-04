module Make (Lexer : Sig.Lexer)
= struct
  module Structure = Structure.Make Lexer;
  module Delete    = Delete.Make    Structure;
  module Insert    = Insert.Make    Structure;
  include Entry.Make Structure;   
  module Fold      = Fold.Make      Structure;
  include Structure;
  let mk () =
    let gkeywords = Hashtbl.create 301 in
    {
      gkeywords = gkeywords;
      gfilter = Token.Filter.mk (Hashtbl.mem gkeywords);
      glexer = Lexer.mk ();
      warning_verbose = ref True; (* FIXME *)
      error_verbose = FanConfig.verbose
    };

  let get_filter g = g.gfilter;

  let lex g loc cs = g.glexer loc cs;

  let lex_string g loc str = lex g loc (Stream.of_string str);

  let filter g ts = Tools.keep_prev_loc (Token.Filter.filter g.gfilter ts);

  let filter_and_parse_tokens entry ts = parse_origin_tokens entry (filter entry.egram ts);

  let parse entry loc cs = filter_and_parse_tokens entry (lex entry.egram loc cs);

  let parse_string entry loc str =
    filter_and_parse_tokens entry (lex_string entry.egram loc str);

  let delete_rule = Delete.delete_rule;

  let srules e rl =
    let t =
      List.fold_left
      (fun tree (symbols, action) -> Insert.insert_tree e symbols action tree)
      DeadEnd rl
    in
    Stree t;
  let sfold0 = Fold.sfold0;
  let sfold1 = Fold.sfold1;
  let sfold0sep = Fold.sfold0sep;
  (* let sfold1sep = Fold.sfold1sep; *)

  let extend = Insert.extend;
end;
