module Make =
 functor (Lexer : Sig.Lexer) ->
  (struct
    module Structure = (Structure.Make)(Lexer)

    module Delete = (Delete.Make)(Structure)

    module Insert = (Insert.Make)(Structure)

    module Entry = (Entry.Make)(Structure)

    module Fold = (Fold.Make)(Structure)

    module Tools = (Tools.Make)(Structure)

    include Structure

    let mk =
     fun ()
       ->
      let gkeywords = (Hashtbl.create 301) in
      {gkeywords = gkeywords;
       gfilter = ( (Token.Filter.mk ( (Hashtbl.mem gkeywords) )) );
       glexer = ( (Lexer.mk () ) ); warning_verbose = ( (ref true ) );
       error_verbose = FanConfig.verbose}

    let get_filter = fun g -> g.gfilter

    let lex = fun g -> fun loc -> fun cs -> ((g.glexer) loc cs)

    let lex_string =
     fun g -> fun loc -> fun str -> (lex g loc ( (Stream.of_string str) ))

    let filter =
     fun g ->
      fun ts ->
       (Tools.keep_prev_loc ( (Token.Filter.filter ( g.gfilter ) ts) ))

    let parse_tokens_after_filter =
     fun entry -> fun ts -> (Entry.parse_tokens_after_filter entry ts)

    let parse_tokens_before_filter =
     fun entry ->
      fun ts ->
       (parse_tokens_after_filter entry ( (filter ( entry.egram ) ts) ))

    let parse =
     fun entry ->
      fun loc ->
       fun cs ->
        (parse_tokens_before_filter entry ( (lex ( entry.egram ) loc cs) ))

    let parse_string =
     fun entry ->
      fun loc ->
       fun str ->
        (parse_tokens_before_filter entry (
          (lex_string ( entry.egram ) loc str) ))

    let delete_rule = Delete.delete_rule

    let srules =
     fun e ->
      fun rl ->
       let t =
        (List.fold_left (
          fun tree ->
           fun (symbols, action) ->
            (Insert.insert_tree e symbols action tree) ) DeadEnd  rl) in
       (Stree (t))

    let sfold0 = Fold.sfold0

    let sfold1 = Fold.sfold1

    let sfold0sep = Fold.sfold0sep

    let extend = Insert.extend

   end :
    (Sig.Grammar.Dynamic with module Loc = Lexer.Loc and module Loc =
     Lexer.Loc and module Token = Lexer.Token))
