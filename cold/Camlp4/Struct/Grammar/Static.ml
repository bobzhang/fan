let uncurry = fun f -> fun (x, y) -> (f x y)

let flip =
                                               fun f ->
                                                fun x -> fun y -> (f y x)


module Make =
 functor (Lexer : Sig.Lexer) ->
  (struct
    module Structure = (Structure.Make)(Lexer)

    module Delete = (Delete.Make)(Structure)

    module Insert = (Insert.Make)(Structure)

    module Fold = (Fold.Make)(Structure)

    module Tools = (Tools.Make)(Structure)

    include Structure

    let gram =
     let gkeywords = (Hashtbl.create 301) in
     {gkeywords = gkeywords;
      gfilter = ( (Token.Filter.mk ( (Hashtbl.mem gkeywords) )) );
      glexer = ( (Lexer.mk () ) ); warning_verbose = ( (ref true ) );
      error_verbose = Camlp4_config.verbose}

    module Entry =
     struct
      module E = (Entry.Make)(Structure)

      type 'a t = 'a E.t

      let mk = (E.mk gram)

      let of_parser = fun name -> fun strm -> (E.of_parser gram name strm)

      let setup_parser = E.setup_parser

      let name = E.name

      let print = E.print

      let clear = E.clear

      let dump = E.dump

      let obj = fun x -> x

     end

    let trace_parser = Entry.E.trace_parser

    let get_filter = fun ()  -> gram.gfilter

    let lex = fun loc -> fun cs -> ((gram.glexer) loc cs)

    let lex_string =
     fun loc -> fun str -> (lex loc ( (Stream.of_string str) ))

    let filter =
     fun ts ->
      (Tools.keep_prev_loc ( (Token.Filter.filter ( gram.gfilter ) ts) ))

    let parse_tokens_after_filter =
     fun entry -> fun ts -> (Entry.E.parse_tokens_after_filter entry ts)

    let parse_tokens_before_filter =
     fun entry -> fun ts -> (parse_tokens_after_filter entry ( (filter ts) ))

    let parse =
     fun entry ->
      fun loc ->
       fun cs -> (parse_tokens_before_filter entry ( (lex loc cs) ))

    let parse_string =
     fun entry ->
      fun loc ->
       fun str -> (parse_tokens_before_filter entry ( (lex_string loc str) ))

    let delete_rule = Delete.delete_rule

    let srules =
     fun e ->
      fun rl ->
       (Stree
         (List.fold_left ( (flip ( (uncurry ( (Insert.insert_tree e) )) )) )
           DeadEnd  rl))

    let sfold0 = Fold.sfold0

    let sfold1 = Fold.sfold1

    let sfold0sep = Fold.sfold0sep

    let extend = Insert.extend

   end :
    (Sig.Grammar.Static with module Loc = Lexer.Loc and module Loc =
     Lexer.Loc and module Token = Lexer.Token))
