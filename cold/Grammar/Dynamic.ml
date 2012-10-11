open LibUtil

include Entry

include Structure

let mk =
                                                 fun ()
                                                   ->
                                                  let gkeywords =
                                                   (Hashtbl.create 301) in
                                                  {gkeywords = gkeywords;
                                                   gfilter = (
                                                    (FanToken.Filter.mk (
                                                      (Hashtbl.mem gkeywords)
                                                      )) );
                                                   glexer = (
                                                    (FanLexer.mk () ) );
                                                   warning_verbose = (
                                                    (ref true ) );
                                                   error_verbose =
                                                    FanConfig.verbose}


let get_filter = fun g -> g.gfilter

let lex =
                                      fun g ->
                                       fun loc ->
                                        fun cs -> ((g.glexer) loc cs)


let lex_string =
 fun g -> fun loc -> fun str -> (lex g loc ( (Stream.of_string str) ))


let filter =
 fun g ->
  fun ts ->
   (Tools.keep_prev_loc ( (FanToken.Filter.filter ( g.gfilter ) ts) ))


let filter_and_parse_tokens =
 fun entry ->
  fun ts -> (parse_origin_tokens entry ( (filter ( entry.egram ) ts) ))


let parse =
 fun entry ->
  fun loc ->
   fun cs -> (filter_and_parse_tokens entry ( (lex ( entry.egram ) loc cs) ))


let parse_string =
 fun entry ->
  fun loc ->
   fun str ->
    (filter_and_parse_tokens entry ( (lex_string ( entry.egram ) loc str) ))


let debug_origin_token_stream =
 fun entry ->
  fun tokens ->
   (parse_origin_tokens entry (
     (Stream.map ( fun t -> (t, ghost_token_info) ) tokens) ))

let debug_filtered_token_stream =
                                                                 fun entry ->
                                                                  fun tokens ->
                                                                   (filter_and_parse_tokens
                                                                    entry (
                                                                    (Stream.map
                                                                    (
                                                                    fun t ->
                                                                    (t,
                                                                    FanLoc.ghost)
                                                                    ) tokens)
                                                                    ))


let delete_rule = Delete.delete_rule

let srules =
                                       fun e ->
                                        fun rl ->
                                         let t =
                                          (List.fold_left (
                                            fun tree ->
                                             fun (symbols, action) ->
                                              (Insert.insert_tree e symbols
                                                action tree) ) DeadEnd  rl) in
                                         `Stree (t)

let sfold0 = Fold.sfold0


let sfold1 = Fold.sfold1

let sfold0sep = Fold.sfold0sep

let extend =
                                                           Insert.extend
