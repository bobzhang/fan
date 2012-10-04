(****************************************************************************)
(*                                                                          *)
(*                                   OCaml                                  *)
(*                                                                          *)
(*                            INRIA Rocquencourt                            *)
(*                                                                          *)
(*  Copyright  2006   Institut National de Recherche  en  Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed under   *)
(*  the terms of the GNU Library General Public License, with the special   *)
(*  exception on linking described in LICENSE at the top of the OCaml       *)
(*  source tree.                                                            *)
(*                                                                          *)
(****************************************************************************)

(* Authors:
 * - Daniel de Rauglaudre: initial version
 * - Nicolas Pouillard: refactoring
 *)
module Make (Lexer : Sig.Lexer)
= struct
  module Structure = Structure.Make Lexer;
  module Delete    = Delete.Make    Structure;
  module Insert    = Insert.Make    Structure;
  module Entry     = Entry.Make     Structure;
  module Fold      = Fold.Make      Structure;
  module Tools     = Tools.Make     Structure;

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

  let parse_tokens_after_filter entry ts = Entry.parse_tokens_after_filter entry ts;

  let parse_tokens_before_filter entry ts = parse_tokens_after_filter entry (filter entry.egram ts);

  let parse entry loc cs = parse_tokens_before_filter entry (lex entry.egram loc cs);

  let parse_string entry loc str =
    parse_tokens_before_filter entry (lex_string entry.egram loc str);

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
