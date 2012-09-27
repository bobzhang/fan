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

open FanUtil;
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

  let delete_rule = Delete.delete_rule;

  let srules e rl =
    Stree (List.fold_left (flip (uncurry (Insert.insert_tree e))) DeadEnd rl);
  let sfold0 = Fold.sfold0;
  let sfold1 = Fold.sfold1;
  let sfold0sep = Fold.sfold0sep;
  (* let sfold1sep = Fold.sfold1sep; *)

  let extend = Insert.extend;

end;
