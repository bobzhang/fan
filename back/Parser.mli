(* camlp4r *)
(****************************************************************************)
(*                                                                          *)
(*                                   OCaml                                  *)
(*                                                                          *)
(*                            INRIA Rocquencourt                            *)
(*                                                                          *)
(*  Copyright  2007   Institut National de Recherche  en  Informatique et   *)
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

module Make (Structure : Structure.S) : sig
  open Structure;
  val add_loc :
    FanLoc.t -> (token_stream -> 'b) -> token_stream -> ('b * FanLoc.t);
  val level_number : internal_entry -> string -> int;
  val strict_parsing : ref bool;
  val strict_parsing_warning : ref bool;
  val top_symb :
    internal_entry -> symbol -> symbol;
  val top_tree :
    internal_entry -> tree -> tree;
  val entry_of_symb :
    internal_entry -> symbol -> internal_entry;
  val continue :
    internal_entry -> FanLoc.t -> Action.t -> symbol -> tree -> efun -> efun;
  val do_recover :
    (internal_entry -> 'a -> 'b -> tree -> efun) -> internal_entry ->
    'a -> 'b -> FanLoc.t -> Action.t -> symbol -> tree -> efun;
  val recover :
    (internal_entry -> 'a -> 'b -> tree -> efun) -> internal_entry ->
    'a -> 'b -> FanLoc.t -> Action.t -> symbol -> tree -> efun;
  val parser_of_tree :
    internal_entry -> int -> int -> tree -> efun;
  val parser_cont :
    efun -> internal_entry -> int -> int -> symbol -> tree -> FanLoc.t -> Action.t -> efun;
  val parser_of_token_list :
    (FanLoc.t -> Action.t -> efun) -> list symbol -> efun;
  val parser_of_symbol :
    internal_entry -> int -> symbol -> efun;
  val parse_top_symb :
    internal_entry -> symbol -> efun;
  val start_parser_of_levels :
    internal_entry -> int -> list level -> int -> efun;
  val start_parser_of_entry :
    internal_entry -> int -> efun;
  val continue_parser_of_levels :
    internal_entry -> int -> list level -> int -> FanLoc.t -> 'a -> efun;
  val continue_parser_of_entry :
    internal_entry -> int -> FanLoc.t -> Action.t -> efun;
end;
