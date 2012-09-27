(* camlp4r *)
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

module Make (Structure : Structure.S) : sig
  open Structure;

  val sfold0 : ('a -> 'b -> 'b) -> 'b -> fold _ 'a 'b;
  val sfold1 : ('a -> 'b -> 'b) -> 'b -> fold _ 'a 'b;
  val sfold0sep : ('a -> 'b -> 'b) -> 'b -> foldsep _ 'a 'b;
  (* value sfold1sep : ('a -> 'b -> 'b) -> 'b -> foldsep _ 'a 'b; *)
end;
