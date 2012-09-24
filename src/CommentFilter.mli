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
module Make (Token : FanSig.Camlp4Token) : sig
  open Token;

  type t;

  value mk : unit -> t;

  value define : Token.Filter.t -> t -> unit;

  value filter : t -> Stream.t (Token.t * FanLoc.t) -> Stream.t (Token.t * FanLoc.t);

  value take_list : t -> list (string * FanLoc.t);

  value take_stream : t -> Stream.t (string * FanLoc.t);
end;
