open Camlp4;                                             (* -*- camlp4r -*- *)
(****************************************************************************)
(*                                                                          *)
(*                                   OCaml                                  *)
(*                                                                          *)
(*                            INRIA Rocquencourt                            *)
(*                                                                          *)
(*  Copyright  2007  Institut  National  de  Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed under   *)
(*  the terms of the GNU Library General Public License, with the special   *)
(*  exception on linking described in LICENSE at the top of the OCaml       *)
(*  source tree.                                                            *)
(*                                                                          *)
(****************************************************************************)

(* Authors:
 * - Nicolas Pouillard: Original version
 *)


(* let module M = Register.OCamlSyntaxExtension Camlp4Parsers.IdReloadedParser
 *     Camlp4Parsers.MakeReloadedParser in (); *)
open Camlp4;
open Camlp4Parsers;
pa_rr (module Register);
