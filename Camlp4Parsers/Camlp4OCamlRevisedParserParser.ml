open Camlp4;                                        (* -*- camlp4r -*- *)
(****************************************************************************)
(*                                                                          *)
(*                                   OCaml                                  *)
(*                                                                          *)
(*                            INRIA Rocquencourt                            *)
(*                                                                          *)
(*  Copyright 1998-2006 Institut National de Recherche en Informatique et   *)
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



(* module M = Register.OCamlSyntaxExtension Camlp4Parsers.IdRevisedParserParser
 *     Camlp4Parsers.MakeRevisedParserParser; *)
open Camlp4;
open Camlp4Parsers;
pa_rp (module Register);
