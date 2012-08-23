open Camlp4;                                             (* -*- camlp4r -*- *)
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
 * - Aleksey Nogin: extra features and bug fixes.
 * - Christopher Conway: extra feature (-D<uident>=)
 * - Jean-vincent Loddo: definitions inside IFs.
 *)

(* let module M = Register.OCamlSyntaxExtension Camlp4Parsers.IdMacroParser
 *     Camlp4Parsers.MakeMacroParser in ();
 * let module M = Camlp4.Register.AstFilter Camlp4Parsers.IdMacroParser
 *     Camlp4Parsers.MakeNothing in (); *)
open Camlp4;
open Camlp4Parsers;
pa_m (module Register);
