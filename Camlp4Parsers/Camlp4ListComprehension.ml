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
 * - Nao Hirokawa: initial version
 * - Nicolas Pouillard: revised syntax version
 *)



(* let module M = Register.OCamlSyntaxExtension Camlp4Parsers.IdListComprehension
 *     Camlp4Parsers.MakeListComprehension in (); *)
open Camlp4;
open Camlp4Parsers;
pa_l (module Register);
