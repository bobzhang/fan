open Camlp4;                                             (* -*- camlp4r -*- *)
(****************************************************************************)
(*                                                                          *)
(*                                   OCaml                                  *)
(*                                                                          *)
(*                            INRIA Rocquencourt                            *)
(*                                                                          *)
(*  Copyright 2002-2006 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed under   *)
(*  the terms of the GNU Library General Public License, with the special   *)
(*  exception on linking described in LICENSE at the top of the OCaml       *)
(*  source tree.                                                            *)
(*                                                                          *)
(****************************************************************************)

(* Authors:
 * - Nicolas Pouillard: initial version
 *)

open Camlp4;
open Camlp4Parsers;
(* let module Gram = MakeGram Lexer in
 * let module M1 = OCamlInitSyntax.Make Ast Gram Quotation in
 * let module M2 = MakeRevisedParser M1 in
 * let module M3 = MakeParser M2 in
 * let module M3 = MakeQuotationCommon M3 Syntax.AntiquotSyntax  in (); *)

pa_oq (module Register) (module PreCast);
