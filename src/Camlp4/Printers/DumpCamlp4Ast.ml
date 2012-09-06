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

module Id = struct
  value name = "Camlp4Printers.DumpCamlp4Ast";
  value version = Sys.ocaml_version;
end;

module Make (Syntax : Sig.Syntax)
: (Sig.Printer Syntax.Ast).S
= struct
  value print_interf ?input_file:(_) ?output_file ast =
    FanUtil.(with_open_out_file output_file
               (dump_ast FanConfig.camlp4_ast_intf_magic_number ast));

  value print_implem ?input_file:(_) ?output_file ast =
    FanUtil.(with_open_out_file output_file
               (dump_ast FanConfig.camlp4_ast_impl_magic_number ast));
end;
