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

module Id : Sig.Id = struct
  value name = "Camlp4Printers.DumpOCamlAst";
  value version = Sys.ocaml_version;
end;

module Make (Syntax : Sig.Camlp4Syntax)
: (Sig.Printer Syntax.Ast).S
= struct
  module Ast2pt = Struct.Camlp4Ast2OCamlAst.Make Syntax.Ast;

  value print_interf ?(input_file = "-") ?output_file ast =
    let pt = Ast2pt.sig_item ast in
    P4_util.(with_open_out_file
               output_file
               (dump_pt
                  Camlp4_config.ocaml_ast_intf_magic_number input_file pt));

  value print_implem ?(input_file = "-") ?output_file ast =
    let pt = Ast2pt.str_item ast in
    P4_util.(with_open_out_file
               output_file
               (dump_pt Camlp4_config.ocaml_ast_impl_magic_number input_file pt));

end;
