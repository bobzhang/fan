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
 * - Nicolas Pouillard: initial version
 *)

open Format;

module Id = struct
  value name = "Camlp4.Printers.OCaml";
  value version = Sys.ocaml_version;
end;

module Make (Syntax : Sig.Camlp4Syntax) : (Sig.Printer Syntax.Ast).S = struct
  (* module Ast2pt = Struct.Camlp4Ast2OCamlAst.Make Syntax.Ast; *)
  value print_implem ?input_file:(_) ?output_file ast =
    let pt = Syntax.Ast2pt.str_item ast in
    FanUtil.with_open_out_file output_file
      (fun oc ->
        let fmt = Format.formatter_of_out_channel oc in
        let () = Pprintast.print_structure fmt pt in 
        pp_print_flush fmt ();
      );
  value print_interf ?input_file:(_) ?output_file ast =
    let pt = Syntax.Ast2pt.sig_item ast in
    FanUtil.with_open_out_file output_file
      (fun oc ->
        let fmt = Format.formatter_of_out_channel oc in
        let () = Pprintast.print_signature fmt pt in
        pp_print_flush fmt ();
      );
end;
