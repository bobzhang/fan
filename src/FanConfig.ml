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

let ocaml_standard_library = Config.standard_library;;

let camlp4_standard_library =
  try Sys.getenv "CAMLP4LIB"
  with Not_found ->
    Filename.concat ocaml_standard_library "camlp4";;

let version = Sys.ocaml_version;;
let program_name = ref "camlp4";;
let unsafe             = ref false;;
let verbose            = ref false;;
let antiquotations     = ref false;;
let quotations         = ref true;;
let inter_phrases: string option ref
    = ref None;;
let camlp4_ast_impl_magic_number = "Camlp42006M002";;
let camlp4_ast_intf_magic_number = "Camlp42006N002";;
let ocaml_ast_intf_magic_number = Config.ast_intf_magic_number;;
let ocaml_ast_impl_magic_number = Config.ast_impl_magic_number;;
let current_input_file = ref "";;

(* new config *)
let bug_main_address = "hongboz@seas.upenn.edu";;

let fan_debug = ref false;;
let conversion_table : (string, string) Hashtbl.t = Hashtbl.create 50

let strict_parsing = ref false;;
(* let strict_parsing = ref true;; *)
      
let strict_parsing_warning = ref false;;


let gram_warning_verbose = ref true;
(* let gram_error_verbose =   *)
