open Astf
open Compile_stream

val parser_ipat : pat Gramf.t


(* val stream_begin : string option Gramf.t *)

val stream_pat : stream_pats Gramf.t
val stream_pat_comp : spat_comp Gramf.t
val stream_pat_comp_err : stream_pat Gramf.t
val stream_pat_comp_err_list : stream_pats Gramf.t




(* build stream expression *)    

val parser_case : stream_case Gramf.t

val parser_case_list : stream_cases Gramf.t

(* val fill_parsers: unit -> unit     *)

