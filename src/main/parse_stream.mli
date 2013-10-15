open FAst
open Compile_stream

val parser_ipat : pat Fgram.t


(* val stream_begin : string option Fgram.t *)

val stream_pat : stream_pats Fgram.t
val stream_pat_comp : spat_comp Fgram.t
val stream_pat_comp_err : stream_pat Fgram.t
val stream_pat_comp_err_list : stream_pats Fgram.t




(* build stream expression *)    

val parser_case : stream_case Fgram.t

val parser_case_list : stream_cases Fgram.t

(* val fill_parsers: unit -> unit     *)

