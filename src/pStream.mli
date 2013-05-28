open FAst
open FStreamGen
val parser_ipat : pat Gram.t
val stream_exp_comp : sexp_comp Gram.t
val stream_exp_comp_list : sexp_comp list Gram.t
val stream_begin : string option Gram.t

val stream_pat : stream_pats Gram.t
val stream_pat_comp : spat_comp Gram.t
val stream_pat_comp_err : stream_pat Gram.t
val stream_pat_comp_err_list : stream_pats Gram.t




(* build stream expression *)    
val stream_exp : exp Gram.t    
val parser_case : stream_case Gram.t

val parser_case_list : stream_cases Gram.t
val apply : unit -> unit
