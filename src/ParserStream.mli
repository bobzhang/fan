open Ast
open FanStreamTools
val parser_ipat : pat Gram.t
val stream_exp_comp : sexp_comp Gram.t
val stream_exp_comp_list : sexp_comp list Gram.t
val stream_begin : string option Gram.t

val stream_pat : (spat_comp * exp option) list Gram.t
val stream_pat_comp : spat_comp Gram.t
val stream_pat_comp_err : (spat_comp * exp option) Gram.t
val stream_pat_comp_err_list : (spat_comp * exp option) list Gram.t
    
val parser_case :
  ((spat_comp * exp option) list * pat option *
   exp)
  Gram.t

val parser_case_list :
  ((spat_comp * exp option) list * pat option *
   exp)
  list Gram.t
val apply : unit -> unit
