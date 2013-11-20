
(** [stream] DDSL compiler, original Camlp4's stream extension *)
open Astf

type spat_comp =
  | When of Locf.t * pat * exp option
  | Match of Locf.t * pat * exp
  | Str of Locf.t * pat
type sexp_comp =
  | Trm of Locf.t * exp
  | Ntr of Locf.t * exp


type stream_pat = (spat_comp * exp option)
type stream_pats = stream_pat list
type stream_case = (stream_pats * pat option * exp)      
type stream_cases = stream_case list
      
val grammar_module_name : string ref
val gm : unit -> string
val strm_n : string
val peek_fun : loc -> exp
val junk_fun : loc -> exp
val empty : loc -> exp


val handle_failure : exp -> bool
val is_constr_apply : exp -> bool

val stream_pattern_component : exp -> exp -> spat_comp -> exp

val stream_pattern : loc -> stream_case -> (exp option -> exp) -> exp

val stream_patterns_term :
  loc ->
  (unit -> exp) ->
  (pat * exp option * loc * stream_pats * pat option * exp) list -> exp

val parser_cases :  loc ->  stream_cases ->  exp

val cparser :  loc  -> stream_cases ->  exp

(* val cparser_match :  loc ->  exp ->  pat option -> stream_cases ->  exp *)
    
val not_computing : exp -> bool



val slazy : loc -> exp -> exp

val cstream : loc -> sexp_comp list -> exp
