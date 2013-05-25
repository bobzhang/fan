open FanGrammar
open FAst
val print_warning : FanLoc.t -> string -> unit

val prefix : string

val grammar_module_name : vid ref
val gm : unit -> vid
val mk_entry :
  name:name ->
  pos:exp option -> levels:levels -> entry
val mk_level :
  label:string option ->
  assoc:exp option -> rules:rule list -> level
val mk_rule :
  prod:symbol list -> action:exp option -> rule
val mk_symbol :
  ?pattern:pat option ->
  text:text -> styp:styp -> symbol
val string_of_pat : pat -> string

val check_not_tok : symbol -> unit
    
val new_type_var: unit -> string

val gensym: unit -> int ref
val gen_lid: unit -> string
    
val retype_rule_list_without_patterns:  loc -> rule list -> rule list

    
val make_ctyp : styp -> string -> ctyp

val text_of_action :
  loc ->
  symbol list -> ?action:exp -> string   -> string -> exp
val mk_srules :
  loc ->
  string ->
  rule list -> string -> (text list * exp) list
val make_exp : (* name -> *) string -> text -> exp
val make_exp_rules :
    loc -> (* name -> *) (text list * exp) list -> string -> exp

(* val exp_of_delete_rule : *)
val exp_delete_rule:
  loc -> name -> symbol list list  -> exp
      
val mk_name : loc -> (* ident *)vid -> name
val mk_slist :
  loc ->
  bool -> symbol option -> symbol -> text
val text_of_entry :   entry -> exp
val let_in_of_extend :
  loc ->
  (* ident *)vid option -> name list option -> exp -> exp
val text_of_functorial_extend :
  loc ->
  (* ident *)vid option ->
  name list option -> entry list -> exp
val mk_tok :
  loc ->
  ?restrict:exp ->
  pattern:pat -> styp -> symbol

(* val sfold : *)
(*   ?sep:symbol -> *)
(*   loc -> *)
(*   string list -> *)
(*   exp -> exp -> symbol -> symbol *)
