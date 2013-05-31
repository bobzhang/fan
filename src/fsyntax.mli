
(** Fan's core nonterminals exported *)  

open FAst

type warning = FLoc.t -> string -> unit

val default_warning : warning

val current_warning : warning ref 

val print_warning : warning


val interf : (sigi list  * FLoc.t option ) Fgram.t 

val implem : (stru list  * FLoc.t option ) Fgram.t 

val top_phrase : stru option Fgram.t 

val a_lident: alident  Fgram.t  (* alident *)
val a_uident: auident Fgram.t
val aident: ident Fgram.t
val astr: astring Fgram.t
val luident: string Fgram.t
val uident: uident Fgram.t  
    
    
val amp_ctyp : ctyp Fgram.t
val and_ctyp : ctyp Fgram.t
val case : case Fgram.t
val case0 : case Fgram.t
val case_quot : case Fgram.t
val bind : bind Fgram.t
val bind_quot : bind Fgram.t
val rec_exp_quot : rec_exp Fgram.t
val class_declaration : cldecl Fgram.t
val class_description : cltdecl Fgram.t
val clexp : clexp Fgram.t
val clexp_quot : clexp Fgram.t
val class_fun_bind : clexp Fgram.t
val class_fun_def : clexp Fgram.t
val class_info_for_cltyp : cltyp Fgram.t
val class_longident : ident Fgram.t
val clsigi : clsigi Fgram.t
val clsigi_quot : clsigi Fgram.t
val class_signature : clsigi Fgram.t
val clfield : clfield Fgram.t
val clfield_quot : clfield Fgram.t
val class_structure : clfield Fgram.t
val cltyp : cltyp Fgram.t
val cltyp_declaration : cltdecl Fgram.t
val cltyp_longident : ident Fgram.t

val cltyp_plus : cltyp Fgram.t
val cltyp_quot : cltyp Fgram.t
val comma_ctyp : type_parameters Fgram.t

val vid: vid Fgram.t  
val comma_exp : exp Fgram.t
val comma_ipat : pat Fgram.t
val comma_pat : pat Fgram.t
val comma_type_parameter : type_parameters Fgram.t
val constrain : type_constr Fgram.t
val constructor_arg_list : ctyp Fgram.t
val constructor_declaration : of_ctyp Fgram.t
val constructor_declarations : or_ctyp Fgram.t
val ctyp : ctyp Fgram.t
val ctyp_quot : ctyp Fgram.t
val cvalue_bind : exp Fgram.t
val flag : flag Fgram.t
val direction_flag_quot : flag Fgram.t
val dummy : unit Fgram.t
val eq_exp : (alident -> pat -> pat) Fgram.t 
val exp : exp Fgram.t
val exp_eoi : exp Fgram.t
val exp_quot : exp Fgram.t
val field_exp : rec_exp Fgram.t
val field_exp_list : rec_exp Fgram.t
val fun_bind : exp Fgram.t
val fun_def : exp Fgram.t
val ident : ident Fgram.t
val ident_quot : ident Fgram.t
val ipat : pat Fgram.t
val ipat_tcon : pat Fgram.t
val pat_tcon : pat Fgram.t    

val label_declaration : name_ctyp Fgram.t
val label_declaration_list : name_ctyp Fgram.t
val label_exp : rec_exp Fgram.t
val label_exp_list : rec_exp Fgram.t
val label_longident : ident Fgram.t

val label_pat : rec_pat Fgram.t
val label_pat_list : rec_pat Fgram.t
    
val let_bind : bind Fgram.t
val meth_list : (name_ctyp * flag) Fgram.t 
val meth_decl : name_ctyp Fgram.t
val mbind : mbind Fgram.t
val mbind0 : mexp Fgram.t
val mbind_quot : mbind Fgram.t
val module_declaration : mtyp Fgram.t
val mexp : mexp Fgram.t
val mexp_quot : mexp Fgram.t
val module_longident : vid Fgram.t
val module_longident_with_app : ident Fgram.t
val module_rec_declaration : mbind Fgram.t
val mtyp : mtyp Fgram.t
val mtyp_quot : mtyp Fgram.t
val name_tags : tag_names Fgram.t
val opt_class_self_type : ctyp Fgram.t

val opt_dot_dot : flag Fgram.t
val row_var_flag_quot : flag Fgram.t
val opt_meth_list : ctyp Fgram.t
val opt_mutable : flag Fgram.t
val mutable_flag_quot : flag Fgram.t
val opt_override : flag Fgram.t
val override_flag_quot : flag Fgram.t
val opt_private : flag Fgram.t
val private_flag_quot : flag Fgram.t
val opt_rec : flag Fgram.t
val rec_flag_quot : flag Fgram.t
val opt_virtual : flag Fgram.t
val virtual_flag_quot : flag Fgram.t
val pat : pat Fgram.t
val pat_as_pat_opt : pat Fgram.t
val pat_eoi : pat Fgram.t
val pat_quot : pat Fgram.t
val row_field : row_field Fgram.t
val sem_exp : exp Fgram.t
val sem_exp_for_list : (exp -> exp) Fgram.t 
val sem_pat : pat Fgram.t
val sem_pat_for_list :  (pat -> pat) Fgram.t
val sequence : exp Fgram.t
val sigi : sigi Fgram.t
val sigi_quot : sigi Fgram.t
val sigis : sigi Fgram.t

val star_ctyp: ctyp Fgram.t
val com_ctyp: ctyp Fgram.t
val stru : stru Fgram.t
val stru_quot : stru Fgram.t
val strus : stru Fgram.t
val type_declaration : typedecl Fgram.t
val type_ident_and_parameters : (alident * opt_decl_params) Fgram.t 
val type_info: type_info Fgram.t
val type_repr: type_repr Fgram.t
    
val type_longident : ident Fgram.t
val type_longident_and_parameters : ctyp Fgram.t
val type_parameter : decl_param Fgram.t
val type_parameters : (ctyp -> ctyp) Fgram.t 
val typevars : ctyp Fgram.t
val val_longident : ident Fgram.t
val constr : constr Fgram.t
val constr_quot : constr Fgram.t
val prefixop : exp Fgram.t
val infixop0 : exp Fgram.t
val infixop1 : exp Fgram.t
val infixop2 : exp Fgram.t
val infixop3 : exp Fgram.t
val infixop4 : exp Fgram.t

val string_list: strings Fgram.t
val infixop5: exp Fgram.t
val infixop6: exp Fgram.t
val module_longident_dot_lparen: ident Fgram.t
val sequence': (exp -> exp) Fgram.t 
val fun_def: exp Fgram.t 

val method_opt_override: flag Fgram.t
val value_val_opt_override: flag Fgram.t
val unquoted_typevars:ctyp Fgram.t
val lang: FToken.name Fgram.t
val with_exp_lang: exp Fgram.t  
val with_stru_lang: stru Fgram.t  
(* val extend_body: exp Fgram.t *)
(* val delete_rule_body: exp Fgram.t  *)
val dot_lstrings: (FToken.name) Fgram.t 

(**  generally "pat; EOI". *)    
val parse_exp: FLoc.t -> string -> exp
val parse_pat: FLoc.t -> string -> pat

val parse_ident: FLoc.t -> string -> ident

val exp_filter : ep -> exp
val pat_filter : ep -> pat

val exp_filter_n : ep -> exp
val pat_filter_n : ep -> pat

    

val dot_namespace :  string list Fgram.t
    
