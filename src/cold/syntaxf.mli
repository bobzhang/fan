
(** Fan's core nonterminals exported *)  

open Astf





val interf : (sigi list  * Locf.t option ) Gramf.t 

val implem : (stru list  * Locf.t option ) Gramf.t 
val let_bind : bind Gramf.t
val top_phrase : stru option Gramf.t 

val a_lident : alident  Gramf.t  (* alident *)
val a_uident : auident Gramf.t
val aident : ident Gramf.t
val astr : astring Gramf.t
val luident : string Gramf.t
val uident : uident Gramf.t  
    
    
val amp_ctyp : ctyp Gramf.t
val and_ctyp : ctyp Gramf.t
val case : case Gramf.t
val case0 : case Gramf.t
val case_quot : case Gramf.t
val bind : bind Gramf.t
val bind_quot : bind Gramf.t
val rec_exp_quot : rec_exp Gramf.t
val class_declaration : cldecl Gramf.t
val class_description : cltdecl Gramf.t
val clexp : clexp Gramf.t
val clexp_quot : clexp Gramf.t
val class_fun_bind : clexp Gramf.t
val class_fun_def : clexp Gramf.t
val class_info_for_cltyp : cltyp Gramf.t
val class_longident : ident Gramf.t
val clsigi : clsigi Gramf.t
val clsigi_quot : clsigi Gramf.t
val class_signature : clsigi Gramf.t
val clfield : clfield Gramf.t
val clfield_quot : clfield Gramf.t
val class_structure : clfield Gramf.t
val cltyp : cltyp Gramf.t
val cltyp_declaration : cltdecl Gramf.t
val cltyp_longident : ident Gramf.t

val cltyp_plus : cltyp Gramf.t
val cltyp_quot : cltyp Gramf.t
val comma_ctyp : type_parameters Gramf.t

val vid: vid Gramf.t  
val comma_exp : exp Gramf.t
val comma_ipat : pat Gramf.t
val comma_pat : pat Gramf.t
val comma_type_parameter : type_parameters Gramf.t
val constrain : type_constr Gramf.t
val constructor_arg_list : ctyp Gramf.t
val constructor_declaration : of_ctyp Gramf.t
val constructor_declarations : or_ctyp Gramf.t
val ctyp : ctyp Gramf.t
val ctyp_quot : ctyp Gramf.t
val cvalue_bind : exp Gramf.t
val flag : flag Gramf.t
val direction_flag_quot : flag Gramf.t

val eq_exp : (alident -> pat -> pat) Gramf.t 
val exp : exp Gramf.t
val exp_eoi : exp Gramf.t
val exp_quot : exp Gramf.t
val field_exp : rec_exp Gramf.t
val field_exp_list : rec_exp Gramf.t
val fun_bind : exp Gramf.t
val fun_def : exp Gramf.t
val ident : ident Gramf.t
val ident_quot : ident Gramf.t
val ipat : pat Gramf.t
val ipat_tcon : pat Gramf.t
val pat_tcon : pat Gramf.t    

val label_declaration : name_ctyp Gramf.t
val label_declaration_list : name_ctyp Gramf.t
val label_exp : rec_exp Gramf.t
val label_exp_list : rec_exp Gramf.t
val label_longident : vid Gramf.t

val label_pat : rec_pat Gramf.t
val label_pat_list : rec_pat Gramf.t
    
val meth_list : (name_ctyp * flag) Gramf.t 
val meth_decl : name_ctyp Gramf.t
val mbind : mbind Gramf.t
val mbind0 : mexp Gramf.t
val mbind_quot : mbind Gramf.t
val module_declaration : mtyp Gramf.t
val mexp : mexp Gramf.t
val mexp_quot : mexp Gramf.t
val module_longident : vid Gramf.t
val module_longident_with_app : ident Gramf.t
val module_rec_declaration : mbind Gramf.t
val mtyp : mtyp Gramf.t
val mtyp_quot : mtyp Gramf.t
val name_tags : tag_names Gramf.t
val opt_class_self_type : ctyp Gramf.t

val opt_dot_dot : flag Gramf.t
val row_var_flag_quot : flag Gramf.t
val opt_meth_list : ctyp Gramf.t
val opt_mutable : flag Gramf.t
val mutable_flag_quot : flag Gramf.t
val opt_override : flag Gramf.t
val override_flag_quot : flag Gramf.t
val opt_private : flag Gramf.t
val private_flag_quot : flag Gramf.t
val opt_rec : flag Gramf.t
val rec_flag_quot : flag Gramf.t
val opt_virtual : flag Gramf.t
val virtual_flag_quot : flag Gramf.t
val pat : pat Gramf.t

val pat_eoi : pat Gramf.t
val pat_quot : pat Gramf.t
val row_field : row_field Gramf.t
val sem_exp : exp Gramf.t
val sem_exp_for_list : exp Gramf.t 
val sem_pat : pat Gramf.t
val sem_pat_for_list :  pat  Gramf.t
val sequence : exp Gramf.t
val sigi : sigi Gramf.t
val sigi_quot : sigi Gramf.t
val sigis : sigi Gramf.t

val star_ctyp: ctyp Gramf.t
val com_ctyp: ctyp Gramf.t
val stru : stru Gramf.t
val stru_quot : stru Gramf.t
val strus : stru Gramf.t
val type_declaration : typedecl Gramf.t
val type_ident_and_parameters : (alident * opt_decl_params) Gramf.t 
val type_info: type_info Gramf.t
val type_repr: type_repr Gramf.t
    
val type_longident : ident Gramf.t
val type_longident_and_parameters : ctyp Gramf.t
val type_parameter : decl_param Gramf.t
val type_parameters : (ctyp -> ctyp) Gramf.t 
val typevars : ctyp Gramf.t
val val_longident : ident Gramf.t
val constr : constr Gramf.t

val constr_quot : constr Gramf.t
val string_list : strings Gramf.t


val module_longident_dot_lparen : ident Gramf.t
val sequence' : (exp -> exp) Gramf.t 
val fun_def : exp Gramf.t 

val method_opt_override : flag Gramf.t
val value_val_opt_override : flag Gramf.t
val unquoted_typevars :ctyp Gramf.t
val lang : Tokenf.name option Gramf.t
val with_exp_lang : exp Gramf.t  
val with_stru_lang : stru Gramf.t
    
val dot_lstrings : Tokenf.name Gramf.t 

    


    
