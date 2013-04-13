open LibUtil

open Ast

module type Id = sig val name : string val version : string end

module type Warning =
  sig
    type warning = FanLoc.t -> string -> unit 
    val default_warning : warning
    val current_warning : warning ref
    val print_warning : warning
  end

type ('a,'loc) stream_filter = ('a * 'loc) XStream.t -> ('a * 'loc) XStream.t 

module type ParserImpl =
  sig
    val parse_implem :
      ?directive_handler:(stru -> stru option) ->
        FanLoc.t -> char XStream.t -> stru option
    val parse_interf :
      ?directive_handler:(sigi -> sigi option) ->
        FanLoc.t -> char XStream.t -> sigi option
  end

module type PrinterImpl =
  sig
    val print_interf :
      ?input_file:string -> ?output_file:string -> sigi option -> unit
    val print_implem :
      ?input_file:string -> ?output_file:string -> stru option -> unit
  end

module type Syntax =
  sig
    include Warning
    include ParserImpl
    include PrinterImpl
    val interf : (sigi list * FanLoc.t option) Gram.t
    val implem : (stru list * FanLoc.t option) Gram.t
    val top_phrase : stru option Gram.t
    val a_lident : [ `Lid of (loc * string) | ant] Gram.t
    val a_uident : auident Gram.t
    val aident : ident Gram.t
    val astr : astring Gram.t
    val luident : string Gram.t
    val uident : uident Gram.t
    val amp_ctyp : ctyp Gram.t
    val and_ctyp : ctyp Gram.t
    val case : case Gram.t
    val case0 : case Gram.t
    val case_quot : case Gram.t
    val binding : binding Gram.t
    val binding_quot : binding Gram.t
    val rec_exp_quot : rec_exp Gram.t
    val class_declaration : cldecl Gram.t
    val class_description : cltdecl Gram.t
    val clexp : clexp Gram.t
    val clexp_quot : clexp Gram.t
    val class_fun_binding : clexp Gram.t
    val class_fun_def : clexp Gram.t
    val class_info_for_cltyp : cltyp Gram.t
    val class_longident : ident Gram.t
    val clsigi : clsigi Gram.t
    val clsigi_quot : clsigi Gram.t
    val class_signature : clsigi Gram.t
    val cstru : cstru Gram.t
    val cstru_quot : cstru Gram.t
    val class_structure : cstru Gram.t
    val cltyp : cltyp Gram.t
    val cltyp_declaration : cltdecl Gram.t
    val cltyp_longident : ident Gram.t
    val cltyp_plus : cltyp Gram.t
    val cltyp_quot : cltyp Gram.t
    val comma_ctyp : type_parameters Gram.t
    val vid : vid Gram.t
    val comma_exp : exp Gram.t
    val comma_ipat : pat Gram.t
    val comma_pat : pat Gram.t
    val comma_type_parameter : type_parameters Gram.t
    val constrain : type_constr Gram.t
    val constructor_arg_list : ctyp Gram.t
    val constructor_declaration : of_ctyp Gram.t
    val constructor_declarations : or_ctyp Gram.t
    val ctyp : ctyp Gram.t
    val ctyp_quot : ctyp Gram.t
    val cvalue_binding : exp Gram.t
    val direction_flag : direction_flag Gram.t
    val direction_flag_quot : direction_flag Gram.t
    val dummy : unit Gram.t
    val eq_exp : (alident -> pat -> pat) Gram.t
    val exp : exp Gram.t
    val exp_eoi : exp Gram.t
    val exp_quot : exp Gram.t
    val field_exp : rec_exp Gram.t
    val field_exp_list : rec_exp Gram.t
    val fun_binding : exp Gram.t
    val fun_def : exp Gram.t
    val ident : ident Gram.t
    val ident_quot : ident Gram.t
    val ipat : pat Gram.t
    val ipat_tcon : pat Gram.t
    val pat_tcon : pat Gram.t
    val label_declaration : name_ctyp Gram.t
    val label_declaration_list : name_ctyp Gram.t
    val label_exp : rec_exp Gram.t
    val label_exp_list : rec_exp Gram.t
    val label_longident : ident Gram.t
    val label_pat : rec_pat Gram.t
    val label_pat_list : rec_pat Gram.t
    val let_binding : binding Gram.t
    val meth_list : (name_ctyp * row_var_flag) Gram.t
    val meth_decl : name_ctyp Gram.t
    val mbind : mbind Gram.t
    val mbind0 : mexp Gram.t
    val mbind_quot : mbind Gram.t
    val module_declaration : mtyp Gram.t
    val mexp : mexp Gram.t
    val mexp_quot : mexp Gram.t
    val module_longident : vid Gram.t
    val module_longident_with_app : ident Gram.t
    val module_rec_declaration : mbind Gram.t
    val mtyp : mtyp Gram.t
    val mtyp_quot : mtyp Gram.t
    val name_tags : tag_names Gram.t
    val opt_class_self_type : ctyp Gram.t
    val opt_dot_dot : row_var_flag Gram.t
    val row_var_flag_quot : row_var_flag Gram.t
    val opt_meth_list : ctyp Gram.t
    val opt_mutable : mutable_flag Gram.t
    val mutable_flag_quot : mutable_flag Gram.t
    val opt_override : override_flag Gram.t
    val override_flag_quot : override_flag Gram.t
    val opt_private : private_flag Gram.t
    val private_flag_quot : private_flag Gram.t
    val opt_rec : rec_flag Gram.t
    val rec_flag_quot : rec_flag Gram.t
    val opt_virtual : virtual_flag Gram.t
    val virtual_flag_quot : virtual_flag Gram.t
    val pat : pat Gram.t
    val pat_as_pat_opt : pat Gram.t
    val pat_eoi : pat Gram.t
    val pat_quot : pat Gram.t
    val row_field : row_field Gram.t
    val sem_exp : exp Gram.t
    val sem_exp_for_list : (exp -> exp) Gram.t
    val sem_pat : pat Gram.t
    val sem_pat_for_list : (pat -> pat) Gram.t
    val sequence : exp Gram.t
    val sigi : sigi Gram.t
    val sigi_quot : sigi Gram.t
    val sigis : sigi Gram.t
    val star_ctyp : ctyp Gram.t
    val stru : stru Gram.t
    val stru_quot : stru Gram.t
    val strus : stru Gram.t
    val type_declaration : typedecl Gram.t
    val type_ident_and_parameters : (alident * opt_decl_params) Gram.t
    val type_info : type_info Gram.t
    val type_repr : type_repr Gram.t
    val type_longident : ident Gram.t
    val type_longident_and_parameters : ctyp Gram.t
    val type_parameter : decl_param Gram.t
    val type_parameters : (ctyp -> ctyp) Gram.t
    val typevars : ctyp Gram.t
    val val_longident : ident Gram.t
    val constr : constr Gram.t
    val constr_quot : constr Gram.t
    val prefixop : exp Gram.t
    val infixop0 : exp Gram.t
    val infixop1 : exp Gram.t
    val infixop2 : exp Gram.t
    val infixop3 : exp Gram.t
    val infixop4 : exp Gram.t
    val string_list : strings Gram.t
    val infixop5 : exp Gram.t
    val infixop6 : exp Gram.t
    val module_longident_dot_lparen : ident Gram.t
    val sequence' : (exp -> exp) Gram.t
    val fun_def : exp Gram.t
    val method_opt_override : override_flag Gram.t
    val value_val_opt_override : override_flag Gram.t
    val unquoted_typevars : ctyp Gram.t
    val lang : FanToken.name Gram.t
    val with_exp_lang : exp Gram.t
    val with_stru_lang : stru Gram.t
    val symbol : FanGrammar.symbol Gram.t
    val rule : FanGrammar.rule Gram.t
    val rule_list : FanGrammar.rule list Gram.t
    val psymbol : FanGrammar.symbol Gram.t
    val level : FanGrammar.level Gram.t
    val level_list :
      [ `Group of FanGrammar.level list | `Single of FanGrammar.level] Gram.t
    val entry : FanGrammar.entry Gram.t
    val extend_body : exp Gram.t
    val delete_rule_body : exp Gram.t
    val dot_lstrings : FanToken.name Gram.t
    val parse_exp : FanLoc.t -> string -> exp
    val parse_pat : FanLoc.t -> string -> pat
    val parse_ident : FanLoc.t -> string -> ident
    val exp_filter : ep -> exp
    val pat_filter : ep -> pat
    val dot_namespace : string list Gram.t
    module Options :
    sig
      type spec_list = (string * FanArg.spec * string) list 
      val init : spec_list -> unit
      val add : (string * FanArg.spec * string) -> unit
      val adds : (string * FanArg.spec * string) list -> unit
      val init_spec_list : spec_list ref
    end
  end

module type SyntaxExtension = functor (Syn : Syntax) -> Syntax

module type PLUGIN = functor (Unit : sig  end) -> sig  end

module type SyntaxPlugin = functor (Syn : Syntax) -> sig  end

module type PrinterPlugin = functor (Syn : Syntax) -> PrinterImpl

module type ParserPlugin = functor (Syn : Syntax) -> ParserImpl

type 'a parser_fun =
  ?directive_handler:('a -> 'a option) -> loc -> char XStream.t -> 'a option 

type 'a printer_fun =
  ?input_file:string -> ?output_file:string -> 'a option -> unit 

module type PRECAST =
  sig
    module Syntax : Syntax
    val loaded_modules : string list ref
    val iter_and_take_callbacks : ((string * (unit -> unit)) -> unit) -> unit
    val register_stru_parser : stru parser_fun -> unit
    val register_sigi_parser : sigi parser_fun -> unit
    val register_parser : stru parser_fun -> sigi parser_fun -> unit
    val current_parser : unit -> (stru parser_fun * sigi parser_fun)
    val plugin : (module Id) -> (module PLUGIN) -> unit
    val syntax_plugin : (module Id) -> (module SyntaxPlugin) -> unit
    val syntax_extension : (module Id) -> (module SyntaxExtension) -> unit
    val printer_plugin : (module Id) -> (module PrinterPlugin) -> unit
    val replace_printer : (module Id) -> (module PrinterImpl) -> unit
    val replace_parser : (module Id) -> (module ParserImpl) -> unit
    val parser_plugin : (module Id) -> (module ParserPlugin) -> unit
    val enable_ocaml_printer : unit -> unit
    val enable_dump_ocaml_ast_printer : unit -> unit
    val enable_dump_ast_printer : unit -> unit
    val enable_null_printer : unit -> unit
    val enable_auto : (unit -> bool) -> unit
    val register_stru_printer : stru printer_fun -> unit
    val register_sigi_printer : sigi printer_fun -> unit
    val register_printer : stru printer_fun -> sigi printer_fun -> unit
    val current_printer : unit -> (stru printer_fun * sigi printer_fun)
    val declare_dyn_module : string -> (unit -> unit) -> unit
    module CurrentParser : ParserImpl
    module CurrentPrinter : PrinterImpl
  end

module type PRECAST_PLUGIN = sig val apply : (module PRECAST) -> unit end