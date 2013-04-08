open LibUtil;
open Ast;

module type Id = sig
  val name : string;
  val version : string;
end;
module type Warning = sig
  type warning = FanLoc.t -> string -> unit;
  val default_warning: warning;
  val current_warning: ref warning;
  val print_warning: warning;  
end;

(** A type for stream filters. *)
type ('a, 'loc) stream_filter  =
    XStream.t ('a * 'loc) -> XStream.t ('a * 'loc);

module type ParserImpl = sig
  (** When  the parser encounter a directive it stops (since the directive may change  the
      syntax), the given [directive_handler] function  evaluates  it  and
      the parsing starts again. *)
  val parse_implem : ?directive_handler:(stru -> option stru) ->
    FanLoc.t -> XStream.t char -> option stru;

  val parse_interf : ?directive_handler:(sig_item -> option sig_item) ->
        FanLoc.t -> XStream.t char -> option sig_item;
end;

module type PrinterImpl = sig
  val print_interf : ?input_file:string -> ?output_file:string ->
    option sig_item  -> unit;
  val print_implem : ?input_file:string -> ?output_file:string ->
    option stru -> unit;
end;


  
module type Syntax = sig
  include Warning;
  include ParserImpl;
  include PrinterImpl;
  val interf : Gram.t (list sig_item * option FanLoc.t);
  val implem : Gram.t (list stru * option FanLoc.t);
  val top_phrase : Gram.t (option stru);
  (* val a_string: Gram.t astring; *)
  val a_lident: Gram.t [= `Lid of (loc*string) | ant] (* alident *);
  val a_uident: Gram.t auident;
  val aident: Gram.t ident;
  val astr: Gram.t astring;
  val luident: Gram.t string;
  val uident: Gram.t uident;  
    
    
  val amp_ctyp : Gram.t ctyp;
  val and_ctyp : Gram.t ctyp;
  val case : Gram.t case;
  val case0 : Gram.t case;
  val case_quot : Gram.t case;
  val binding : Gram.t binding;
  val binding_quot : Gram.t binding;
  val rec_exp_quot : Gram.t rec_exp;
  val class_declaration : Gram.t class_exp;
  val class_description : Gram.t class_type;
  val class_exp : Gram.t class_exp;
  val class_exp_quot : Gram.t class_exp;
  val class_fun_binding : Gram.t class_exp;
  val class_fun_def : Gram.t class_exp;
  val class_info_for_class_exp : Gram.t class_exp;
  val class_info_for_class_type : Gram.t class_type;
  val class_longident : Gram.t ident;
  val class_longident_and_param : Gram.t class_exp;
  val class_sig_item : Gram.t class_sig_item;
  val class_sig_item_quot : Gram.t class_sig_item;
  val class_signature : Gram.t class_sig_item;
  val cstru : Gram.t cstru;
  val cstru_quot : Gram.t cstru;
  val class_structure : Gram.t cstru;
  val class_type : Gram.t class_type;
  val class_type_declaration : Gram.t class_type;
  val class_type_longident : Gram.t ident;
  val class_type_longident_and_param : Gram.t class_type;
  val class_type_plus : Gram.t class_type;
  val class_type_quot : Gram.t class_type;
  val comma_ctyp : Gram.t type_parameters;

    
  val comma_exp : Gram.t exp;
  val comma_ipat : Gram.t pat;
  val comma_pat : Gram.t pat;
  val comma_type_parameter : Gram.t type_parameters;
  val constrain : Gram.t type_constr;
  val constructor_arg_list : Gram.t ctyp;
  val constructor_declaration : Gram.t of_ctyp;
  val constructor_declarations : Gram.t or_ctyp;
  val ctyp : Gram.t ctyp;
  val ctyp_quot : Gram.t ctyp;
  val cvalue_binding : Gram.t exp;
  val direction_flag : Gram.t direction_flag;
  val direction_flag_quot : Gram.t direction_flag;
  val dummy : Gram.t unit;
  val eq_exp : Gram.t (alident -> pat -> pat);
  val exp : Gram.t exp;
  val exp_eoi : Gram.t exp;
  val exp_quot : Gram.t exp;
  val field_exp : Gram.t rec_exp;
  val field_exp_list : Gram.t rec_exp;
  val fun_binding : Gram.t exp;
  val fun_def : Gram.t exp;
  val ident : Gram.t ident;
  val ident_quot : Gram.t ident;
  val ipat : Gram.t pat;
  val ipat_tcon : Gram.t pat;
  val pat_tcon : Gram.t pat;    

  val label_declaration : Gram.t name_ctyp;
  val label_declaration_list : Gram.t name_ctyp;
  val label_exp : Gram.t rec_exp;
  val label_exp_list : Gram.t rec_exp;
  val label_longident : Gram.t ident;

  val label_pat : Gram.t rec_pat;
  val label_pat_list : Gram.t rec_pat;
    
  val let_binding : Gram.t binding;
  val meth_list : Gram.t (name_ctyp * row_var_flag);
  val meth_decl : Gram.t name_ctyp;
  val module_binding : Gram.t module_binding;
  val module_binding0 : Gram.t module_exp;
  val module_binding_quot : Gram.t module_binding;
  val module_declaration : Gram.t module_type;
  val module_exp : Gram.t module_exp;
  val module_exp_quot : Gram.t module_exp;
  val module_longident : Gram.t ident;
  val module_longident_with_app : Gram.t ident;
  val module_rec_declaration : Gram.t module_binding;
  val module_type : Gram.t module_type;
  (* val package_type : Gram.t module_type; *)
  val module_type_quot : Gram.t module_type;
  val more_ctyp : Gram.t ctyp;
  val name_tags : Gram.t tag_names;
  val opt_class_self_type : Gram.t ctyp;

  val opt_dot_dot : Gram.t row_var_flag;
  val row_var_flag_quot : Gram.t row_var_flag;
  val opt_meth_list : Gram.t ctyp;
  val opt_mutable : Gram.t mutable_flag;
  val mutable_flag_quot : Gram.t mutable_flag;
  val opt_override : Gram.t override_flag;
  val override_flag_quot : Gram.t override_flag;
  val opt_private : Gram.t private_flag;
  val private_flag_quot : Gram.t private_flag;
  val opt_rec : Gram.t rec_flag;
  val rec_flag_quot : Gram.t rec_flag;
  val opt_virtual : Gram.t virtual_flag;
  val virtual_flag_quot : Gram.t virtual_flag;
  val pat : Gram.t pat;
  val pat_as_pat_opt : Gram.t pat;
  val pat_eoi : Gram.t pat;
  val pat_quot : Gram.t pat;
  (* val poly_type : Gram.t ctyp; *)
  val row_field : Gram.t row_field;
  val sem_exp : Gram.t exp;
  val sem_exp_for_list : Gram.t (exp -> exp);
  val sem_pat : Gram.t pat;
  val sem_pat_for_list : Gram.t (pat -> pat);
  (* val semi : Gram.t unit; *)
  val sequence : Gram.t exp;
  val sig_item : Gram.t sig_item;
  val sig_item_quot : Gram.t sig_item;
  val sig_items : Gram.t sig_item;
  val star_ctyp : Gram.t ctyp;
  val stru : Gram.t stru;
  val stru_quot : Gram.t stru;
  val strus : Gram.t stru;
  (* val type_constraint : Gram.t unit; *)
  val type_declaration : Gram.t typedecl;
  val type_ident_and_parameters : Gram.t (alident * opt_decl_params);
  (* val type_kind : Gram.t ctyp; *)
  val type_info: Gram.t type_info;
  val type_repr: Gram.t type_repr;
    
  val type_longident : Gram.t ident;
  val type_longident_and_parameters : Gram.t ctyp;
  val type_parameter : Gram.t decl_param;
  val type_parameters : Gram.t (ctyp -> ctyp);
  val typevars : Gram.t ctyp;
  val val_longident : Gram.t ident;
  val with_constr : Gram.t with_constr;
  val with_constr_quot : Gram.t with_constr;
  val prefixop : Gram.t exp;
  val infixop0 : Gram.t exp;
  val infixop1 : Gram.t exp;
  val infixop2 : Gram.t exp;
  val infixop3 : Gram.t exp;
  val infixop4 : Gram.t exp;

  val string_list: Gram.t strings;
  val infixop5: Gram.t exp;
  val infixop6: Gram.t exp;
  val module_longident_dot_lparen: Gram.t ident;
  val sequence':Gram.t (exp -> exp); 
  val fun_def: Gram.t exp;

  val method_opt_override: Gram.t override_flag;
  val value_val_opt_override: Gram.t override_flag;
  val unquoted_typevars:Gram.t ctyp;
  val lang: Gram.t FanToken.name;
  val with_exp_lang: Gram.t exp;  
  val with_stru_lang: Gram.t stru;  
  val symbol:  Gram.t FanGrammar.symbol ;
  val rule:  Gram.t FanGrammar.rule;
  (* val meta_rule: Gram.t exp; *)

  (* val rules: Gram.t (list FanGrammar.rule); *)
  val rule_list: Gram.t (list FanGrammar.rule);
  val psymbol: Gram.t FanGrammar.symbol;
  val level:  Gram.t FanGrammar.level;
  val level_list: Gram.t ([= `Group of (list FanGrammar.level) | `Single of FanGrammar.level ]);
  val entry: Gram.t FanGrammar.entry;
  val extend_body: Gram.t exp;
  val delete_rule_body: Gram.t exp;
  val dot_lstrings: Gram.t (FanToken.name);  
  val parse_exp: FanLoc.t -> string -> exp;
    (**  generally "pat; EOI". *)
  val parse_pat: FanLoc.t -> string -> pat;

  val parse_ident: FanLoc.t -> string -> ident;

  val exp_filter: (* exp *)ep -> exp;
  val pat_filter: (* pat *)ep -> pat;

  val dot_namespace: Gram.t (list string);  
  module Options:sig
    type spec_list = list (string * FanArg.spec * string);
    val init : spec_list -> unit;
    val add : (string * FanArg.spec * string) -> unit;
    val adds : list (string * FanArg.spec * string) -> unit;
    val init_spec_list: ref spec_list;
  end;
end;


module type SyntaxExtension =
    functor (Syn : Syntax)  -> Syntax;

module type PLUGIN = functor (Unit:sig end) -> sig end;
module type SyntaxPlugin = functor (Syn:Syntax) -> sig end ;

(** generate two printers to be registered*)
module type PrinterPlugin = functor (Syn:Syntax) -> PrinterImpl;


module type ParserPlugin = functor (Syn:Syntax) -> ParserImpl;
(* module type ASTFILTER_PLUGIN  = functor (F:AstFilters.S) -> sig end ; *)


type 'a parser_fun  =
    ?directive_handler:('a -> option 'a) -> loc
      -> XStream.t char -> option 'a;

type 'a printer_fun  =
      ?input_file:string -> ?output_file:string ->
        option 'a -> unit;

module type PRECAST = sig
  module Syntax     : Syntax ;
  val loaded_modules : ref (list string);
  val iter_and_take_callbacks : ((string * (unit -> unit)) -> unit) -> unit ;
  val register_stru_parser : parser_fun stru -> unit;
  val register_sig_item_parser : parser_fun sig_item -> unit;
  val register_parser :
      parser_fun stru -> parser_fun sig_item -> unit;
  val current_parser :
      unit -> (parser_fun stru * parser_fun sig_item);

  val plugin : (module Id) -> (module PLUGIN) -> unit ;
  val syntax_plugin:(module Id) -> (module SyntaxPlugin) -> unit;
  val syntax_extension: (module Id) -> (module SyntaxExtension) -> unit;
  val printer_plugin: (module Id) -> (module PrinterPlugin) -> unit;
  val replace_printer: (module Id) -> (module PrinterImpl) -> unit;
  val replace_parser: (module Id) -> (module ParserImpl) -> unit;
  val parser_plugin: (module Id) -> (module ParserPlugin) -> unit;
  val enable_ocaml_printer: unit -> unit;
  val enable_dump_ocaml_ast_printer: unit -> unit;
  val enable_dump_ast_printer: unit -> unit;
  val enable_null_printer: unit -> unit;
  val enable_auto: (unit->bool) -> unit;


  val register_stru_printer : printer_fun stru -> unit;
  val register_sig_item_printer : printer_fun sig_item -> unit;
  val register_printer :
      printer_fun stru -> printer_fun sig_item -> unit;
  val current_printer :
      unit -> (printer_fun stru * printer_fun sig_item);
        
  val declare_dyn_module : string -> (unit -> unit) -> unit  ;

  module CurrentParser : ParserImpl;
  module CurrentPrinter : PrinterImpl;
end ;


(* for dynamic loading *)
module type PRECAST_PLUGIN = sig
  val apply : (module PRECAST) -> unit;
end;



