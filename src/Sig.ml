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
type stream_filter 'a 'loc =
    XStream.t ('a * 'loc) -> XStream.t ('a * 'loc);

module type ParserImpl = sig
  (** When  the parser encounter a directive it stops (since the directive may change  the
      syntax), the given [directive_handler] function  evaluates  it  and
      the parsing starts again. *)
  val parse_implem : ?directive_handler:(str_item -> option str_item) ->
    FanLoc.t -> XStream.t char -> str_item;

  val parse_interf : ?directive_handler:(sig_item -> option sig_item) ->
        FanLoc.t -> XStream.t char -> sig_item;
end;

module type PrinterImpl = sig
  val print_interf : ?input_file:string -> ?output_file:string ->
    sig_item -> unit;
  val print_implem : ?input_file:string -> ?output_file:string ->
    str_item -> unit;
end;


  
module type Syntax = sig
  include Warning;
  include ParserImpl;
  include PrinterImpl;
  val interf : Gram.t (list sig_item * option FanLoc.t);
  val implem : Gram.t (list str_item * option FanLoc.t);
  val top_phrase : Gram.t (option str_item);
  val a_CHAR : Gram.t string;
  val a_FLOAT : Gram.t string;
  val a_INT : Gram.t string;
  val a_INT32 : Gram.t string;
  val a_INT64 : Gram.t string;
  val a_LABEL : Gram.t string;
  val a_LIDENT: Gram.t string;
  val a_string: Gram.t astring;
  val a_lident: Gram.t [= `Lid of (loc*string) | ant] (* alident *);
  val a_uident: Gram.t auident;  
  val a_NATIVEINT : Gram.t string;
  val a_OPTLABEL : Gram.t string;
  val a_STRING : Gram.t string;
  val a_UIDENT : Gram.t string;
  val a_ident : Gram.t string;
  val amp_ctyp : Gram.t ctyp;
  val and_ctyp : Gram.t ctyp;
  val match_case : Gram.t match_case;
  val match_case0 : Gram.t match_case;
  val match_case_quot : Gram.t match_case;
  val binding : Gram.t binding;
  val binding_quot : Gram.t binding;
  val rec_binding_quot : Gram.t rec_binding;
  val class_declaration : Gram.t class_expr;
  val class_description : Gram.t class_type;
  val class_expr : Gram.t class_expr;
  val class_expr_quot : Gram.t class_expr;
  val class_fun_binding : Gram.t class_expr;
  val class_fun_def : Gram.t class_expr;
  val class_info_for_class_expr : Gram.t class_expr;
  val class_info_for_class_type : Gram.t class_type;
  val class_longident : Gram.t ident;
  val class_longident_and_param : Gram.t class_expr;
  val class_name_and_param : Gram.t (string * ctyp);
  val class_sig_item : Gram.t class_sig_item;
  val class_sig_item_quot : Gram.t class_sig_item;
  val class_signature : Gram.t class_sig_item;
  val class_str_item : Gram.t class_str_item;
  val class_str_item_quot : Gram.t class_str_item;
  val class_structure : Gram.t class_str_item;
  val class_type : Gram.t class_type;
  val class_type_declaration : Gram.t class_type;
  val class_type_longident : Gram.t ident;
  val class_type_longident_and_param : Gram.t class_type;
  val class_type_plus : Gram.t class_type;
  val class_type_quot : Gram.t class_type;
  val comma_ctyp : Gram.t ctyp;
  val comma_expr : Gram.t expr;
  val comma_ipatt : Gram.t patt;
  val comma_patt : Gram.t patt;
  val comma_type_parameter : Gram.t ctyp;
  val constrain : Gram.t (ctyp * ctyp);
  val constructor_arg_list : Gram.t ctyp;
  val constructor_declaration : Gram.t ctyp;
  val constructor_declarations : Gram.t ctyp;
  val ctyp : Gram.t ctyp;
  val ctyp_quot : Gram.t ctyp;
  val cvalue_binding : Gram.t expr;
  val direction_flag : Gram.t direction_flag;
  val direction_flag_quot : Gram.t direction_flag;
  val dummy : Gram.t unit;
  val eq_expr : Gram.t ((* string *)alident -> patt -> patt);
  val expr : Gram.t expr;
  val expr_eoi : Gram.t expr;
  val expr_quot : Gram.t expr;
  val field_expr : Gram.t rec_binding;
  val field_expr_list : Gram.t rec_binding;
  val fun_binding : Gram.t expr;
  val fun_def : Gram.t expr;
  val ident : Gram.t ident;
  val ident_quot : Gram.t ident;
  val ipatt : Gram.t patt;
  val ipatt_tcon : Gram.t patt;
  val patt_tcon : Gram.t patt;    
  val label : Gram.t string;
  val label_declaration : Gram.t ctyp;
  val label_declaration_list : Gram.t ctyp;
  val label_expr : Gram.t rec_binding;
  val label_expr_list : Gram.t rec_binding;
  val label_longident : Gram.t ident;
  val label_patt : Gram.t patt;
  val label_patt_list : Gram.t patt;
  val let_binding : Gram.t binding;
  val meth_list : Gram.t (ctyp * row_var_flag);
  val meth_decl : Gram.t ctyp;
  val module_binding : Gram.t module_binding;
  val module_binding0 : Gram.t module_expr;
  val module_binding_quot : Gram.t module_binding;
  val module_declaration : Gram.t module_type;
  val module_expr : Gram.t module_expr;
  val module_expr_quot : Gram.t module_expr;
  val module_longident : Gram.t ident;
  val module_longident_with_app : Gram.t ident;
  val module_rec_declaration : Gram.t module_binding;
  val module_type : Gram.t module_type;
  val package_type : Gram.t module_type;
  val module_type_quot : Gram.t module_type;
  val more_ctyp : Gram.t ctyp;
  val name_tags : Gram.t ctyp;
  val opt_as_lident : Gram.t (* string *)(meta_option alident);
  val opt_class_self_patt : Gram.t patt;
  val opt_class_self_type : Gram.t ctyp;
  val opt_comma_ctyp : Gram.t ctyp;
  val opt_dot_dot : Gram.t row_var_flag;
  val row_var_flag_quot : Gram.t row_var_flag;
  val opt_eq_ctyp : Gram.t ctyp;
  val opt_expr : Gram.t expr;
  val opt_meth_list : Gram.t ctyp;
  val opt_mutable : Gram.t mutable_flag;
  val mutable_flag_quot : Gram.t mutable_flag;
  val opt_override : Gram.t override_flag;
  val override_flag_quot : Gram.t override_flag;
  val opt_polyt : Gram.t ctyp;
  val opt_private : Gram.t private_flag;
  val private_flag_quot : Gram.t private_flag;
  val opt_rec : Gram.t rec_flag;
  val rec_flag_quot : Gram.t rec_flag;
  val opt_virtual : Gram.t virtual_flag;
  val virtual_flag_quot : Gram.t virtual_flag;
  val patt : Gram.t patt;
  val patt_as_patt_opt : Gram.t patt;
  val patt_eoi : Gram.t patt;
  val patt_quot : Gram.t patt;
  val poly_type : Gram.t ctyp;
  val row_field : Gram.t ctyp;
  val sem_expr : Gram.t expr;
  val sem_expr_for_list : Gram.t (expr -> expr);
  val sem_patt : Gram.t patt;
  val sem_patt_for_list : Gram.t (patt -> patt);
  val semi : Gram.t unit;
  val sequence : Gram.t expr;
  val sig_item : Gram.t sig_item;
  val sig_item_quot : Gram.t sig_item;
  val sig_items : Gram.t sig_item;
  val star_ctyp : Gram.t ctyp;
  val str_item : Gram.t str_item;
  val str_item_quot : Gram.t str_item;
  val str_items : Gram.t str_item;
  val type_constraint : Gram.t unit;
  val type_declaration : Gram.t ctyp;
  val type_ident_and_parameters : Gram.t (string * list ctyp);
  val type_kind : Gram.t ctyp;
  val type_longident : Gram.t ident;
  val type_longident_and_parameters : Gram.t ctyp;
  val type_parameter : Gram.t ctyp;
  val type_parameters : Gram.t (ctyp -> ctyp);
  val typevars : Gram.t ctyp;
  val val_longident : Gram.t ident;
  val with_constr : Gram.t with_constr;
  val with_constr_quot : Gram.t with_constr;
  val prefixop : Gram.t expr;
  val infixop0 : Gram.t expr;
  val infixop1 : Gram.t expr;
  val infixop2 : Gram.t expr;
  val infixop3 : Gram.t expr;
  val infixop4 : Gram.t expr;

  val string_list: Gram.t (meta_list string);
  val infixop5: Gram.t expr;
  val infixop6: Gram.t expr;
  val module_longident_dot_lparen: Gram.t ident;
  val sequence':Gram.t (expr -> expr); 
  val fun_def: Gram.t expr;
  val optional_type_parameter:  Gram.t ctyp;
  val method_opt_override: Gram.t override_flag;
  val value_val_opt_override: Gram.t override_flag;
  val unquoted_typevars:Gram.t ctyp;
  val lang: Gram.t string;
  val symbol:  Gram.t FanGrammar.symbol ;
  val rule:  Gram.t FanGrammar.rule;
  val rule_list: Gram.t (list FanGrammar.rule);
  val psymbol: Gram.t FanGrammar.symbol;
  val level:  Gram.t FanGrammar.level;
  val level_list: Gram.t (list FanGrammar.level);
  val entry: Gram.t FanGrammar.entry;
  val extend_body: Gram.t expr;
  val delete_rule_body: Gram.t expr;
  val dot_lstrings: Gram.t (list string);  
  val parse_expr: FanLoc.t -> string -> expr;
    (**  generally "patt; EOI". *)
  val parse_patt: FanLoc.t -> string -> patt;

  val parse_ident: FanLoc.t -> string -> ident;

  val expr_filter: expr -> expr;
  val patt_filter: patt -> patt;
    
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

(* module type OCAML_PARSER = functor (Ast:FanAst) -> Parser.S ; *)
module type ParserPlugin = functor (Syn:Syntax) -> ParserImpl;
(* module type ASTFILTER_PLUGIN  = functor (F:AstFilters.S) -> sig end ; *)


type parser_fun 'a =
    ?directive_handler:('a -> option 'a) -> FanLoc.t -> XStream.t char -> 'a;
type printer_fun 'a =
      ?input_file:string -> ?output_file:string -> 'a -> unit;

module type PRECAST = sig
  module Syntax     : Syntax ;
  module Printers : sig
    module OCaml         : PrinterImpl;
    module DumpOCamlAst  : PrinterImpl;
    module DumpCamlp4Ast : PrinterImpl;
    module Null          : PrinterImpl;
  end;

  val loaded_modules : ref (list string);
  val iter_and_take_callbacks : ((string * (unit -> unit)) -> unit) -> unit ;
  val register_str_item_parser : parser_fun str_item -> unit;
  val register_sig_item_parser : parser_fun sig_item -> unit;
  val register_parser :
      parser_fun str_item -> parser_fun sig_item -> unit;
  val current_parser :
      unit -> (parser_fun str_item * parser_fun sig_item);

  val plugin : (module Id) -> (module PLUGIN) -> unit ;
  val syntax_plugin:(module Id) -> (module SyntaxPlugin) -> unit;
  val syntax_extension: (module Id) -> (module SyntaxExtension) -> unit;
  val printer_plugin: (module Id) -> (module PrinterPlugin) -> unit;
  val replace_printer: (module Id) -> (module PrinterImpl) -> unit;
  val replace_parser: (module Id) -> (module ParserImpl) -> unit;
  val parser_plugin: (module Id) -> (module ParserPlugin) -> unit;
  val enable_ocaml_printer: unit -> unit;
  val enable_dump_ocaml_ast_printer: unit -> unit;
  val enable_dump_camlp4_ast_printer: unit -> unit;
  val enable_null_printer: unit -> unit;
  val enable_auto: (unit->bool) -> unit;


  val register_str_item_printer : printer_fun str_item -> unit;
  val register_sig_item_printer : printer_fun sig_item -> unit;
  val register_printer :
      printer_fun str_item -> printer_fun sig_item -> unit;
  val current_printer :
      unit -> (printer_fun str_item * printer_fun sig_item);
        
  val declare_dyn_module : string -> (unit -> unit) -> unit  ;

  module CurrentParser : ParserImpl;
  module CurrentPrinter : PrinterImpl;
end ;


(* for dynamic loading *)
module type PRECAST_PLUGIN = sig
  val apply : (module PRECAST) -> unit;
end;



