open LibUtil;

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
type stream_filter 'a 'loc = XStream.t ('a * 'loc) -> XStream.t ('a * 'loc);

module type ParserImpl = sig
  (** When  the parser encounter a directive it stops (since the directive may change  the
      syntax), the given [directive_handler] function  evaluates  it  and
      the parsing starts again. *)
  val parse_implem : ?directive_handler:(Ast.str_item -> option Ast.str_item) ->
    FanLoc.t -> XStream.t char -> Ast.str_item;

  val parse_interf : ?directive_handler:(Ast.sig_item -> option Ast.sig_item) ->
        FanLoc.t -> XStream.t char -> Ast.sig_item;
end;

module type PrinterImpl = sig
  val print_interf : ?input_file:string -> ?output_file:string ->
    Ast.sig_item -> unit;
  val print_implem : ?input_file:string -> ?output_file:string ->
    Ast.str_item -> unit;
end;


  
module type Camlp4Syntax = sig
  include Warning;
  include ParserImpl;
  include PrinterImpl;
  val interf : Gram.t (list Ast.sig_item * option FanLoc.t);
  val implem : Gram.t (list Ast.str_item * option FanLoc.t);
  val top_phrase : Gram.t (option Ast.str_item);
  (* val use_file : Gram.t (list Ast.str_item * option FanLoc.t); *)
  val a_CHAR : Gram.t string;
  val a_FLOAT : Gram.t string;
  val a_INT : Gram.t string;
  val a_INT32 : Gram.t string;
  val a_INT64 : Gram.t string;
  val a_LABEL : Gram.t string;
  val a_LIDENT : Gram.t string;
  val a_NATIVEINT : Gram.t string;
  val a_OPTLABEL : Gram.t string;
  val a_STRING : Gram.t string;
  val a_UIDENT : Gram.t string;
  val a_ident : Gram.t string;
  val amp_ctyp : Gram.t Ast.ctyp;
  val and_ctyp : Gram.t Ast.ctyp;
  val match_case : Gram.t Ast.match_case;
  val match_case0 : Gram.t Ast.match_case;
  val match_case_quot : Gram.t Ast.match_case;
  val binding : Gram.t Ast.binding;
  val binding_quot : Gram.t Ast.binding;
  val rec_binding_quot : Gram.t Ast.rec_binding;
  val class_declaration : Gram.t Ast.class_expr;
  val class_description : Gram.t Ast.class_type;
  val class_expr : Gram.t Ast.class_expr;
  val class_expr_quot : Gram.t Ast.class_expr;
  val class_fun_binding : Gram.t Ast.class_expr;
  val class_fun_def : Gram.t Ast.class_expr;
  val class_info_for_class_expr : Gram.t Ast.class_expr;
  val class_info_for_class_type : Gram.t Ast.class_type;
  val class_longident : Gram.t Ast.ident;
  val class_longident_and_param : Gram.t Ast.class_expr;
  val class_name_and_param : Gram.t (string * Ast.ctyp);
  val class_sig_item : Gram.t Ast.class_sig_item;
  val class_sig_item_quot : Gram.t Ast.class_sig_item;
  val class_signature : Gram.t Ast.class_sig_item;
  val class_str_item : Gram.t Ast.class_str_item;
  val class_str_item_quot : Gram.t Ast.class_str_item;
  val class_structure : Gram.t Ast.class_str_item;
  val class_type : Gram.t Ast.class_type;
  val class_type_declaration : Gram.t Ast.class_type;
  val class_type_longident : Gram.t Ast.ident;
  val class_type_longident_and_param : Gram.t Ast.class_type;
  val class_type_plus : Gram.t Ast.class_type;
  val class_type_quot : Gram.t Ast.class_type;
  val comma_ctyp : Gram.t Ast.ctyp;
  val comma_expr : Gram.t Ast.expr;
  val comma_ipatt : Gram.t Ast.patt;
  val comma_patt : Gram.t Ast.patt;
  val comma_type_parameter : Gram.t Ast.ctyp;
  val constrain : Gram.t (Ast.ctyp * Ast.ctyp);
  val constructor_arg_list : Gram.t Ast.ctyp;
  val constructor_declaration : Gram.t Ast.ctyp;
  val constructor_declarations : Gram.t Ast.ctyp;
  val ctyp : Gram.t Ast.ctyp;
  val ctyp_quot : Gram.t Ast.ctyp;
  val cvalue_binding : Gram.t Ast.expr;
  val direction_flag : Gram.t Ast.direction_flag;
  val direction_flag_quot : Gram.t Ast.direction_flag;
  val dummy : Gram.t unit;
  val eq_expr : Gram.t (string -> Ast.patt -> Ast.patt);
  val expr : Gram.t Ast.expr;
  val expr_eoi : Gram.t Ast.expr;
  val expr_quot : Gram.t Ast.expr;
  val field_expr : Gram.t Ast.rec_binding;
  val field_expr_list : Gram.t Ast.rec_binding;
  val fun_binding : Gram.t Ast.expr;
  val fun_def : Gram.t Ast.expr;
  val ident : Gram.t Ast.ident;
  val ident_quot : Gram.t Ast.ident;
  val ipatt : Gram.t Ast.patt;
  val ipatt_tcon : Gram.t Ast.patt;
  val patt_tcon : Gram.t Ast.patt;    
  val label : Gram.t string;
  val label_declaration : Gram.t Ast.ctyp;
  val label_declaration_list : Gram.t Ast.ctyp;
  val label_expr : Gram.t Ast.rec_binding;
  val label_expr_list : Gram.t Ast.rec_binding;
  val label_longident : Gram.t Ast.ident;
  val label_patt : Gram.t Ast.patt;
  val label_patt_list : Gram.t Ast.patt;
  val let_binding : Gram.t Ast.binding;
  val meth_list : Gram.t (Ast.ctyp * Ast.row_var_flag);
  val meth_decl : Gram.t Ast.ctyp;
  val module_binding : Gram.t Ast.module_binding;
  val module_binding0 : Gram.t Ast.module_expr;
  val module_binding_quot : Gram.t Ast.module_binding;
  val module_declaration : Gram.t Ast.module_type;
  val module_expr : Gram.t Ast.module_expr;
  val module_expr_quot : Gram.t Ast.module_expr;
  val module_longident : Gram.t Ast.ident;
  val module_longident_with_app : Gram.t Ast.ident;
  val module_rec_declaration : Gram.t Ast.module_binding;
  val module_type : Gram.t Ast.module_type;
  val package_type : Gram.t Ast.module_type;
  val module_type_quot : Gram.t Ast.module_type;
  val more_ctyp : Gram.t Ast.ctyp;
  val name_tags : Gram.t Ast.ctyp;
  val opt_as_lident : Gram.t string;
  val opt_class_self_patt : Gram.t Ast.patt;
  val opt_class_self_type : Gram.t Ast.ctyp;
  val opt_comma_ctyp : Gram.t Ast.ctyp;
  val opt_dot_dot : Gram.t Ast.row_var_flag;
  val row_var_flag_quot : Gram.t Ast.row_var_flag;
  val opt_eq_ctyp : Gram.t Ast.ctyp;
  val opt_expr : Gram.t Ast.expr;
  val opt_meth_list : Gram.t Ast.ctyp;
  val opt_mutable : Gram.t Ast.mutable_flag;
  val mutable_flag_quot : Gram.t Ast.mutable_flag;
  val opt_override : Gram.t Ast.override_flag;
  val override_flag_quot : Gram.t Ast.override_flag;
  val opt_polyt : Gram.t Ast.ctyp;
  val opt_private : Gram.t Ast.private_flag;
  val private_flag_quot : Gram.t Ast.private_flag;
  val opt_rec : Gram.t Ast.rec_flag;
  val rec_flag_quot : Gram.t Ast.rec_flag;
  val opt_virtual : Gram.t Ast.virtual_flag;
  val virtual_flag_quot : Gram.t Ast.virtual_flag;
  val patt : Gram.t Ast.patt;
  val patt_as_patt_opt : Gram.t Ast.patt;
  val patt_eoi : Gram.t Ast.patt;
  val patt_quot : Gram.t Ast.patt;
  val poly_type : Gram.t Ast.ctyp;
  val row_field : Gram.t Ast.ctyp;
  val sem_expr : Gram.t Ast.expr;
  val sem_expr_for_list : Gram.t (Ast.expr -> Ast.expr);
  val sem_patt : Gram.t Ast.patt;
  val sem_patt_for_list : Gram.t (Ast.patt -> Ast.patt);
  val semi : Gram.t unit;
  val sequence : Gram.t Ast.expr;
  val sig_item : Gram.t Ast.sig_item;
  val sig_item_quot : Gram.t Ast.sig_item;
  val sig_items : Gram.t Ast.sig_item;
  val star_ctyp : Gram.t Ast.ctyp;
  val str_item : Gram.t Ast.str_item;
  val str_item_quot : Gram.t Ast.str_item;
  val str_items : Gram.t Ast.str_item;
  val type_constraint : Gram.t unit;
  val type_declaration : Gram.t Ast.ctyp;
  val type_ident_and_parameters : Gram.t (string * list Ast.ctyp);
  val type_kind : Gram.t Ast.ctyp;
  val type_longident : Gram.t Ast.ident;
  val type_longident_and_parameters : Gram.t Ast.ctyp;
  val type_parameter : Gram.t Ast.ctyp;
  val type_parameters : Gram.t (Ast.ctyp -> Ast.ctyp);
  val typevars : Gram.t Ast.ctyp;
  val val_longident : Gram.t Ast.ident;
  val with_constr : Gram.t Ast.with_constr;
  val with_constr_quot : Gram.t Ast.with_constr;
  val prefixop : Gram.t Ast.expr;
  val infixop0 : Gram.t Ast.expr;
  val infixop1 : Gram.t Ast.expr;
  val infixop2 : Gram.t Ast.expr;
  val infixop3 : Gram.t Ast.expr;
  val infixop4 : Gram.t Ast.expr;

  val string_list: Gram.t (Ast.meta_list string);
  val infixop5: Gram.t Ast.expr;
  val infixop6: Gram.t Ast.expr;
  val module_longident_dot_lparen: Gram.t Ast.ident;
  val sequence':Gram.t (Ast.expr -> Lib.Expr.Ast.expr); 
  val fun_def: Gram.t Ast.expr;
  val optional_type_parameter:  Gram.t Ast.ctyp;
  val method_opt_override: Gram.t Ast.override_flag;
  val value_val_opt_override: Gram.t Ast.override_flag;
  val unquoted_typevars:Gram.t Ast.ctyp;
  val lang: Gram.t string;
  val symbol:  Gram.t FanGrammar.symbol ;
  val rule:  Gram.t FanGrammar.rule;
  val rule_list: Gram.t (list FanGrammar.rule);
  val psymbol: Gram.t FanGrammar.symbol;
  val level:  Gram.t FanGrammar.level;
  val level_list: Gram.t (list FanGrammar.level);
  val entry: Gram.t FanGrammar.entry;
  val extend_body: Gram.t Ast.expr;
  val delete_rule_body: Gram.t Ast.expr;

  val parse_expr: FanLoc.t -> string -> Ast.expr;
    (**  generally "patt; EOI". *)
  val parse_patt: FanLoc.t -> string -> Ast.patt;

  val parse_ident: FanLoc.t -> string -> Ast.ident;

  val expr_filter: Ast.expr -> Ast.expr;
  val patt_filter: Ast.patt -> Ast.patt;
  (* val anti_filter: Ast.map ;   *)
  module Options:sig
    type spec_list = list (string * FanArg.spec * string);
    val init : spec_list -> unit;
    val add : (string * FanArg.spec * string) -> unit;
    val adds : list (string * FanArg.spec * string) -> unit;
    val init_spec_list: ref spec_list;
  end;
end;


module type SyntaxExtension = functor (Syn : Camlp4Syntax)
  -> Camlp4Syntax;

module type PLUGIN = functor (Unit:sig end) -> sig end;
module type SyntaxPlugin = functor (Syn:Camlp4Syntax) -> sig end ;

(** generate two printers to be registered*)
module type PrinterPlugin = functor (Syn:Camlp4Syntax) -> PrinterImpl;

(* module type OCAML_PARSER = functor (Ast:Camlp4Ast) -> Parser.S ; *)
module type ParserPlugin = functor (Syn:Camlp4Syntax) -> ParserImpl;
(* module type ASTFILTER_PLUGIN  = functor (F:AstFilters.S) -> sig end ; *)


type parser_fun 'a =
    ?directive_handler:('a -> option 'a) -> FanLoc.t -> XStream.t char -> 'a;
type printer_fun 'a =
      ?input_file:string -> ?output_file:string -> 'a -> unit;
module type PRECAST = sig
  module Syntax     : Camlp4Syntax ;
  module Printers : sig
    module OCaml         : PrinterImpl;
    module DumpOCamlAst  : PrinterImpl;
    module DumpCamlp4Ast : PrinterImpl;
    module Null          : PrinterImpl;
  end;

  val loaded_modules : ref (list string);
  val iter_and_take_callbacks : ((string * (unit -> unit)) -> unit) -> unit ;
  val register_str_item_parser : parser_fun Ast.str_item -> unit;
  val register_sig_item_parser : parser_fun Ast.sig_item -> unit;
  val register_parser :
      parser_fun Ast.str_item -> parser_fun Ast.sig_item -> unit;
  val current_parser :
      unit -> (parser_fun Ast.str_item * parser_fun Ast.sig_item);

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


  val register_str_item_printer : printer_fun Ast.str_item -> unit;
  val register_sig_item_printer : printer_fun Ast.sig_item -> unit;
  val register_printer :
      printer_fun Ast.str_item -> printer_fun Ast.sig_item -> unit;
  val current_printer :
      unit -> (printer_fun Ast.str_item * printer_fun Ast.sig_item);
        
  val declare_dyn_module : string -> (unit -> unit) -> unit  ;

  module CurrentParser : ParserImpl;
  module CurrentPrinter : PrinterImpl;
end ;


(* for dynamic loading *)
module type PRECAST_PLUGIN = sig
  val apply : (module PRECAST) -> unit;
end;


