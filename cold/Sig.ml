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
type ('a,'loc) stream_filter = ('a* 'loc) XStream.t -> ('a* 'loc) XStream.t 
module type ParserImpl =
  sig
    val parse_implem :
      ?directive_handler:(str_item -> str_item option) ->
        FanLoc.t -> char XStream.t -> str_item
    val parse_interf :
      ?directive_handler:(sig_item -> sig_item option) ->
        FanLoc.t -> char XStream.t -> sig_item
  end
module type PrinterImpl =
  sig
    val print_interf :
      ?input_file:string -> ?output_file:string -> sig_item -> unit
    val print_implem :
      ?input_file:string -> ?output_file:string -> str_item -> unit
  end
module type Syntax =
  sig
    include Warning
    include ParserImpl
    include PrinterImpl
    val interf : (sig_item list* FanLoc.t option) Gram.t
    val implem : (str_item list* FanLoc.t option) Gram.t
    val top_phrase : str_item option Gram.t
    val a_string : astring Gram.t
    val a_lident : [ `Lid of (loc* string) | ant] Gram.t
    val a_uident : auident Gram.t
    val aident : ident Gram.t
    val astr : astring Gram.t
    val luident : string Gram.t
    val amp_ctyp : ctyp Gram.t
    val and_ctyp : ctyp Gram.t
    val match_case : match_case Gram.t
    val match_case0 : match_case Gram.t
    val match_case_quot : match_case Gram.t
    val binding : binding Gram.t
    val binding_quot : binding Gram.t
    val rec_expr_quot : rec_expr Gram.t
    val class_declaration : class_expr Gram.t
    val class_description : class_type Gram.t
    val class_expr : class_expr Gram.t
    val class_expr_quot : class_expr Gram.t
    val class_fun_binding : class_expr Gram.t
    val class_fun_def : class_expr Gram.t
    val class_info_for_class_expr : class_expr Gram.t
    val class_info_for_class_type : class_type Gram.t
    val class_longident : ident Gram.t
    val class_longident_and_param : class_expr Gram.t
    val class_name_and_param : (alident* ctyp) Gram.t
    val class_sig_item : class_sig_item Gram.t
    val class_sig_item_quot : class_sig_item Gram.t
    val class_signature : class_sig_item Gram.t
    val class_str_item : class_str_item Gram.t
    val class_str_item_quot : class_str_item Gram.t
    val class_structure : class_str_item Gram.t
    val class_type : class_type Gram.t
    val class_type_declaration : class_type Gram.t
    val class_type_longident : ident Gram.t
    val class_type_longident_and_param : class_type Gram.t
    val class_type_plus : class_type Gram.t
    val class_type_quot : class_type Gram.t
    val comma_ctyp : ctyp Gram.t
    val comma_expr : expr Gram.t
    val comma_ipatt : patt Gram.t
    val comma_patt : patt Gram.t
    val comma_type_parameter : ctyp Gram.t
    val constrain : (ctyp* ctyp) Gram.t
    val constructor_arg_list : ctyp Gram.t
    val constructor_declaration : ctyp Gram.t
    val constructor_declarations : ctyp Gram.t
    val ctyp : ctyp Gram.t
    val ctyp_quot : ctyp Gram.t
    val cvalue_binding : expr Gram.t
    val direction_flag : direction_flag Gram.t
    val direction_flag_quot : direction_flag Gram.t
    val dummy : unit Gram.t
    val eq_expr : (alident -> patt -> patt) Gram.t
    val expr : expr Gram.t
    val expr_eoi : expr Gram.t
    val expr_quot : expr Gram.t
    val field_expr : rec_expr Gram.t
    val field_expr_list : rec_expr Gram.t
    val fun_binding : expr Gram.t
    val fun_def : expr Gram.t
    val ident : ident Gram.t
    val ident_quot : ident Gram.t
    val ipatt : patt Gram.t
    val ipatt_tcon : patt Gram.t
    val patt_tcon : patt Gram.t
    val label_declaration : ctyp Gram.t
    val label_declaration_list : ctyp Gram.t
    val label_expr : rec_expr Gram.t
    val label_expr_list : rec_expr Gram.t
    val label_longident : ident Gram.t
    val label_patt : rec_patt Gram.t
    val label_patt_list : rec_patt Gram.t
    val let_binding : binding Gram.t
    val meth_list : (ctyp* row_var_flag) Gram.t
    val meth_decl : ctyp Gram.t
    val module_binding : module_binding Gram.t
    val module_binding0 : module_expr Gram.t
    val module_binding_quot : module_binding Gram.t
    val module_declaration : module_type Gram.t
    val module_expr : module_expr Gram.t
    val module_expr_quot : module_expr Gram.t
    val module_longident : ident Gram.t
    val module_longident_with_app : ident Gram.t
    val module_rec_declaration : module_binding Gram.t
    val module_type : module_type Gram.t
    val module_type_quot : module_type Gram.t
    val more_ctyp : ctyp Gram.t
    val name_tags : ctyp Gram.t
    val opt_as_lident : alident meta_option Gram.t
    val opt_class_self_patt : patt Gram.t
    val opt_class_self_type : ctyp Gram.t
    val opt_comma_ctyp : ctyp Gram.t
    val opt_dot_dot : row_var_flag Gram.t
    val row_var_flag_quot : row_var_flag Gram.t
    val opt_expr : expr Gram.t
    val opt_meth_list : ctyp Gram.t
    val opt_mutable : mutable_flag Gram.t
    val mutable_flag_quot : mutable_flag Gram.t
    val opt_override : override_flag Gram.t
    val override_flag_quot : override_flag Gram.t
    val opt_polyt : ctyp Gram.t
    val opt_private : private_flag Gram.t
    val private_flag_quot : private_flag Gram.t
    val opt_rec : rec_flag Gram.t
    val rec_flag_quot : rec_flag Gram.t
    val opt_virtual : virtual_flag Gram.t
    val virtual_flag_quot : virtual_flag Gram.t
    val patt : patt Gram.t
    val patt_as_patt_opt : patt Gram.t
    val patt_eoi : patt Gram.t
    val patt_quot : patt Gram.t
    val row_field : ctyp Gram.t
    val sem_expr : expr Gram.t
    val sem_expr_for_list : (expr -> expr) Gram.t
    val sem_patt : patt Gram.t
    val sem_patt_for_list : (patt -> patt) Gram.t
    val semi : unit Gram.t
    val sequence : expr Gram.t
    val sig_item : sig_item Gram.t
    val sig_item_quot : sig_item Gram.t
    val sig_items : sig_item Gram.t
    val star_ctyp : ctyp Gram.t
    val str_item : str_item Gram.t
    val str_item_quot : str_item Gram.t
    val str_items : str_item Gram.t
    val type_declaration : ctyp Gram.t
    val type_ident_and_parameters : (alident* ctyp list) Gram.t
    val type_longident : ident Gram.t
    val type_longident_and_parameters : ctyp Gram.t
    val type_parameter : ctyp Gram.t
    val type_parameters : (ctyp -> ctyp) Gram.t
    val typevars : ctyp Gram.t
    val val_longident : ident Gram.t
    val with_constr : with_constr Gram.t
    val with_constr_quot : with_constr Gram.t
    val prefixop : expr Gram.t
    val infixop0 : expr Gram.t
    val infixop1 : expr Gram.t
    val infixop2 : expr Gram.t
    val infixop3 : expr Gram.t
    val infixop4 : expr Gram.t
    val string_list : string meta_list Gram.t
    val infixop5 : expr Gram.t
    val infixop6 : expr Gram.t
    val module_longident_dot_lparen : ident Gram.t
    val sequence' : (expr -> expr) Gram.t
    val fun_def : expr Gram.t
    val method_opt_override : override_flag Gram.t
    val value_val_opt_override : override_flag Gram.t
    val unquoted_typevars : ctyp Gram.t
    val lang : FanToken.name Gram.t
    val symbol : FanGrammar.symbol Gram.t
    val rule : FanGrammar.rule Gram.t
    val rule_list : FanGrammar.rule list Gram.t
    val psymbol : FanGrammar.symbol Gram.t
    val level : FanGrammar.level Gram.t
    val level_list :
      [ `Group of FanGrammar.level list | `Single of FanGrammar.level] Gram.t
    val entry : FanGrammar.entry Gram.t
    val extend_body : expr Gram.t
    val delete_rule_body : expr Gram.t
    val dot_lstrings : FanToken.name Gram.t
    val parse_expr : FanLoc.t -> string -> expr
    val parse_patt : FanLoc.t -> string -> patt
    val parse_ident : FanLoc.t -> string -> ident
    val expr_filter : expr -> expr
    val patt_filter : patt -> patt
    val dot_namespace : string list Gram.t
    module Options :
    sig
      type spec_list = (string* FanArg.spec* string) list 
      val init : spec_list -> unit
      val add : (string* FanArg.spec* string) -> unit
      val adds : (string* FanArg.spec* string) list -> unit
      val init_spec_list : spec_list ref
    end
  end
module type SyntaxExtension = functor (Syn : Syntax) -> Syntax
module type PLUGIN = functor (Unit : sig  end) -> sig  end
module type SyntaxPlugin = functor (Syn : Syntax) -> sig  end
module type PrinterPlugin = functor (Syn : Syntax) -> PrinterImpl
module type ParserPlugin = functor (Syn : Syntax) -> ParserImpl
type 'a parser_fun =
  ?directive_handler:('a -> 'a option) -> FanLoc.t -> char XStream.t -> 'a 
type 'a printer_fun = ?input_file:string -> ?output_file:string -> 'a -> unit 
module type PRECAST =
  sig
    module Syntax : Syntax
    module Printers :
    sig
      module OCaml : PrinterImpl
      module DumpOCamlAst : PrinterImpl
      module DumpCamlp4Ast : PrinterImpl
      module Null : PrinterImpl
    end
    val loaded_modules : string list ref
    val iter_and_take_callbacks : ((string* (unit -> unit)) -> unit) -> unit
    val register_str_item_parser : str_item parser_fun -> unit
    val register_sig_item_parser : sig_item parser_fun -> unit
    val register_parser : str_item parser_fun -> sig_item parser_fun -> unit
    val current_parser : unit -> (str_item parser_fun* sig_item parser_fun)
    val plugin : (module Id) -> (module PLUGIN) -> unit
    val syntax_plugin : (module Id) -> (module SyntaxPlugin) -> unit
    val syntax_extension : (module Id) -> (module SyntaxExtension) -> unit
    val printer_plugin : (module Id) -> (module PrinterPlugin) -> unit
    val replace_printer : (module Id) -> (module PrinterImpl) -> unit
    val replace_parser : (module Id) -> (module ParserImpl) -> unit
    val parser_plugin : (module Id) -> (module ParserPlugin) -> unit
    val enable_ocaml_printer : unit -> unit
    val enable_dump_ocaml_ast_printer : unit -> unit
    val enable_dump_camlp4_ast_printer : unit -> unit
    val enable_null_printer : unit -> unit
    val enable_auto : (unit -> bool) -> unit
    val register_str_item_printer : str_item printer_fun -> unit
    val register_sig_item_printer : sig_item printer_fun -> unit
    val register_printer :
      str_item printer_fun -> sig_item printer_fun -> unit
    val current_printer :
      unit -> (str_item printer_fun* sig_item printer_fun)
    val declare_dyn_module : string -> (unit -> unit) -> unit
    module CurrentParser : ParserImpl
    module CurrentPrinter : PrinterImpl
  end
module type PRECAST_PLUGIN = sig val apply : (module PRECAST) -> unit end