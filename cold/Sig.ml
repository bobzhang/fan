module type Id = sig val name : string  val version : string  end
module type Warning =
  sig
    type warning = FanLoc.t  -> string  -> unit  
    val default_warning : warning 
    val current_warning : warning  ref 
    val print_warning : warning 
  end
type ('a,'loc) stream_filter = ('a* 'loc) Stream.t  -> ('a* 'loc) Stream.t  
module type ParserImpl =
  sig
    val parse_implem :
      ?directive_handler:( Ast.str_item  -> Ast.str_item  option  ) ->
        FanLoc.t  -> char  Stream.t  -> Ast.str_item 
    val parse_interf :
      ?directive_handler:( Ast.sig_item  -> Ast.sig_item  option  ) ->
        FanLoc.t  -> char  Stream.t  -> Ast.sig_item 
  end
module type PrinterImpl =
  sig
    val print_interf :
      ?input_file:string  -> ?output_file:string  -> Ast.sig_item  -> unit 
    val print_implem :
      ?input_file:string  -> ?output_file:string  -> Ast.str_item  -> unit 
  end
module type Camlp4Syntax =
  sig
    module AntiquotSyntax : Quotation.AntiquotSyntax
    module Quotation : Quotation.S
    module AstFilters : AstFilters.S
    include Warning
    include ParserImpl
    include PrinterImpl
    val interf : (Ast.sig_item  list * FanLoc.t  option ) Gram.t 
    val implem : (Ast.str_item  list * FanLoc.t  option ) Gram.t 
    val top_phrase : Ast.str_item  option  Gram.t 
    val use_file : (Ast.str_item  list * FanLoc.t  option ) Gram.t 
    val a_CHAR : string  Gram.t 
    val a_FLOAT : string  Gram.t 
    val a_INT : string  Gram.t 
    val a_INT32 : string  Gram.t 
    val a_INT64 : string  Gram.t 
    val a_LABEL : string  Gram.t 
    val a_LIDENT : string  Gram.t 
    val a_NATIVEINT : string  Gram.t 
    val a_OPTLABEL : string  Gram.t 
    val a_STRING : string  Gram.t 
    val a_UIDENT : string  Gram.t 
    val a_ident : string  Gram.t 
    val amp_ctyp : Ast.ctyp  Gram.t 
    val and_ctyp : Ast.ctyp  Gram.t 
    val match_case : Ast.match_case  Gram.t 
    val match_case0 : Ast.match_case  Gram.t 
    val match_case_quot : Ast.match_case  Gram.t 
    val binding : Ast.binding  Gram.t 
    val binding_quot : Ast.binding  Gram.t 
    val rec_binding_quot : Ast.rec_binding  Gram.t 
    val class_declaration : Ast.class_expr  Gram.t 
    val class_description : Ast.class_type  Gram.t 
    val class_expr : Ast.class_expr  Gram.t 
    val class_expr_quot : Ast.class_expr  Gram.t 
    val class_fun_binding : Ast.class_expr  Gram.t 
    val class_fun_def : Ast.class_expr  Gram.t 
    val class_info_for_class_expr : Ast.class_expr  Gram.t 
    val class_info_for_class_type : Ast.class_type  Gram.t 
    val class_longident : Ast.ident  Gram.t 
    val class_longident_and_param : Ast.class_expr  Gram.t 
    val class_name_and_param : (string * Ast.ctyp ) Gram.t 
    val class_sig_item : Ast.class_sig_item  Gram.t 
    val class_sig_item_quot : Ast.class_sig_item  Gram.t 
    val class_signature : Ast.class_sig_item  Gram.t 
    val class_str_item : Ast.class_str_item  Gram.t 
    val class_str_item_quot : Ast.class_str_item  Gram.t 
    val class_structure : Ast.class_str_item  Gram.t 
    val class_type : Ast.class_type  Gram.t 
    val class_type_declaration : Ast.class_type  Gram.t 
    val class_type_longident : Ast.ident  Gram.t 
    val class_type_longident_and_param : Ast.class_type  Gram.t 
    val class_type_plus : Ast.class_type  Gram.t 
    val class_type_quot : Ast.class_type  Gram.t 
    val comma_ctyp : Ast.ctyp  Gram.t 
    val comma_expr : Ast.expr  Gram.t 
    val comma_ipatt : Ast.patt  Gram.t 
    val comma_patt : Ast.patt  Gram.t 
    val comma_type_parameter : Ast.ctyp  Gram.t 
    val constrain : (Ast.ctyp * Ast.ctyp ) Gram.t 
    val constructor_arg_list : Ast.ctyp  Gram.t 
    val constructor_declaration : Ast.ctyp  Gram.t 
    val constructor_declarations : Ast.ctyp  Gram.t 
    val ctyp : Ast.ctyp  Gram.t 
    val ctyp_quot : Ast.ctyp  Gram.t 
    val cvalue_binding : Ast.expr  Gram.t 
    val direction_flag : Ast.direction_flag  Gram.t 
    val direction_flag_quot : Ast.direction_flag  Gram.t 
    val dummy : unit  Gram.t 
    val eq_expr : ( string  -> Ast.patt  -> Ast.patt  ) Gram.t 
    val expr : Ast.expr  Gram.t 
    val expr_eoi : Ast.expr  Gram.t 
    val expr_quot : Ast.expr  Gram.t 
    val field_expr : Ast.rec_binding  Gram.t 
    val field_expr_list : Ast.rec_binding  Gram.t 
    val fun_binding : Ast.expr  Gram.t 
    val fun_def : Ast.expr  Gram.t 
    val ident : Ast.ident  Gram.t 
    val ident_quot : Ast.ident  Gram.t 
    val ipatt : Ast.patt  Gram.t 
    val ipatt_tcon : Ast.patt  Gram.t 
    val label : string  Gram.t 
    val label_declaration : Ast.ctyp  Gram.t 
    val label_declaration_list : Ast.ctyp  Gram.t 
    val label_expr : Ast.rec_binding  Gram.t 
    val label_expr_list : Ast.rec_binding  Gram.t 
    val label_ipatt : Ast.patt  Gram.t 
    val label_ipatt_list : Ast.patt  Gram.t 
    val label_longident : Ast.ident  Gram.t 
    val label_patt : Ast.patt  Gram.t 
    val label_patt_list : Ast.patt  Gram.t 
    val labeled_ipatt : Ast.patt  Gram.t 
    val let_binding : Ast.binding  Gram.t 
    val meth_list : (Ast.ctyp * Ast.row_var_flag ) Gram.t 
    val meth_decl : Ast.ctyp  Gram.t 
    val module_binding : Ast.module_binding  Gram.t 
    val module_binding0 : Ast.module_expr  Gram.t 
    val module_binding_quot : Ast.module_binding  Gram.t 
    val module_declaration : Ast.module_type  Gram.t 
    val module_expr : Ast.module_expr  Gram.t 
    val module_expr_quot : Ast.module_expr  Gram.t 
    val module_longident : Ast.ident  Gram.t 
    val module_longident_with_app : Ast.ident  Gram.t 
    val module_rec_declaration : Ast.module_binding  Gram.t 
    val module_type : Ast.module_type  Gram.t 
    val package_type : Ast.module_type  Gram.t 
    val module_type_quot : Ast.module_type  Gram.t 
    val more_ctyp : Ast.ctyp  Gram.t 
    val name_tags : Ast.ctyp  Gram.t 
    val opt_as_lident : string  Gram.t 
    val opt_class_self_patt : Ast.patt  Gram.t 
    val opt_class_self_type : Ast.ctyp  Gram.t 
    val opt_comma_ctyp : Ast.ctyp  Gram.t 
    val opt_dot_dot : Ast.row_var_flag  Gram.t 
    val row_var_flag_quot : Ast.row_var_flag  Gram.t 
    val opt_eq_ctyp : Ast.ctyp  Gram.t 
    val opt_expr : Ast.expr  Gram.t 
    val opt_meth_list : Ast.ctyp  Gram.t 
    val opt_mutable : Ast.mutable_flag  Gram.t 
    val mutable_flag_quot : Ast.mutable_flag  Gram.t 
    val opt_override : Ast.override_flag  Gram.t 
    val override_flag_quot : Ast.override_flag  Gram.t 
    val opt_polyt : Ast.ctyp  Gram.t 
    val opt_private : Ast.private_flag  Gram.t 
    val private_flag_quot : Ast.private_flag  Gram.t 
    val opt_rec : Ast.rec_flag  Gram.t 
    val rec_flag_quot : Ast.rec_flag  Gram.t 
    val opt_virtual : Ast.virtual_flag  Gram.t 
    val virtual_flag_quot : Ast.virtual_flag  Gram.t 
    val opt_when_expr : Ast.expr  Gram.t 
    val patt : Ast.patt  Gram.t 
    val patt_as_patt_opt : Ast.patt  Gram.t 
    val patt_eoi : Ast.patt  Gram.t 
    val patt_quot : Ast.patt  Gram.t 
    val patt_tcon : Ast.patt  Gram.t 
    val phrase : Ast.str_item  Gram.t 
    val poly_type : Ast.ctyp  Gram.t 
    val row_field : Ast.ctyp  Gram.t 
    val sem_expr : Ast.expr  Gram.t 
    val sem_expr_for_list : ( Ast.expr  -> Ast.expr  ) Gram.t 
    val sem_patt : Ast.patt  Gram.t 
    val sem_patt_for_list : ( Ast.patt  -> Ast.patt  ) Gram.t 
    val semi : unit  Gram.t 
    val sequence : Ast.expr  Gram.t 
    val do_sequence : Ast.expr  Gram.t 
    val sig_item : Ast.sig_item  Gram.t 
    val sig_item_quot : Ast.sig_item  Gram.t 
    val sig_items : Ast.sig_item  Gram.t 
    val star_ctyp : Ast.ctyp  Gram.t 
    val str_item : Ast.str_item  Gram.t 
    val str_item_quot : Ast.str_item  Gram.t 
    val str_items : Ast.str_item  Gram.t 
    val type_constraint : unit  Gram.t 
    val type_declaration : Ast.ctyp  Gram.t 
    val type_ident_and_parameters : (string * Ast.ctyp  list ) Gram.t 
    val type_kind : Ast.ctyp  Gram.t 
    val type_longident : Ast.ident  Gram.t 
    val type_longident_and_parameters : Ast.ctyp  Gram.t 
    val type_parameter : Ast.ctyp  Gram.t 
    val type_parameters : ( Ast.ctyp  -> Ast.ctyp  ) Gram.t 
    val typevars : Ast.ctyp  Gram.t 
    val val_longident : Ast.ident  Gram.t 
    val with_constr : Ast.with_constr  Gram.t 
    val with_constr_quot : Ast.with_constr  Gram.t 
    val prefixop : Ast.expr  Gram.t 
    val infixop0 : Ast.expr  Gram.t 
    val infixop1 : Ast.expr  Gram.t 
    val infixop2 : Ast.expr  Gram.t 
    val infixop3 : Ast.expr  Gram.t 
    val infixop4 : Ast.expr  Gram.t 
    val symbol : FanGrammar.symbol  Gram.t 
    val rule : FanGrammar.rule  Gram.t 
    val rule_list : FanGrammar.rule  list  Gram.t 
    val psymbol : FanGrammar.symbol  Gram.t 
    val level : FanGrammar.level  Gram.t 
    val level_list : FanGrammar.level  list  Gram.t 
    val entry : FanGrammar.entry  Gram.t 
  end
module type SyntaxExtension =
  functor (Syn : Camlp4Syntax) ->
    (Camlp4Syntax with module AntiquotSyntax = Syn.AntiquotSyntax and
      module Quotation = Syn.Quotation and module AstFilters =
      Syn.AstFilters)
module type PLUGIN = functor (Unit : sig  end) -> sig  end
module type SyntaxPlugin = functor (Syn : Camlp4Syntax) -> sig  end
module type PrinterPlugin = functor (Syn : Camlp4Syntax) -> PrinterImpl
module type ParserPlugin = functor (Syn : Camlp4Syntax) -> ParserImpl
type 'a parser_fun =
  ?directive_handler:( 'a -> 'a option  ) ->
    FanLoc.t  -> char  Stream.t  -> 'a
  
type 'a printer_fun =
  ?input_file:string  -> ?output_file:string  -> 'a -> unit  
module type PRECAST =
  sig
    module Syntax : Camlp4Syntax
    module Printers :
    sig
      module OCaml : PrinterImpl
      module DumpOCamlAst : PrinterImpl
      module DumpCamlp4Ast : PrinterImpl
      module Null : PrinterImpl
    end
    val loaded_modules : string  list  ref 
    val iter_and_take_callbacks :
      ( (string * ( unit  -> unit  )) -> unit  ) -> unit 
    val register_str_item_parser : Ast.str_item  parser_fun  -> unit 
    val register_sig_item_parser : Ast.sig_item  parser_fun  -> unit 
    val register_parser :
      Ast.str_item  parser_fun  -> Ast.sig_item  parser_fun  -> unit 
    val current_parser :
      unit  -> (Ast.str_item  parser_fun * Ast.sig_item  parser_fun )
    val plugin : (module Id) -> (module PLUGIN) -> unit 
    val syntax_plugin : (module Id) -> (module SyntaxPlugin) -> unit 
    val syntax_extension : (module Id) -> (module SyntaxExtension) -> unit 
    val printer_plugin : (module Id) -> (module PrinterPlugin) -> unit 
    val replace_printer : (module Id) -> (module PrinterImpl) -> unit 
    val replace_parser : (module Id) -> (module ParserImpl) -> unit 
    val parser_plugin : (module Id) -> (module ParserPlugin) -> unit 
    val enable_ocaml_printer : unit  -> unit 
    val enable_dump_ocaml_ast_printer : unit  -> unit 
    val enable_dump_camlp4_ast_printer : unit  -> unit 
    val enable_null_printer : unit  -> unit 
    val enable_auto : ( unit  -> bool  ) -> unit 
    val register_str_item_printer : Ast.str_item  printer_fun  -> unit 
    val register_sig_item_printer : Ast.sig_item  printer_fun  -> unit 
    val register_printer :
      Ast.str_item  printer_fun  -> Ast.sig_item  printer_fun  -> unit 
    val current_printer :
      unit  -> (Ast.str_item  printer_fun * Ast.sig_item  printer_fun )
    val declare_dyn_module : string  -> ( unit  -> unit  ) -> unit 
    module CurrentParser : ParserImpl
    module CurrentPrinter : PrinterImpl
  end
module type PRECAST_PLUGIN = sig val apply : (module PRECAST) -> unit  end