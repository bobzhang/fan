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
type stream_filter 'a 'loc = Stream.t ('a * 'loc) -> Stream.t ('a * 'loc);

module type ParserExpr = sig
    (**generally "expr; EOI". *)
  val parse_expr : FanLoc.t -> string -> Ast.expr;
    (**  generally "patt; EOI". *)
  val parse_patt : FanLoc.t -> string -> Ast.patt;
end;
module type ParserImpl = sig
  (** When  the parser encounter a directive it stops (since the directive may change  the
      syntax), the given [directive_handler] function  evaluates  it  and
      the parsing starts again. *)
  val parse_implem : ?directive_handler:(Ast.str_item -> option Ast.str_item) ->
    FanLoc.t -> Stream.t char -> Ast.str_item;

  val parse_interf : ?directive_handler:(Ast.sig_item -> option Ast.sig_item) ->
        FanLoc.t -> Stream.t char -> Ast.sig_item;
end;

module type PrinterImpl = sig
  val print_interf : ?input_file:string -> ?output_file:string ->
    Ast.sig_item -> unit;
  val print_implem : ?input_file:string -> ?output_file:string ->
    Ast.str_item -> unit;
end;

module type Camlp4Syntax = sig
  module Token          : FanSig.Camlp4Token ;
  module Gram           : FanSig.Grammar.Static with module Token = Token;
  module AntiquotSyntax : ParserExpr;
  module Quotation : Quotation.S;
  module AstFilters: AstFilters.S;  
  include Warning;  
  include ParserImpl;
  include PrinterImpl;
  val interf : Gram.Entry.t (list Ast.sig_item * option FanLoc.t);
  val implem : Gram.Entry.t (list Ast.str_item * option FanLoc.t);
  val top_phrase : Gram.Entry.t (option Ast.str_item);
  val use_file : Gram.Entry.t (list Ast.str_item * option FanLoc.t);
  val a_CHAR : Gram.Entry.t string;
  val a_FLOAT : Gram.Entry.t string;
  val a_INT : Gram.Entry.t string;
  val a_INT32 : Gram.Entry.t string;
  val a_INT64 : Gram.Entry.t string;
  val a_LABEL : Gram.Entry.t string;
  val a_LIDENT : Gram.Entry.t string;
  val a_NATIVEINT : Gram.Entry.t string;
  val a_OPTLABEL : Gram.Entry.t string;
  val a_STRING : Gram.Entry.t string;
  val a_UIDENT : Gram.Entry.t string;
  val a_ident : Gram.Entry.t string;
  val amp_ctyp : Gram.Entry.t Ast.ctyp;
  val and_ctyp : Gram.Entry.t Ast.ctyp;
  val match_case : Gram.Entry.t Ast.match_case;
  val match_case0 : Gram.Entry.t Ast.match_case;
  val match_case_quot : Gram.Entry.t Ast.match_case;
  val binding : Gram.Entry.t Ast.binding;
  val binding_quot : Gram.Entry.t Ast.binding;
  val rec_binding_quot : Gram.Entry.t Ast.rec_binding;
  val class_declaration : Gram.Entry.t Ast.class_expr;
  val class_description : Gram.Entry.t Ast.class_type;
  val class_expr : Gram.Entry.t Ast.class_expr;
  val class_expr_quot : Gram.Entry.t Ast.class_expr;
  val class_fun_binding : Gram.Entry.t Ast.class_expr;
  val class_fun_def : Gram.Entry.t Ast.class_expr;
  val class_info_for_class_expr : Gram.Entry.t Ast.class_expr;
  val class_info_for_class_type : Gram.Entry.t Ast.class_type;
  val class_longident : Gram.Entry.t Ast.ident;
  val class_longident_and_param : Gram.Entry.t Ast.class_expr;
  val class_name_and_param : Gram.Entry.t (string * Ast.ctyp);
  val class_sig_item : Gram.Entry.t Ast.class_sig_item;
  val class_sig_item_quot : Gram.Entry.t Ast.class_sig_item;
  val class_signature : Gram.Entry.t Ast.class_sig_item;
  val class_str_item : Gram.Entry.t Ast.class_str_item;
  val class_str_item_quot : Gram.Entry.t Ast.class_str_item;
  val class_structure : Gram.Entry.t Ast.class_str_item;
  val class_type : Gram.Entry.t Ast.class_type;
  val class_type_declaration : Gram.Entry.t Ast.class_type;
  val class_type_longident : Gram.Entry.t Ast.ident;
  val class_type_longident_and_param : Gram.Entry.t Ast.class_type;
  val class_type_plus : Gram.Entry.t Ast.class_type;
  val class_type_quot : Gram.Entry.t Ast.class_type;
  val comma_ctyp : Gram.Entry.t Ast.ctyp;
  val comma_expr : Gram.Entry.t Ast.expr;
  val comma_ipatt : Gram.Entry.t Ast.patt;
  val comma_patt : Gram.Entry.t Ast.patt;
  val comma_type_parameter : Gram.Entry.t Ast.ctyp;
  val constrain : Gram.Entry.t (Ast.ctyp * Ast.ctyp);
  val constructor_arg_list : Gram.Entry.t Ast.ctyp;
  val constructor_declaration : Gram.Entry.t Ast.ctyp;
  val constructor_declarations : Gram.Entry.t Ast.ctyp;
  val ctyp : Gram.Entry.t Ast.ctyp;
  val ctyp_quot : Gram.Entry.t Ast.ctyp;
  val cvalue_binding : Gram.Entry.t Ast.expr;
  val direction_flag : Gram.Entry.t Ast.direction_flag;
  val direction_flag_quot : Gram.Entry.t Ast.direction_flag;
  val dummy : Gram.Entry.t unit;
  val eq_expr : Gram.Entry.t (string -> Ast.patt -> Ast.patt);
  val expr : Gram.Entry.t Ast.expr;
  val expr_eoi : Gram.Entry.t Ast.expr;
  val expr_quot : Gram.Entry.t Ast.expr;
  val field_expr : Gram.Entry.t Ast.rec_binding;
  val field_expr_list : Gram.Entry.t Ast.rec_binding;
  val fun_binding : Gram.Entry.t Ast.expr;
  val fun_def : Gram.Entry.t Ast.expr;
  val ident : Gram.Entry.t Ast.ident;
  val ident_quot : Gram.Entry.t Ast.ident;
  val ipatt : Gram.Entry.t Ast.patt;
  val ipatt_tcon : Gram.Entry.t Ast.patt;
  val label : Gram.Entry.t string;
  val label_declaration : Gram.Entry.t Ast.ctyp;
  val label_declaration_list : Gram.Entry.t Ast.ctyp;
  val label_expr : Gram.Entry.t Ast.rec_binding;
  val label_expr_list : Gram.Entry.t Ast.rec_binding;
  val label_ipatt : Gram.Entry.t Ast.patt;
  val label_ipatt_list : Gram.Entry.t Ast.patt;
  val label_longident : Gram.Entry.t Ast.ident;
  val label_patt : Gram.Entry.t Ast.patt;
  val label_patt_list : Gram.Entry.t Ast.patt;
  val labeled_ipatt : Gram.Entry.t Ast.patt;
  val let_binding : Gram.Entry.t Ast.binding;
  val meth_list : Gram.Entry.t (Ast.ctyp * Ast.row_var_flag);
  val meth_decl : Gram.Entry.t Ast.ctyp;
  val module_binding : Gram.Entry.t Ast.module_binding;
  val module_binding0 : Gram.Entry.t Ast.module_expr;
  val module_binding_quot : Gram.Entry.t Ast.module_binding;
  val module_declaration : Gram.Entry.t Ast.module_type;
  val module_expr : Gram.Entry.t Ast.module_expr;
  val module_expr_quot : Gram.Entry.t Ast.module_expr;
  val module_longident : Gram.Entry.t Ast.ident;
  val module_longident_with_app : Gram.Entry.t Ast.ident;
  val module_rec_declaration : Gram.Entry.t Ast.module_binding;
  val module_type : Gram.Entry.t Ast.module_type;
  val package_type : Gram.Entry.t Ast.module_type;
  val module_type_quot : Gram.Entry.t Ast.module_type;
  val more_ctyp : Gram.Entry.t Ast.ctyp;
  val name_tags : Gram.Entry.t Ast.ctyp;
  val opt_as_lident : Gram.Entry.t string;
  val opt_class_self_patt : Gram.Entry.t Ast.patt;
  val opt_class_self_type : Gram.Entry.t Ast.ctyp;
  val opt_comma_ctyp : Gram.Entry.t Ast.ctyp;
  val opt_dot_dot : Gram.Entry.t Ast.row_var_flag;
  val row_var_flag_quot : Gram.Entry.t Ast.row_var_flag;
  val opt_eq_ctyp : Gram.Entry.t Ast.ctyp;
  val opt_expr : Gram.Entry.t Ast.expr;
  val opt_meth_list : Gram.Entry.t Ast.ctyp;
  val opt_mutable : Gram.Entry.t Ast.mutable_flag;
  val mutable_flag_quot : Gram.Entry.t Ast.mutable_flag;
  val opt_override : Gram.Entry.t Ast.override_flag;
  val override_flag_quot : Gram.Entry.t Ast.override_flag;
  val opt_polyt : Gram.Entry.t Ast.ctyp;
  val opt_private : Gram.Entry.t Ast.private_flag;
  val private_flag_quot : Gram.Entry.t Ast.private_flag;
  val opt_rec : Gram.Entry.t Ast.rec_flag;
  val rec_flag_quot : Gram.Entry.t Ast.rec_flag;
  val opt_virtual : Gram.Entry.t Ast.virtual_flag;
  val virtual_flag_quot : Gram.Entry.t Ast.virtual_flag;
  val opt_when_expr : Gram.Entry.t Ast.expr;
  val patt : Gram.Entry.t Ast.patt;
  val patt_as_patt_opt : Gram.Entry.t Ast.patt;
  val patt_eoi : Gram.Entry.t Ast.patt;
  val patt_quot : Gram.Entry.t Ast.patt;
  val patt_tcon : Gram.Entry.t Ast.patt;
  val phrase : Gram.Entry.t Ast.str_item;
  val poly_type : Gram.Entry.t Ast.ctyp;
  val row_field : Gram.Entry.t Ast.ctyp;
  val sem_expr : Gram.Entry.t Ast.expr;
  val sem_expr_for_list : Gram.Entry.t (Ast.expr -> Ast.expr);
  val sem_patt : Gram.Entry.t Ast.patt;
  val sem_patt_for_list : Gram.Entry.t (Ast.patt -> Ast.patt);
  val semi : Gram.Entry.t unit;
  val sequence : Gram.Entry.t Ast.expr;
  val do_sequence : Gram.Entry.t Ast.expr;
  val sig_item : Gram.Entry.t Ast.sig_item;
  val sig_item_quot : Gram.Entry.t Ast.sig_item;
  val sig_items : Gram.Entry.t Ast.sig_item;
  val star_ctyp : Gram.Entry.t Ast.ctyp;
  val str_item : Gram.Entry.t Ast.str_item;
  val str_item_quot : Gram.Entry.t Ast.str_item;
  val str_items : Gram.Entry.t Ast.str_item;
  val type_constraint : Gram.Entry.t unit;
  val type_declaration : Gram.Entry.t Ast.ctyp;
  val type_ident_and_parameters : Gram.Entry.t (string * list Ast.ctyp);
  val type_kind : Gram.Entry.t Ast.ctyp;
  val type_longident : Gram.Entry.t Ast.ident;
  val type_longident_and_parameters : Gram.Entry.t Ast.ctyp;
  val type_parameter : Gram.Entry.t Ast.ctyp;
  val type_parameters : Gram.Entry.t (Ast.ctyp -> Ast.ctyp);
  val typevars : Gram.Entry.t Ast.ctyp;
  val val_longident : Gram.Entry.t Ast.ident;
  val value_let : Gram.Entry.t unit;
  val value_val : Gram.Entry.t unit;
  val with_constr : Gram.Entry.t Ast.with_constr;
  val with_constr_quot : Gram.Entry.t Ast.with_constr;
  val prefixop : Gram.Entry.t Ast.expr;
  val infixop0 : Gram.Entry.t Ast.expr;
  val infixop1 : Gram.Entry.t Ast.expr;
  val infixop2 : Gram.Entry.t Ast.expr;
  val infixop3 : Gram.Entry.t Ast.expr;
  val infixop4 : Gram.Entry.t Ast.expr;

end;

(** A signature for syntax extension (syntax -> syntax functors). *)
module type SyntaxExtension = functor (Syn : Camlp4Syntax)
  -> (Camlp4Syntax with
      module Token = Syn.Token and
      module Gram  = Syn.Gram  and
      module AntiquotSyntax = Syn.AntiquotSyntax and
      module Quotation = Syn.Quotation and
      module AstFilters = Syn.AstFilters );

module type PLUGIN = functor (Unit:sig end) -> sig end;
module type SyntaxPlugin = functor (Syn:Camlp4Syntax) -> sig end ;

(** generate two printers to be registered*)
module type PrinterPlugin = functor (Syn:Camlp4Syntax) -> PrinterImpl;   

(* module type OCAML_PARSER = functor (Ast:Camlp4Ast) -> Parser.S ; *)
module type ParserPlugin = functor (Syn:Camlp4Syntax) -> ParserImpl;
(* module type ASTFILTER_PLUGIN  = functor (F:AstFilters.S) -> sig end ; *)

module type LEXER =  functor (Token: FanSig.Camlp4Token)
  -> FanSig.Lexer with  module Token = Token;

module type PRECAST = sig
  type token = FanSig.camlp4_token ;
  module Token      : FanSig.Token  with 
                      type t = FanSig.camlp4_token;
  module Lexer      : FanSig.Lexer  with 
                      module Token = Token;
  module Gram       : FanSig.Grammar.Static  with
                      module Token = Token;
  module Syntax     : Camlp4Syntax with 
                       module Token   = Token;
  module Printers : sig
    module OCaml         : PrinterImpl;
    module DumpOCamlAst  : PrinterImpl;
    module DumpCamlp4Ast : PrinterImpl;
    module Null          : PrinterImpl;
  end;

  module MakeSyntax (U : sig end) : Camlp4Syntax;

  (* parser signature *)  
  type parser_fun 'a =
      ?directive_handler:('a -> option 'a) -> FanLoc.t -> Stream.t char -> 'a;
  type printer_fun 'a =
      ?input_file:string -> ?output_file:string -> 'a -> unit;
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
  val enable_ocaml_printer : unit -> unit;
  val enable_dump_ocaml_ast_printer : unit -> unit;
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
