module type Id = sig
  value name : string;
  value version : string;
end;
module type Warning = sig
  type warning = FanLoc.t -> string -> unit;
  value default_warning: warning;
  value current_warning: ref warning;
  value print_warning: warning;  
end;

(** A type for stream filters. *)
type stream_filter 'a 'loc = Stream.t ('a * 'loc) -> Stream.t ('a * 'loc);

module type ParserExpr = sig
    (**generally "expr; EOI". *)
  value parse_expr : FanLoc.t -> string -> Ast.expr;
    (**  generally "patt; EOI". *)
  value parse_patt : FanLoc.t -> string -> Ast.patt;
end;
module type ParserImpl = sig
  (** When  the parser encounter a directive it stops (since the directive may change  the
      syntax), the given [directive_handler] function  evaluates  it  and
      the parsing starts again. *)
  value parse_implem : ?directive_handler:(Ast.str_item -> option Ast.str_item) ->
    FanLoc.t -> Stream.t char -> Ast.str_item;

  value parse_interf : ?directive_handler:(Ast.sig_item -> option Ast.sig_item) ->
        FanLoc.t -> Stream.t char -> Ast.sig_item;
end;

module type PrinterImpl = sig
  value print_interf : ?input_file:string -> ?output_file:string ->
    Ast.sig_item -> unit;
  value print_implem : ?input_file:string -> ?output_file:string ->
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
  value interf : Gram.Entry.t (list Ast.sig_item * option FanLoc.t);
  value implem : Gram.Entry.t (list Ast.str_item * option FanLoc.t);
  value top_phrase : Gram.Entry.t (option Ast.str_item);
  value use_file : Gram.Entry.t (list Ast.str_item * option FanLoc.t);
  value a_CHAR : Gram.Entry.t string;
  value a_FLOAT : Gram.Entry.t string;
  value a_INT : Gram.Entry.t string;
  value a_INT32 : Gram.Entry.t string;
  value a_INT64 : Gram.Entry.t string;
  value a_LABEL : Gram.Entry.t string;
  value a_LIDENT : Gram.Entry.t string;
  value a_NATIVEINT : Gram.Entry.t string;
  value a_OPTLABEL : Gram.Entry.t string;
  value a_STRING : Gram.Entry.t string;
  value a_UIDENT : Gram.Entry.t string;
  value a_ident : Gram.Entry.t string;
  value amp_ctyp : Gram.Entry.t Ast.ctyp;
  value and_ctyp : Gram.Entry.t Ast.ctyp;
  value match_case : Gram.Entry.t Ast.match_case;
  value match_case0 : Gram.Entry.t Ast.match_case;
  value match_case_quot : Gram.Entry.t Ast.match_case;
  value binding : Gram.Entry.t Ast.binding;
  value binding_quot : Gram.Entry.t Ast.binding;
  value rec_binding_quot : Gram.Entry.t Ast.rec_binding;
  value class_declaration : Gram.Entry.t Ast.class_expr;
  value class_description : Gram.Entry.t Ast.class_type;
  value class_expr : Gram.Entry.t Ast.class_expr;
  value class_expr_quot : Gram.Entry.t Ast.class_expr;
  value class_fun_binding : Gram.Entry.t Ast.class_expr;
  value class_fun_def : Gram.Entry.t Ast.class_expr;
  value class_info_for_class_expr : Gram.Entry.t Ast.class_expr;
  value class_info_for_class_type : Gram.Entry.t Ast.class_type;
  value class_longident : Gram.Entry.t Ast.ident;
  value class_longident_and_param : Gram.Entry.t Ast.class_expr;
  value class_name_and_param : Gram.Entry.t (string * Ast.ctyp);
  value class_sig_item : Gram.Entry.t Ast.class_sig_item;
  value class_sig_item_quot : Gram.Entry.t Ast.class_sig_item;
  value class_signature : Gram.Entry.t Ast.class_sig_item;
  value class_str_item : Gram.Entry.t Ast.class_str_item;
  value class_str_item_quot : Gram.Entry.t Ast.class_str_item;
  value class_structure : Gram.Entry.t Ast.class_str_item;
  value class_type : Gram.Entry.t Ast.class_type;
  value class_type_declaration : Gram.Entry.t Ast.class_type;
  value class_type_longident : Gram.Entry.t Ast.ident;
  value class_type_longident_and_param : Gram.Entry.t Ast.class_type;
  value class_type_plus : Gram.Entry.t Ast.class_type;
  value class_type_quot : Gram.Entry.t Ast.class_type;
  value comma_ctyp : Gram.Entry.t Ast.ctyp;
  value comma_expr : Gram.Entry.t Ast.expr;
  value comma_ipatt : Gram.Entry.t Ast.patt;
  value comma_patt : Gram.Entry.t Ast.patt;
  value comma_type_parameter : Gram.Entry.t Ast.ctyp;
  value constrain : Gram.Entry.t (Ast.ctyp * Ast.ctyp);
  value constructor_arg_list : Gram.Entry.t Ast.ctyp;
  value constructor_declaration : Gram.Entry.t Ast.ctyp;
  value constructor_declarations : Gram.Entry.t Ast.ctyp;
  value ctyp : Gram.Entry.t Ast.ctyp;
  value ctyp_quot : Gram.Entry.t Ast.ctyp;
  value cvalue_binding : Gram.Entry.t Ast.expr;
  value direction_flag : Gram.Entry.t Ast.direction_flag;
  value direction_flag_quot : Gram.Entry.t Ast.direction_flag;
  value dummy : Gram.Entry.t unit;
  value eq_expr : Gram.Entry.t (string -> Ast.patt -> Ast.patt);
  value expr : Gram.Entry.t Ast.expr;
  value expr_eoi : Gram.Entry.t Ast.expr;
  value expr_quot : Gram.Entry.t Ast.expr;
  value field_expr : Gram.Entry.t Ast.rec_binding;
  value field_expr_list : Gram.Entry.t Ast.rec_binding;
  value fun_binding : Gram.Entry.t Ast.expr;
  value fun_def : Gram.Entry.t Ast.expr;
  value ident : Gram.Entry.t Ast.ident;
  value ident_quot : Gram.Entry.t Ast.ident;
  value ipatt : Gram.Entry.t Ast.patt;
  value ipatt_tcon : Gram.Entry.t Ast.patt;
  value label : Gram.Entry.t string;
  value label_declaration : Gram.Entry.t Ast.ctyp;
  value label_declaration_list : Gram.Entry.t Ast.ctyp;
  value label_expr : Gram.Entry.t Ast.rec_binding;
  value label_expr_list : Gram.Entry.t Ast.rec_binding;
  value label_ipatt : Gram.Entry.t Ast.patt;
  value label_ipatt_list : Gram.Entry.t Ast.patt;
  value label_longident : Gram.Entry.t Ast.ident;
  value label_patt : Gram.Entry.t Ast.patt;
  value label_patt_list : Gram.Entry.t Ast.patt;
  value labeled_ipatt : Gram.Entry.t Ast.patt;
  value let_binding : Gram.Entry.t Ast.binding;
  value meth_list : Gram.Entry.t (Ast.ctyp * Ast.row_var_flag);
  value meth_decl : Gram.Entry.t Ast.ctyp;
  value module_binding : Gram.Entry.t Ast.module_binding;
  value module_binding0 : Gram.Entry.t Ast.module_expr;
  value module_binding_quot : Gram.Entry.t Ast.module_binding;
  value module_declaration : Gram.Entry.t Ast.module_type;
  value module_expr : Gram.Entry.t Ast.module_expr;
  value module_expr_quot : Gram.Entry.t Ast.module_expr;
  value module_longident : Gram.Entry.t Ast.ident;
  value module_longident_with_app : Gram.Entry.t Ast.ident;
  value module_rec_declaration : Gram.Entry.t Ast.module_binding;
  value module_type : Gram.Entry.t Ast.module_type;
  value package_type : Gram.Entry.t Ast.module_type;
  value module_type_quot : Gram.Entry.t Ast.module_type;
  value more_ctyp : Gram.Entry.t Ast.ctyp;
  value name_tags : Gram.Entry.t Ast.ctyp;
  value opt_as_lident : Gram.Entry.t string;
  value opt_class_self_patt : Gram.Entry.t Ast.patt;
  value opt_class_self_type : Gram.Entry.t Ast.ctyp;
  value opt_comma_ctyp : Gram.Entry.t Ast.ctyp;
  value opt_dot_dot : Gram.Entry.t Ast.row_var_flag;
  value row_var_flag_quot : Gram.Entry.t Ast.row_var_flag;
  value opt_eq_ctyp : Gram.Entry.t Ast.ctyp;
  value opt_expr : Gram.Entry.t Ast.expr;
  value opt_meth_list : Gram.Entry.t Ast.ctyp;
  value opt_mutable : Gram.Entry.t Ast.mutable_flag;
  value mutable_flag_quot : Gram.Entry.t Ast.mutable_flag;
  value opt_override : Gram.Entry.t Ast.override_flag;
  value override_flag_quot : Gram.Entry.t Ast.override_flag;
  value opt_polyt : Gram.Entry.t Ast.ctyp;
  value opt_private : Gram.Entry.t Ast.private_flag;
  value private_flag_quot : Gram.Entry.t Ast.private_flag;
  value opt_rec : Gram.Entry.t Ast.rec_flag;
  value rec_flag_quot : Gram.Entry.t Ast.rec_flag;
  value opt_virtual : Gram.Entry.t Ast.virtual_flag;
  value virtual_flag_quot : Gram.Entry.t Ast.virtual_flag;
  value opt_when_expr : Gram.Entry.t Ast.expr;
  value patt : Gram.Entry.t Ast.patt;
  value patt_as_patt_opt : Gram.Entry.t Ast.patt;
  value patt_eoi : Gram.Entry.t Ast.patt;
  value patt_quot : Gram.Entry.t Ast.patt;
  value patt_tcon : Gram.Entry.t Ast.patt;
  value phrase : Gram.Entry.t Ast.str_item;
  value poly_type : Gram.Entry.t Ast.ctyp;
  value row_field : Gram.Entry.t Ast.ctyp;
  value sem_expr : Gram.Entry.t Ast.expr;
  value sem_expr_for_list : Gram.Entry.t (Ast.expr -> Ast.expr);
  value sem_patt : Gram.Entry.t Ast.patt;
  value sem_patt_for_list : Gram.Entry.t (Ast.patt -> Ast.patt);
  value semi : Gram.Entry.t unit;
  value sequence : Gram.Entry.t Ast.expr;
  value do_sequence : Gram.Entry.t Ast.expr;
  value sig_item : Gram.Entry.t Ast.sig_item;
  value sig_item_quot : Gram.Entry.t Ast.sig_item;
  value sig_items : Gram.Entry.t Ast.sig_item;
  value star_ctyp : Gram.Entry.t Ast.ctyp;
  value str_item : Gram.Entry.t Ast.str_item;
  value str_item_quot : Gram.Entry.t Ast.str_item;
  value str_items : Gram.Entry.t Ast.str_item;
  value type_constraint : Gram.Entry.t unit;
  value type_declaration : Gram.Entry.t Ast.ctyp;
  value type_ident_and_parameters : Gram.Entry.t (string * list Ast.ctyp);
  value type_kind : Gram.Entry.t Ast.ctyp;
  value type_longident : Gram.Entry.t Ast.ident;
  value type_longident_and_parameters : Gram.Entry.t Ast.ctyp;
  value type_parameter : Gram.Entry.t Ast.ctyp;
  value type_parameters : Gram.Entry.t (Ast.ctyp -> Ast.ctyp);
  value typevars : Gram.Entry.t Ast.ctyp;
  value val_longident : Gram.Entry.t Ast.ident;
  value value_let : Gram.Entry.t unit;
  value value_val : Gram.Entry.t unit;
  value with_constr : Gram.Entry.t Ast.with_constr;
  value with_constr_quot : Gram.Entry.t Ast.with_constr;
  value prefixop : Gram.Entry.t Ast.expr;
  value infixop0 : Gram.Entry.t Ast.expr;
  value infixop1 : Gram.Entry.t Ast.expr;
  value infixop2 : Gram.Entry.t Ast.expr;
  value infixop3 : Gram.Entry.t Ast.expr;
  value infixop4 : Gram.Entry.t Ast.expr;

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
  value loaded_modules : ref (list string);
  value iter_and_take_callbacks : ((string * (unit -> unit)) -> unit) -> unit ;  
  value register_str_item_parser : parser_fun Ast.str_item -> unit;
  value register_sig_item_parser : parser_fun Ast.sig_item -> unit;
  value register_parser :
      parser_fun Ast.str_item -> parser_fun Ast.sig_item -> unit;
  value current_parser :
      unit -> (parser_fun Ast.str_item * parser_fun Ast.sig_item);

  value plugin : (module Id) -> (module PLUGIN) -> unit ;
  value syntax_plugin:(module Id) -> (module SyntaxPlugin) -> unit;  
  value syntax_extension: (module Id) -> (module SyntaxExtension) -> unit;
  value printer_plugin: (module Id) -> (module PrinterPlugin) -> unit;
  value replace_printer: (module Id) -> (module PrinterImpl) -> unit;
  value replace_parser: (module Id) -> (module ParserImpl) -> unit;
  value parser_plugin: (module Id) -> (module ParserPlugin) -> unit;
  value enable_ocaml_printer : unit -> unit;
  value enable_dump_ocaml_ast_printer : unit -> unit;
  value enable_dump_camlp4_ast_printer: unit -> unit;  
  value enable_null_printer: unit -> unit;
  value enable_auto: (unit->bool) -> unit;  


  value register_str_item_printer : printer_fun Ast.str_item -> unit;
  value register_sig_item_printer : printer_fun Ast.sig_item -> unit;
  value register_printer :
      printer_fun Ast.str_item -> printer_fun Ast.sig_item -> unit;
  value current_printer :
      unit -> (printer_fun Ast.str_item * printer_fun Ast.sig_item);
        
  value declare_dyn_module : string -> (unit -> unit) -> unit  ;

  module CurrentParser : ParserImpl;
  module CurrentPrinter : PrinterImpl;
end ;


(* for dynamic loading *)  
module type PRECAST_PLUGIN = sig
  value apply : (module PRECAST) -> unit;
end; 
