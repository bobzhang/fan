module Id = struct
  value name = "Camlp4.MakePreCast";
  value version = Sys.ocaml_version;
end ;

module type S = sig
  type camlp4_token = Sig.camlp4_token ==
  [ KEYWORD       of string
  | SYMBOL        of string
  | LIDENT        of string
  | UIDENT        of string
  | ESCAPED_IDENT of string
  | INT           of int and string
  | INT32         of int32 and string
  | INT64         of int64 and string
  | NATIVEINT     of nativeint and string
  | FLOAT         of float and string
  | CHAR          of char and string
  | STRING        of string and string
  | LABEL         of string
  | OPTLABEL      of string
  | QUOTATION     of Sig.quotation
  | ANTIQUOT      of string and string
  | COMMENT       of string
  | BLANKS        of string
  | NEWLINE
  | LINE_DIRECTIVE of int and option string
  | EOI ];

  module Loc        : Sig.Loc;
  module Ast        : Sig.Camlp4Ast with module Loc = Loc;
  module Token      : Sig.Token  with module Loc = Loc and type t = camlp4_token;
  module Lexer      : Sig.Lexer  with module Loc = Loc and module Token = Token;
  module Gram       : Sig.Grammar.Static  with module Loc = Loc and module Token = Token;
  module Quotation  : Sig.Quotation with module Ast = Sig.Camlp4AstToAst Ast;
  module DynLoader  : Sig.DynLoader;
  module AstFilters : Sig.AstFilters with module Ast = Ast;
  module Syntax     : Sig.Camlp4Syntax with module Loc     = Loc
                       and module Token   = Token
                       and module Ast     = Ast
                       and module Gram    = Gram
                       and module Quotation = Quotation;
  module Printers : sig
    module OCaml         : (Sig.Printer Ast).S;
    module OCamlr        : (Sig.Printer Ast).S;
    module DumpOCamlAst  : (Sig.Printer Ast).S;
    module DumpCamlp4Ast : (Sig.Printer Ast).S;
    module Null          : (Sig.Printer Ast).S;
  end;
  module MakeGram (Lexer : Sig.Lexer with module Loc = Loc) : Sig.Grammar.Static
  with module Loc = Loc and module Token = Lexer.Token;

  module MakeSyntax (U : sig end) : Sig.Syntax;

  module FilterSyntax: Sig.FilterSyntax  with module Loc     = Loc
                       and module Token   = Token
                       and module Ast     = Ast
                       and module Gram    = Gram
                       and module Quotation = Quotation;
  (* module Register: MakeRegister.S with module Loc = Loc and module Ast = Ast;                         *)
end ;

  
module Make (Loc: Sig.Loc)
    (Lexer:
     functor (Token:Sig.Camlp4Token) ->
       Sig.Lexer with module Loc = Token.Loc and module Token = Token
    ) : S with module Loc = Loc  = struct

  type camlp4_token = Sig.camlp4_token ==
    [ KEYWORD       of string
    | SYMBOL        of string
    | LIDENT        of string
    | UIDENT        of string
    | ESCAPED_IDENT of string
    | INT           of int and string
    | INT32         of int32 and string
    | INT64         of int64 and string
    | NATIVEINT     of nativeint and string
    | FLOAT         of float and string
    | CHAR          of char and string
    | STRING        of string and string
    | LABEL         of string
    | OPTLABEL      of string
    | QUOTATION     of Sig.quotation
    | ANTIQUOT      of string and string
    | COMMENT       of string
    | BLANKS        of string
    | NEWLINE
    | LINE_DIRECTIVE of int and option string
    | EOI ];

  module Loc = Loc;      
  module Ast = Struct.Camlp4Ast.Make Loc;
  module Token = Struct.Token.Make Loc;
  module Lexer = Lexer Token;
  module Gram = Struct.Grammar.Static.Make Lexer;
  module DynLoader = Struct.DynLoader;
  module Quotation = Struct.Quotation.Make Ast;
  module MakeSyntax (U : sig end) = OCamlInitSyntax.Make Ast Gram Quotation;
  module Syntax = MakeSyntax (struct end);
  module AstFilters = Struct.AstFilters.Make Ast;
  module MakeGram = Struct.Grammar.Static.Make;
  module FilterSyntax = struct
    include Syntax;
    module AstFilters = AstFilters;
  end ;
  (* module Register = MakeRegister.Make FilterSyntax; *)

  type parser_fun 'a = ?directive_handler:('a -> option 'a) -> Syntax.Loc.t -> Stream.t char -> 'a;
  type printer_fun 'a = ?input_file:string -> ?output_file:string -> 'a -> unit;
  value sig_item_parser =
    ref (fun ?directive_handler:(_) _ _ -> failwith "No interface parser");
  value str_item_parser =
    ref (fun ?directive_handler:(_) _ _ -> failwith "No implementation parser");
  value sig_item_printer =
    ref (fun ?input_file:(_) ?output_file:(_) _ -> failwith "No interface printer");
  value str_item_printer =
    ref (fun ?input_file:(_) ?output_file:(_) _ -> failwith "No implementation printer");
  value callbacks = Queue.create ();
  value loaded_modules = ref [];
  (* iter and remove from the Queue *)  
  value iter_and_take_callbacks f =
    let rec loop () = loop (f (Queue.take callbacks)) in
    try loop () with [ Queue.Empty -> () ];
      
  value declare_dyn_module m f =
    begin
      loaded_modules.val := [ m :: loaded_modules.val ];
        Queue.add (m, f) callbacks;
    end;

  value register_str_item_parser f = str_item_parser.val := f;
  value register_sig_item_parser f = sig_item_parser.val := f;
  value register_parser f g =
    do { str_item_parser.val := f; sig_item_parser.val := g };
  value current_parser () = (str_item_parser.val, sig_item_parser.val);

  value register_str_item_printer f = str_item_printer.val := f;
  value register_sig_item_printer f = sig_item_printer.val := f;
  value register_printer f g =
    do { str_item_printer.val := f; sig_item_printer.val := g };
  value current_printer () = (str_item_printer.val, sig_item_printer.val);


  module type PLUGIN = functor (Unit:sig end) -> sig end;
    
  module type OCAML_SYNTAX_EXTENSION =
      functor (Syn:Sig.Camlp4Syntax) -> Sig.SyntaxExtension ;

  module type SYNTAX_PLUGIN = functor (Syn:Sig.Syntax) -> sig end ;
  module type PRINTER_PLUGIN = functor (Syn:Sig.Syntax) -> (Sig.Printer Syn.Ast).S;   
  module type OCAML_PRINTER_PLUGIN =
      functor (Syn:Sig.Camlp4Syntax) ->  (Sig.Printer Syn.Ast).S;
  module type PARSER = functor (Ast:Sig.Camlp4Ast) -> (Sig.Parser Ast).S;
  module type OCAML_PARSER = functor (Ast:Sig.Camlp4Ast) -> (Sig.Parser Ast).S ;
    
  value plugin (module Id:Sig.Id) (module Maker:PLUGIN) = 
    declare_dyn_module Id.name (fun _ -> let module M = Maker (struct end) in ());

  value syntax_extension (module Id:Sig.Id) (module Maker:Sig.SyntaxExtension) =
    declare_dyn_module Id.name (fun _ -> let module M = Maker Syntax in ());

  value ocaml_syntax_extension (module Id:Sig.Id) (module Maker:OCAML_SYNTAX_EXTENSION) =
    declare_dyn_module Id.name (fun _ -> let module M = Maker Syntax in ());
    

  value syntax_plugin (module Id:Sig.Id) (module Maker:SYNTAX_PLUGIN) =
    declare_dyn_module Id.name (fun _ -> let module M = Maker Syntax in ());

  value printer (module Id:Sig.Id) (module Maker:PRINTER_PLUGIN) =
    declare_dyn_module Id.name
      (fun _ -> let module M = Maker Syntax in
      register_printer M.print_implem M.print_interf);
    
  value ocaml_printer (module Id:Sig.Id) (module Maker:OCAML_PRINTER_PLUGIN) =
    declare_dyn_module Id.name
      (fun _ -> let module M = Maker Syntax in
      register_printer M.print_implem M.print_interf);

  value ocaml_precast_printer (module Id:Sig.Id) (module P:(Sig.Printer Syntax.Ast).S) =
    (declare_dyn_module Id.name (fun _ ->
      register_printer P.print_implem P.print_interf));

  module Parser
      (Id : Sig.Id) (Maker : functor (Ast : Sig.Ast)
                     -> (Sig.Parser Ast).S) =
    struct
      declare_dyn_module Id.name (fun _ ->
      let module M = Maker Syntax.Ast in
      register_parser M.parse_implem M.parse_interf);
    end;

  module OCamlParser
      (Id : Sig.Id) (Maker : functor (Ast : Sig.Camlp4Ast)
                     -> (Sig.Parser Ast).S) =
    struct
      declare_dyn_module Id.name (fun _ ->
      let module M = Maker Syntax.Ast in
      register_parser M.parse_implem M.parse_interf);
    end;

  module OCamlPreCastParser
      (Id : Sig.Id) (P : (Sig.Parser Syntax.Ast).S) =
    struct
      declare_dyn_module Id.name (fun _ ->
        register_parser P.parse_implem P.parse_interf);
    end;

  module AstFilter
      (Id : Sig.Id) (Maker : functor (F : Sig.AstFilters) -> sig end) =
    struct
      declare_dyn_module Id.name (fun _ -> let module M = Maker AstFilters in ());
    end;

  sig_item_parser.val := Syntax.parse_interf;
  str_item_parser.val := Syntax.parse_implem;

  module CurrentParser = struct
    module Ast = Syntax.Ast;
    value parse_interf ?directive_handler loc strm =
      sig_item_parser.val ?directive_handler loc strm;
    value parse_implem ?directive_handler loc strm =
      str_item_parser.val ?directive_handler loc strm;
  end;

  module CurrentPrinter = struct
    module Ast = Syntax.Ast;
    value print_interf ?input_file ?output_file ast =
      sig_item_printer.val ?input_file ?output_file ast;
    value print_implem ?input_file ?output_file ast =
      str_item_printer.val ?input_file ?output_file ast;
  end;

  (* module PP = Printers; *)
  value enable_ocaml_printer () =
    ocaml_printer (module Printers.OCaml.Id) (module Printers.OCaml.MakeMore);

  value enable_ocamlr_printer () =
    ocaml_printer (module Printers.OCamlr.Id) (module Printers.OCamlr.MakeMore);


  (* value enable_ocamlrr_printer () =
     let module M = OCamlPrinter PP.OCamlrr.Id PP.OCamlrr.MakeMore in ();    *)

  value enable_dump_ocaml_ast_printer () =
    ocaml_printer (module Printers.DumpOCamlAst.Id)
        (module Printers.DumpOCamlAst.Make);

  value enable_dump_camlp4_ast_printer () =
    printer (module Printers.DumpCamlp4Ast.Id) (module Printers.DumpCamlp4Ast.Make);

  value enable_null_printer () =
    printer (module Printers.Null.Id) (module Printers.Null.Make);

  value enable_auto isatty  =
    if isatty () then
      enable_ocaml_printer ()
    else
      enable_dump_ocaml_ast_printer ();

  (* rebound module Printers to extract most useful parts *)  
  module Printers = struct
    module OCaml = Printers.OCaml.Make Syntax;
    module OCamlr = Printers.OCamlr.Make Syntax;
    (* module OCamlrr = Printers.OCamlrr.Make Syntax; *)
    module DumpOCamlAst = Printers.DumpOCamlAst.Make Syntax;
    module DumpCamlp4Ast = Printers.DumpCamlp4Ast.Make Syntax;
    module Null = Printers.Null.Make Syntax;
  end;
    
end; 
