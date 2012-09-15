module Make (Loc: FanSig.Loc)  (Lexer: Sig.LEXER) : Sig.PRECAST
  with module Loc = Loc  = struct
  type token = FanSig.camlp4_token ;
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


    
  value plugin (module Id:Sig.Id) (module Maker:Sig.PLUGIN) = 
    declare_dyn_module Id.name (fun _ -> let module M = Maker (struct end) in ());

  value syntax_plugin (module Id:Sig.Id) (module Maker:Sig.SYNTAX_PLUGIN) =
    declare_dyn_module Id.name (fun _ -> let module M = Maker Syntax in ());
    
  value syntax_extension (module Id:Sig.Id) (module Maker:Sig.SyntaxExtension) =
    declare_dyn_module Id.name (fun _ -> let module M = Maker Syntax in ());

  value ocaml_syntax_extension (module Id:Sig.Id) (module Maker:Sig.OCAML_SYNTAX_EXTENSION) =
    declare_dyn_module Id.name (fun _ -> let module M = Maker Syntax in ());
    


  value printer (module Id:Sig.Id) (module Maker:Sig.PRINTER_PLUGIN) =
    declare_dyn_module Id.name
      (fun _ -> let module M = Maker Syntax in
      register_printer M.print_implem M.print_interf);

  (* Apply the functor and register the generated printer as the main printer *)  
  value ocaml_printer (module Id:Sig.Id) (module Maker:Sig.OCAML_PRINTER_PLUGIN) =
    declare_dyn_module Id.name
      (fun _ -> let module M = Maker Syntax in
      register_printer M.print_implem M.print_interf);

  value ocaml_precast_printer (module Id:Sig.Id) (module P:(Sig.Printer Syntax.Ast).S) =
    declare_dyn_module Id.name (fun _ ->
      register_printer P.print_implem P.print_interf);

  value parser_plugin (module Id:Sig.Id) (module Maker:Sig.PARSER) =
      declare_dyn_module Id.name
        (fun _ -> let module M = Maker Syntax.Ast in
        register_parser M.parse_implem M.parse_interf);
  value ocaml_parser_plugin (module Id:Sig.Id) (module Maker:Sig.OCAML_PARSER) =
    declare_dyn_module Id.name (fun _
      -> let module M = Maker Syntax.Ast in
      register_parser M.parse_implem M.parse_interf );

  value ocaml_precast_parser_plugin (module Id:Sig.Id)
      (module P:(Sig.Parser Syntax.Ast).S) =
    declare_dyn_module Id.name (fun _
      -> register_parser P.parse_implem P.parse_interf );
  value ast_filter (module Id:Sig.Id) (module Maker:Sig.ASTFILTER_PLUGIN) =
    declare_dyn_module Id.name (fun _
        -> let module M = Maker AstFilters in ());

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

  value enable_ocaml_printer () = begin
    Format.eprintf "enable..";
    ocaml_printer (module Printers.OCaml.Id) (module Printers.OCaml.Make);
  end;

  (* value enable_ocamlr_printer () = *)
  (*   ocaml_printer (module Printers.OCamlr.Id) (module Printers.OCamlr.MakeMore); *)

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
    (* module OCamlr = Printers.OCamlr.Make Syntax; *)
    (* module OCamlrr = Printers.OCamlrr.Make Syntax; *)
    module DumpOCamlAst = Printers.DumpOCamlAst.Make Syntax;
    module DumpCamlp4Ast = Printers.DumpCamlp4Ast.Make Syntax;
    module Null = Printers.Null.Make Syntax;
  end;
    
end; 
