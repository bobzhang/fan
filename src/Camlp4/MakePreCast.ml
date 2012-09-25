module Make   (Lexer: Sig.LEXER) : Sig.PRECAST  = struct
  type token = FanSig.camlp4_token ;
  module Token = FanToken;
  module Lexer = Lexer Token;
  module Gram =  Grammar.Static.Make Lexer;
  module Quotation = Quotation.Make (struct end);
  module MakeSyntax (U : sig end) = OCamlInitSyntax.Make Gram ;
  module Syntax = MakeSyntax (struct end);
  (* module AstFilters = AstFilters.Make (struct end); *)
  type parser_fun 'a =
      ?directive_handler:('a -> option 'a) -> FanLoc.t -> Stream.t char -> 'a;
  type printer_fun 'a =
      ?input_file:string -> ?output_file:string -> 'a -> unit;
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

  value syntax_plugin (module Id:Sig.Id) (module Maker:Sig.SyntaxPlugin) =
    declare_dyn_module Id.name (fun _ -> let module M = Maker Syntax in ());
    
  value syntax_extension (module Id:Sig.Id) (module Maker:Sig.SyntaxExtension) =
    declare_dyn_module Id.name (fun _ -> let module M = Maker Syntax in ());

  value printer_plugin (module Id:Sig.Id) (module Maker:Sig.PrinterPlugin) =
    declare_dyn_module Id.name
      (fun _ -> let module M = Maker Syntax in
      register_printer M.print_implem M.print_interf);

  value replace_printer (module Id:Sig.Id) (module P:Sig.PrinterImpl) =
    declare_dyn_module Id.name (fun _ ->
      register_printer P.print_implem P.print_interf);

  value replace_parser (module Id:Sig.Id) (module Maker: Sig.ParserImpl) =
      declare_dyn_module Id.name
        (fun _ ->  register_parser Maker.parse_implem Maker.parse_interf);

  value parser_plugin (module Id:Sig.Id) (module Maker:Sig.ParserPlugin) =
    declare_dyn_module Id.name (fun _
      -> let module M = Maker Syntax in
      register_parser M.parse_implem M.parse_interf );

  value enable_ocaml_printer () = begin
    replace_printer (module Printers.OCaml.Id) (module Printers.OCaml.P);
   (* FIXME can be simplified *)
  end;

  value enable_dump_ocaml_ast_printer () =
    replace_printer (module Printers.DumpOCamlAst.Id)
        (module Printers.DumpOCamlAst.P);

  value enable_dump_camlp4_ast_printer () =
    replace_printer (module Printers.DumpCamlp4Ast.Id)
      (module Printers.DumpCamlp4Ast.P);

  value enable_null_printer () =
    replace_printer (module Printers.Null.Id)
      (module Printers.Null.P);

  value enable_auto isatty  =
    if isatty () then
      enable_ocaml_printer ()
    else
      enable_dump_ocaml_ast_printer ();

  (* rebound module Printers to extract most useful parts *)
  module Printers = struct
    module OCaml = Printers.OCaml.P;
    module DumpOCamlAst = Printers.DumpOCamlAst.P;
    module DumpCamlp4Ast = Printers.DumpCamlp4Ast.P;
    module Null = Printers.Null.P;
  end;
    
  (* value ast_filter (module Id:Sig.Id) (module Maker:Sig.ASTFILTER_PLUGIN) = *)
  (*   declare_dyn_module Id.name (fun _ *)
  (*       -> let module M = Maker AstFilters in ()); *)

  sig_item_parser.val := Syntax.parse_interf;
  str_item_parser.val := Syntax.parse_implem;

  module CurrentParser = struct
    value parse_interf ?directive_handler loc strm =
      sig_item_parser.val ?directive_handler loc strm;
    value parse_implem ?directive_handler loc strm =
      str_item_parser.val ?directive_handler loc strm;
    (* value parse_expr ?directive_handler loc expr = *)
      
  end;

  module CurrentPrinter = struct
    value print_interf ?input_file ?output_file ast =
      sig_item_printer.val ?input_file ?output_file ast;
    value print_implem ?input_file ?output_file ast =
      str_item_printer.val ?input_file ?output_file ast;
  end;

    
end; 
