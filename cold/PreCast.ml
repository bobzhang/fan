module Make(U:sig  end) : Sig.PRECAST =
  struct
  module Syntax = OCamlInitSyntax.Make(U)
  let sig_item_parser =
    ref (fun ?directive_handler:_  _  _  -> failwith "No interface parser")
  let str_item_parser =
    ref
      (fun ?directive_handler  _  _  -> failwith "No implementation parser")
  let sig_item_printer =
    ref
      (fun ?input_file  ?output_file  _  -> failwith "No interface printer")
  let str_item_printer =
    ref
      (fun ?input_file  ?output_file  _  ->
         failwith "No implementation printer")
  let callbacks = Queue.create () let loaded_modules = ref []
  let iter_and_take_callbacks f =
    let rec loop () = loop (f (Queue.take callbacks)) in
    try loop () with | Queue.Empty  -> ()
  let declare_dyn_module m f =
    loaded_modules := (m :: (loaded_modules.contents));
    Queue.add (m, f) callbacks
  let register_str_item_parser f = str_item_parser := f
  let register_sig_item_parser f = sig_item_parser := f
  let register_parser f g = str_item_parser := f; sig_item_parser := g
  let current_parser () =
    ((str_item_parser.contents), (sig_item_parser.contents))
  let register_str_item_printer f = str_item_printer := f
  let register_sig_item_printer f = sig_item_printer := f
  let register_printer f g = str_item_printer := f; sig_item_printer := g
  let current_printer () =
    ((str_item_printer.contents), (sig_item_printer.contents))
  let plugin ((module Id)  : (module Sig.Id))
    ((module Maker)  : (module Sig.PLUGIN)) =
    declare_dyn_module Id.name
      (fun _  -> let module M = Maker(struct
                   
                   end) in ())
  let syntax_plugin ((module Id)  : (module Sig.Id))
    ((module Maker)  : (module Sig.SyntaxPlugin)) =
    declare_dyn_module Id.name (fun _  -> let module M = Maker(Syntax) in ())
  let syntax_extension ((module Id)  : (module Sig.Id))
    ((module Maker)  : (module Sig.SyntaxExtension)) =
    declare_dyn_module Id.name (fun _  -> let module M = Maker(Syntax) in ())
  let printer_plugin ((module Id)  : (module Sig.Id))
    ((module Maker)  : (module Sig.PrinterPlugin)) =
    declare_dyn_module Id.name
      (fun _  ->
         let module M = Maker(Syntax) in
           register_printer M.print_implem M.print_interf)
  let replace_printer ((module Id)  : (module Sig.Id))
    ((module P)  : (module Sig.PrinterImpl)) =
    declare_dyn_module Id.name
      (fun _  -> register_printer P.print_implem P.print_interf)
  let replace_parser ((module Id)  : (module Sig.Id))
    ((module Maker)  : (module Sig.ParserImpl)) =
    declare_dyn_module Id.name
      (fun _  -> register_parser Maker.parse_implem Maker.parse_interf)
  let parser_plugin ((module Id)  : (module Sig.Id))
    ((module Maker)  : (module Sig.ParserPlugin)) =
    declare_dyn_module Id.name
      (fun _  ->
         let module M = Maker(Syntax) in
           register_parser M.parse_implem M.parse_interf)
  let enable_ocaml_printer () =
    replace_printer (module PrinterOCaml.Id) (module PrinterOCaml.P)
  let enable_dump_ocaml_ast_printer () =
    replace_printer (module PrinterDumpOCamlAst.Id) (module
      PrinterDumpOCamlAst.P)
  let enable_dump_camlp4_ast_printer () =
    replace_printer (module PrinterDumpCamlp4Ast.Id) (module
      PrinterDumpCamlp4Ast.P)
  let enable_null_printer () =
    replace_printer (module PrinterNull.Id) (module PrinterNull.P)
  let enable_auto isatty =
    if isatty ()
    then enable_ocaml_printer ()
    else enable_dump_ocaml_ast_printer ()
  module Printers = struct
    module OCaml = PrinterOCaml.P module DumpOCamlAst = PrinterDumpOCamlAst.P
    module DumpCamlp4Ast = PrinterDumpCamlp4Ast.P
    module Null = PrinterNull.P
    end let _ = sig_item_parser := Syntax.parse_interf
  let _ = str_item_parser := Syntax.parse_implem
  module CurrentParser =
    struct
    let parse_interf ?directive_handler  loc strm =
      sig_item_parser.contents ?directive_handler loc strm
    let parse_implem ?directive_handler  loc strm =
      str_item_parser.contents ?directive_handler loc strm
    end
  module CurrentPrinter =
    struct
    let print_interf ?input_file  ?output_file  ast =
      sig_item_printer.contents ?input_file ?output_file ast
    let print_implem ?input_file  ?output_file  ast =
      str_item_printer.contents ?input_file ?output_file ast
    end
  end 