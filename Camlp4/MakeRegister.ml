
module type S = sig
    include Sig.FilterSyntax;
  module Plugin
      (Id : Sig.Id) (Plugin : functor (Unit : sig end) -> sig end) : sig end;
  module SyntaxPlugin
      (Id : Sig.Id) (SyntaxPlugin : functor (Syn : Sig.Syntax) -> sig end) :
      sig end;
  module SyntaxExtension
      (Id : Sig.Id) (SyntaxExtension : Sig.SyntaxExtension) : sig end;
  module OCamlSyntaxExtension
      (Id : Sig.Id)
      (SyntaxExtension : functor (Syntax : Sig.Camlp4Syntax) -> Sig.Camlp4Syntax)
      : sig end;

  (** {6 Registering Parsers} *)

  type parser_fun 'a =
      ?directive_handler:('a -> option 'a) -> Loc.t -> Stream.t char -> 'a;

  value register_str_item_parser : parser_fun Ast.str_item -> unit;
  value register_sig_item_parser : parser_fun Ast.sig_item -> unit;
  value register_parser :
      parser_fun Ast.str_item -> parser_fun Ast.sig_item -> unit;
  value current_parser :
      unit -> (parser_fun Ast.str_item * parser_fun Ast.sig_item);

  module Parser
      (Id : Sig.Id) (Maker : functor (Ast : Sig.Ast) -> (Sig.Parser Ast).S) : sig end;

  module OCamlParser
      (Id : Sig.Id) (Maker : functor (Ast : Sig.Camlp4Ast) -> (Sig.Parser Ast).S) : sig end;

  module OCamlPreCastParser
      (Id : Sig.Id) (Parser : (Sig.Parser Ast).S) : sig end;

  (** {6 Registering Printers} *)

  type printer_fun 'a =
      ?input_file:string -> ?output_file:string -> 'a -> unit;

  value register_str_item_printer : printer_fun Ast.str_item -> unit;
  value register_sig_item_printer : printer_fun Ast.sig_item -> unit;
  value register_printer :
      printer_fun Ast.str_item -> printer_fun Ast.sig_item -> unit;
  value current_printer :
      unit -> (printer_fun Ast.str_item * printer_fun Ast.sig_item);

  module Printer
      (Id : Sig.Id)
      (Maker : functor (Syn : Sig.Syntax) -> (Sig.Printer Syn.Ast).S) :
      sig end;

  module OCamlPrinter
      (Id : Sig.Id)
      (Maker : functor (Syn : Sig.Camlp4Syntax) -> (Sig.Printer Syn.Ast).S) :
      sig end;

  module OCamlPreCastPrinter
      (Id : Sig.Id) (Printer : (Sig.Printer Ast).S) :
      sig end;

  (** {6 Registering Filters} *)

  module AstFilter
      (Id : Sig.Id) (Maker : functor (F : Sig.AstFilters) -> sig end) : sig end;

  value declare_dyn_module : string -> (unit -> unit) -> unit;
  value iter_and_take_callbacks : ((string * (unit -> unit)) -> unit) -> unit;
  value loaded_modules : ref (list string);

  module CurrentParser : (Sig.Parser Ast).S;
  module CurrentPrinter : (Sig.Printer Ast).S;

  value enable_ocaml_printer : unit -> unit;
  value enable_ocamlr_printer : unit -> unit;
  (* value enable_ocamlrr_printer : unit -> unit; *)
  value enable_null_printer : unit -> unit;
  value enable_dump_ocaml_ast_printer : unit -> unit;
  value enable_dump_camlp4_ast_printer : unit -> unit;
  value enable_auto : (unit -> bool) -> unit ;  
  end ;
(* end; *)
  
  
module Make (Syntax:Sig.FilterSyntax) : S
     with   module Loc = Syntax.Loc and module Ast = Syntax.Ast = struct
  include Syntax;
  module PP = Printers; (* packing module Printers for Printers directory *)
  type parser_fun 'a =
      ?directive_handler:('a -> option 'a) -> Syntax.Loc.t -> Stream.t char -> 'a;

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

  value iter_and_take_callbacks f =
    let rec loop () = loop (f (Queue.take callbacks)) in
    try loop () with [ Queue.Empty -> () ];
      
  value declare_dyn_module m f =
    begin
      (* let () = Format.eprintf "declare_dyn_module: %s@." m in *)
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

  module Plugin (Id : Sig.Id) (Maker : functor (Unit : sig end) -> sig end) = struct
    declare_dyn_module Id.name (fun _ -> let module M = Maker (struct end) in ());
  end;

  module SyntaxExtension (Id : Sig.Id) (Maker : Sig.SyntaxExtension) = struct
    declare_dyn_module Id.name (fun _ -> let module M = Maker Syntax in ());
  end;

  module OCamlSyntaxExtension
      (Id : Sig.Id) (Maker : functor (Syn : Sig.Camlp4Syntax) -> Sig.Camlp4Syntax) =
    struct
      declare_dyn_module Id.name (fun _ -> let module M = Maker Syntax in ());
    end;

  module SyntaxPlugin (Id : Sig.Id) (Maker : functor (Syn : Sig.Syntax) -> sig end) = struct
    declare_dyn_module Id.name (fun _ -> let module M = Maker Syntax in ());
  end;

  module Printer
      (Id : Sig.Id) (Maker : functor (Syn : Sig.Syntax)
                     -> (Sig.Printer Syn.Ast).S) =
    struct
      declare_dyn_module Id.name (fun _ ->
        let module M = Maker Syntax in
        register_printer M.print_implem M.print_interf);
    end;

  module OCamlPrinter
      (Id : Sig.Id) (Maker : functor (Syn : Sig.Camlp4Syntax)
                     -> (Sig.Printer Syn.Ast).S) =
    struct
      declare_dyn_module Id.name (fun _ ->
        let module M = Maker Syntax in
        register_printer M.print_implem M.print_interf);
    end;

  module OCamlPreCastPrinter
      (Id : Sig.Id) (P : (Sig.Printer Syntax.Ast).S) =
    struct
      declare_dyn_module Id.name (fun _ ->
        register_printer P.print_implem P.print_interf);
    end;

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
      declare_dyn_module Id.name (fun _ -> let module M = Maker Syntax.AstFilters in ());
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

  value enable_ocaml_printer () =
    let module M = OCamlPrinter PP.OCaml.Id Printers.OCaml.MakeMore in ();

  value enable_ocamlr_printer () =
    let module M = OCamlPrinter PP.OCamlr.Id PP.OCamlr.MakeMore in ();

  (* value enable_ocamlrr_printer () =
     let module M = OCamlPrinter PP.OCamlrr.Id PP.OCamlrr.MakeMore in ();    *)

  value enable_dump_ocaml_ast_printer () =
    let module M = OCamlPrinter PP.DumpOCamlAst.Id PP.DumpOCamlAst.Make in ();

  value enable_dump_camlp4_ast_printer () =
    let module M = Printer PP.DumpCamlp4Ast.Id PP.DumpCamlp4Ast.Make in ();

  value enable_null_printer () =
    let module M = Printer PP.Null.Id PP.Null.Make in ();

  value enable_auto isatty  =
    if isatty () then
      enable_ocaml_printer ()
    else
      enable_dump_ocaml_ast_printer ();

end;
