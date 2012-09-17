module Make =
 functor (Loc : FanSig.Loc) ->
  functor (Lexer : Sig.LEXER) ->
   (struct
     type token = FanSig.camlp4_token

     module Loc = Loc

     module Ast = (Struct.Camlp4Ast.Make)(Loc)

     module Token = (FanToken.Make)(Loc)

     module Lexer = (Lexer)(Token)

     module Gram = (Struct.Grammar.Static.Make)(Lexer)

     module DynLoader = Struct.DynLoader

     module Quotation = (Struct.Quotation.Make)(Ast)

     module MakeSyntax =
      functor (U : sig end) ->
       (((OCamlInitSyntax.Make)(Ast))(Gram))(Quotation)

     module Syntax = (MakeSyntax)(struct end)

     module AstFilters = (Struct.AstFilters.Make)(Ast)

     module MakeGram = Struct.Grammar.Static.Make

     type 'a parser_fun =
      (?directive_handler : ('a -> 'a option) ->
       (Syntax.Loc.t -> (char Stream.t -> 'a)))

     type 'a printer_fun =
      (?input_file : string -> (?output_file : string -> ('a -> unit)))

     let sig_item_parser =
      (ref (
        fun ?directive_handler:_ ->
         fun _ -> fun _ -> (failwith "No interface parser") ))

     let str_item_parser =
      (ref (
        fun ?directive_handler:_ ->
         fun _ -> fun _ -> (failwith "No implementation parser") ))

     let sig_item_printer =
      (ref (
        fun ?input_file:_ ->
         fun ?output_file:_ -> fun _ -> (failwith "No interface printer") ))

     let str_item_printer =
      (ref (
        fun ?input_file:_ ->
         fun ?output_file:_ ->
          fun _ -> (failwith "No implementation printer") ))

     let callbacks = (Queue.create () )

     let loaded_modules = (ref [] )

     let iter_and_take_callbacks =
      fun f ->
       let rec loop = fun ()  -> (loop ( (f ( (Queue.take callbacks) )) )) in
       (try (loop () ) with
        Queue.Empty -> ())

     let declare_dyn_module =
      fun m ->
       fun f ->
        (
        (loaded_modules := ( ( m ) :: !loaded_modules  ))
        );
        (Queue.add (m, f) callbacks)

     let register_str_item_parser = fun f -> (str_item_parser := f)

     let register_sig_item_parser = fun f -> (sig_item_parser := f)

     let register_parser =
      fun f -> fun g -> ( (str_item_parser := f) ); (sig_item_parser := g)

     let current_parser =
      fun ()  -> (( !str_item_parser ), ( !sig_item_parser ))

     let register_str_item_printer = fun f -> (str_item_printer := f)

     let register_sig_item_printer = fun f -> (sig_item_printer := f)

     let register_printer =
      fun f -> fun g -> ( (str_item_printer := f) ); (sig_item_printer := g)

     let current_printer =
      fun ()  -> (( !str_item_printer ), ( !sig_item_printer ))

     let plugin =
      fun ((module
       Id)
        :
        (module Sig.Id
       )) ->
       fun ((module
        Maker)
         :
         (module Sig.PLUGIN
        )) ->
        (declare_dyn_module Id.name (
          fun _ -> let module M = (Maker)(struct end) in () ))

     let syntax_plugin =
      fun ((module
       Id)
        :
        (module Sig.Id
       )) ->
       fun ((module
        Maker)
         :
         (module Sig.SYNTAX_PLUGIN
        )) ->
        (declare_dyn_module Id.name (
          fun _ -> let module M = (Maker)(Syntax) in () ))

     let syntax_extension =
      fun ((module
       Id)
        :
        (module Sig.Id
       )) ->
       fun ((module
        Maker)
         :
         (module Sig.SyntaxExtension
        )) ->
        (declare_dyn_module Id.name (
          fun _ -> let module M = (Maker)(Syntax) in () ))

     let ocaml_syntax_extension =
      fun ((module
       Id)
        :
        (module Sig.Id
       )) ->
       fun ((module
        Maker)
         :
         (module Sig.OCAML_SYNTAX_EXTENSION
        )) ->
        (declare_dyn_module Id.name (
          fun _ -> let module M = (Maker)(Syntax) in () ))

     let printer =
      fun ((module
       Id)
        :
        (module Sig.Id
       )) ->
       fun ((module
        Maker)
         :
         (module Sig.PRINTER_PLUGIN
        )) ->
        (declare_dyn_module Id.name (
          fun _ ->
           let module M = (Maker)(Syntax) in
           (register_printer M.print_implem M.print_interf) ))

     let ocaml_printer =
      fun ((module
       Id)
        :
        (module Sig.Id
       )) ->
       fun ((module
        Maker)
         :
         (module Sig.OCAML_PRINTER_PLUGIN
        )) ->
        (declare_dyn_module Id.name (
          fun _ ->
           let module M = (Maker)(Syntax) in
           (register_printer M.print_implem M.print_interf) ))

     let ocaml_precast_printer =
      fun ((module
       Id)
        :
        (module Sig.Id
       )) ->
       fun ((module
        P)
         :
         (module Sig.Printer(Syntax.Ast).S
        )) ->
        (declare_dyn_module Id.name (
          fun _ -> (register_printer P.print_implem P.print_interf) ))

     let parser_plugin =
      fun ((module
       Id)
        :
        (module Sig.Id
       )) ->
       fun ((module
        Maker)
         :
         (module Sig.PARSER
        )) ->
        (declare_dyn_module Id.name (
          fun _ ->
           let module M = (Maker)(Syntax.Ast) in
           (register_parser M.parse_implem M.parse_interf) ))

     let ocaml_parser_plugin =
      fun ((module
       Id)
        :
        (module Sig.Id
       )) ->
       fun ((module
        Maker)
         :
         (module Sig.OCAML_PARSER
        )) ->
        (declare_dyn_module Id.name (
          fun _ ->
           let module M = (Maker)(Syntax.Ast) in
           (register_parser M.parse_implem M.parse_interf) ))

     let ocaml_precast_parser_plugin =
      fun ((module
       Id)
        :
        (module Sig.Id
       )) ->
       fun ((module
        P)
         :
         (module Sig.Parser(Syntax.Ast).S
        )) ->
        (declare_dyn_module Id.name (
          fun _ -> (register_parser P.parse_implem P.parse_interf) ))

     let ast_filter =
      fun ((module
       Id)
        :
        (module Sig.Id
       )) ->
       fun ((module
        Maker)
         :
         (module Sig.ASTFILTER_PLUGIN
        )) ->
        (declare_dyn_module Id.name (
          fun _ -> let module M = (Maker)(AstFilters) in () ))

     let _ = (sig_item_parser := Syntax.parse_interf)

     let _ = (str_item_parser := Syntax.parse_implem)

     module CurrentParser =
      struct
       module Ast = Syntax.Ast

       let parse_interf =
        fun ?directive_handler ->
         fun loc ->
          fun strm ->
           ((!sig_item_parser) ?directive_handler:directive_handler loc strm)

       let parse_implem =
        fun ?directive_handler ->
         fun loc ->
          fun strm ->
           ((!str_item_parser) ?directive_handler:directive_handler loc strm)

      end

     module CurrentPrinter =
      struct
       module Ast = Syntax.Ast

       let print_interf =
        fun ?input_file ->
         fun ?output_file ->
          fun ast ->
           ((!sig_item_printer) ?input_file:input_file
             ?output_file:output_file ast)

       let print_implem =
        fun ?input_file ->
         fun ?output_file ->
          fun ast ->
           ((!str_item_printer) ?input_file:input_file
             ?output_file:output_file ast)

      end

     let enable_ocaml_printer =
      fun ()
        ->
       (
       (Format.eprintf "enable..")
       );
       (ocaml_printer (module Printers.OCaml.Id) (module
         Printers.OCaml.Make))

     let enable_dump_ocaml_ast_printer =
      fun ()
        ->
       (ocaml_printer (module Printers.DumpOCamlAst.Id) (module
         Printers.DumpOCamlAst.Make))

     let enable_dump_camlp4_ast_printer =
      fun ()
        ->
       (printer (module Printers.DumpCamlp4Ast.Id) (module
         Printers.DumpCamlp4Ast.Make))

     let enable_null_printer =
      fun ()
        ->
       (printer (module Printers.Null.Id) (module Printers.Null.Make))

     let enable_auto =
      fun isatty ->
       if (isatty () ) then ( (enable_ocaml_printer () ) )
       else (enable_dump_ocaml_ast_printer () )

     module Printers =
      struct
       module OCaml = (Printers.OCaml.Make)(Syntax)

       module DumpOCamlAst = (Printers.DumpOCamlAst.Make)(Syntax)

       module DumpCamlp4Ast = (Printers.DumpCamlp4Ast.Make)(Syntax)

       module Null = (Printers.Null.Make)(Syntax)

      end

    end : (Sig.PRECAST with module Loc = Loc))
