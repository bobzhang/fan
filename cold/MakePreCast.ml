module Make =
 functor (U : sig  end) ->
  (struct
    module Syntax = (OCamlInitSyntax.Make)(U)
   let sig_item_parser =
    (ref (
      fun ?directive_handler:_ ->
       fun _ -> fun _ -> (failwith "No interface parser" ) ) )
   let str_item_parser =
    (ref (
      fun ?directive_handler:_ ->
       fun _ -> fun _ -> (failwith "No implementation parser" ) ) )
   let sig_item_printer =
    (ref (
      fun ?input_file:_ ->
       fun ?output_file:_ -> fun _ -> (failwith "No interface printer" ) ) )
   let str_item_printer =
    (ref (
      fun ?input_file:_ ->
       fun ?output_file:_ -> fun _ -> (failwith "No implementation printer" )
      ) )
   let callbacks = (Queue.create ()  )
   let loaded_modules = (ref []  )
   let iter_and_take_callbacks =
    fun f ->
     let rec loop = fun ()  -> (loop ( (f ( (Queue.take callbacks ) ) ) ) ) in
     (try (loop ()  ) with
      Queue.Empty -> ())
   let declare_dyn_module =
    fun m ->
     fun f ->
      (
      (loaded_modules := ( ( m ) :: loaded_modules.contents  ))
      );
      (Queue.add (m , f ) callbacks )
   let register_str_item_parser = fun f -> (str_item_parser := f)
   let register_sig_item_parser = fun f -> (sig_item_parser := f)
   let register_parser =
    fun f -> fun g -> ( (str_item_parser := f) ); (sig_item_parser := g)
   let current_parser =
    fun ()  -> (( str_item_parser.contents ) , ( sig_item_parser.contents ) )
   let register_str_item_printer = fun f -> (str_item_printer := f)
   let register_sig_item_printer = fun f -> (sig_item_printer := f)
   let register_printer =
    fun f -> fun g -> ( (str_item_printer := f) ); (sig_item_printer := g)
   let current_printer =
    fun ()
      ->
     (( str_item_printer.contents ) , ( sig_item_printer.contents ) )
   let plugin =
    fun ((module
     Id)
      :
      (module Sig.Id )) ->
     fun ((module
      Maker)
       :
       (module Sig.PLUGIN )) ->
      (declare_dyn_module Id.name (
        fun _ -> let module M = (Maker)(struct  end) in () ) )
   let syntax_plugin =
    fun ((module
     Id)
      :
      (module Sig.Id )) ->
     fun ((module
      Maker)
       :
       (module Sig.SyntaxPlugin )) ->
      (declare_dyn_module Id.name (
        fun _ -> let module M = (Maker)(Syntax) in () ) )
   let syntax_extension =
    fun ((module
     Id)
      :
      (module Sig.Id )) ->
     fun ((module
      Maker)
       :
       (module Sig.SyntaxExtension )) ->
      (declare_dyn_module Id.name (
        fun _ -> let module M = (Maker)(Syntax) in () ) )
   let printer_plugin =
    fun ((module
     Id)
      :
      (module Sig.Id )) ->
     fun ((module
      Maker)
       :
       (module Sig.PrinterPlugin )) ->
      (declare_dyn_module Id.name (
        fun _ ->
         let module M = (Maker)(Syntax) in
         (register_printer M.print_implem M.print_interf ) ) )
   let replace_printer =
    fun ((module
     Id)
      :
      (module Sig.Id )) ->
     fun ((module
      P)
       :
       (module Sig.PrinterImpl )) ->
      (declare_dyn_module Id.name (
        fun _ -> (register_printer P.print_implem P.print_interf ) ) )
   let replace_parser =
    fun ((module
     Id)
      :
      (module Sig.Id )) ->
     fun ((module
      Maker)
       :
       (module Sig.ParserImpl )) ->
      (declare_dyn_module Id.name (
        fun _ -> (register_parser Maker.parse_implem Maker.parse_interf ) ) )
   let parser_plugin =
    fun ((module
     Id)
      :
      (module Sig.Id )) ->
     fun ((module
      Maker)
       :
       (module Sig.ParserPlugin )) ->
      (declare_dyn_module Id.name (
        fun _ ->
         let module M = (Maker)(Syntax) in
         (register_parser M.parse_implem M.parse_interf ) ) )
   let enable_ocaml_printer =
    fun ()
      ->
     (replace_printer (module PrinterOCaml.Id) (module PrinterOCaml.P) )
   let enable_dump_ocaml_ast_printer =
    fun ()
      ->
     (replace_printer (module PrinterDumpOCamlAst.Id) (module
       PrinterDumpOCamlAst.P) )
   let enable_dump_camlp4_ast_printer =
    fun ()
      ->
     (replace_printer (module PrinterDumpCamlp4Ast.Id) (module
       PrinterDumpCamlp4Ast.P) )
   let enable_null_printer =
    fun ()
      ->
     (replace_printer (module PrinterNull.Id) (module PrinterNull.P) )
   let enable_auto =
    fun isatty ->
     if (isatty ()  ) then ( (enable_ocaml_printer ()  ) )
     else (enable_dump_ocaml_ast_printer ()  )
   module Printers =
    struct
     module OCaml = PrinterOCaml.P
    module DumpOCamlAst = PrinterDumpOCamlAst.P
    module DumpCamlp4Ast = PrinterDumpCamlp4Ast.P
    module Null = PrinterNull.P
    
    end
   let _ = (sig_item_parser := Syntax.parse_interf)
   let _ = (str_item_parser := Syntax.parse_implem)
   module CurrentParser =
    struct
     let parse_interf =
      fun ?directive_handler ->
       fun loc ->
        fun strm ->
         ((sig_item_parser.contents) ?directive_handler:directive_handler loc
           strm )
    let parse_implem =
     fun ?directive_handler ->
      fun loc ->
       fun strm ->
        ((str_item_parser.contents) ?directive_handler:directive_handler loc
          strm )
    
    end
   module CurrentPrinter =
    struct
     let print_interf =
      fun ?input_file ->
       fun ?output_file ->
        fun ast ->
         ((sig_item_printer.contents) ?input_file:input_file
           ?output_file:output_file ast )
    let print_implem =
     fun ?input_file ->
      fun ?output_file ->
       fun ast ->
        ((str_item_printer.contents) ?input_file:input_file
          ?output_file:output_file ast )
    
    end
   
   end : Sig.PRECAST)
