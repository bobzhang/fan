module Id =
 struct
  let name = "Camlp4Printers.DumpCamlp4Ast"

  let version = Sys.ocaml_version

 end

module Make =
       functor (Syntax : Sig.Syntax) ->
        (struct
          let print_interf =
           fun ?input_file:_ ->
            fun ?output_file ->
             fun ast ->
              let open
              P4_util in
              (with_open_out_file output_file (
                (dump_ast Camlp4_config.camlp4_ast_intf_magic_number ast) ))

          let print_implem =
           fun ?input_file:_ ->
            fun ?output_file ->
             fun ast ->
              let open
              P4_util in
              (with_open_out_file output_file (
                (dump_ast Camlp4_config.camlp4_ast_impl_magic_number ast) ))

         end : Sig.Printer(Syntax.Ast).S)
