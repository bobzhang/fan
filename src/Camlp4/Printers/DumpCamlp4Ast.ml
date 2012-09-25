
module Id = struct
  value name = "Camlp4Printers.DumpCamlp4Ast";
  value version = Sys.ocaml_version;
end;

module P = struct 
value print_interf ?input_file:(_) ?output_file ast =
    FanUtil.(with_open_out_file output_file
               (dump_ast FanConfig.camlp4_ast_intf_magic_number ast));

  value print_implem ?input_file:(_) ?output_file ast =
    FanUtil.(with_open_out_file output_file
               (dump_ast FanConfig.camlp4_ast_impl_magic_number ast));
end;
