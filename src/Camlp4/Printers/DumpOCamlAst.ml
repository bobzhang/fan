module Id : Sig.Id = struct
  value name = "Camlp4Printers.DumpOCamlAst";
  value version = Sys.ocaml_version;
end;

module P = struct 
  value print_interf ?(input_file = "-") ?output_file ast =
    let pt = Ast2pt.sig_item ast in
    FanUtil.(with_open_out_file
               output_file
               (dump_pt
                 FanConfig.ocaml_ast_intf_magic_number input_file pt));

  value print_implem ?(input_file = "-") ?output_file ast =
    let pt = Ast2pt.str_item ast in
    FanUtil.(with_open_out_file
               output_file
               (dump_pt FanConfig.ocaml_ast_impl_magic_number input_file pt));

end;
