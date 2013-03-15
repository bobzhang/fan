module Id : Sig.Id = struct
  let name = "Camlp4Printers.DumpOCamlAst";
  let version = Sys.ocaml_version;
end;

module P = struct 
  let print_interf ?(input_file = "-") ?output_file ast =
    let pt =
      match ast with
      [None -> []
      |Some ast -> Ast2pt.sig_item ast] in
    FanUtil.(with_open_out_file
               output_file
               (dump_pt
                 FanConfig.ocaml_ast_intf_magic_number input_file pt));

  let print_implem ?(input_file = "-") ?output_file ast =
    let pt =
      match ast with
      [None -> []  
      |Some ast -> Ast2pt.stru ast] in
    FanUtil.(with_open_out_file
               output_file
               (dump_pt FanConfig.ocaml_ast_impl_magic_number input_file pt));

end;
