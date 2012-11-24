module Id = struct
  let name = "Camlp4Printers.DumpCamlp4Ast"
  let version = Sys.ocaml_version
  end
module P =
  struct
  let print_interf ?input_file  ?output_file  ast =
    let open FanUtil in
      with_open_out_file output_file
        (dump_ast FanConfig.camlp4_ast_intf_magic_number ast)
  let print_implem ?input_file  ?output_file  ast =
    let open FanUtil in
      with_open_out_file output_file
        (dump_ast FanConfig.camlp4_ast_impl_magic_number ast)
  end