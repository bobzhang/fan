open Format
module Id = struct
  let name = "Printers.OCaml" let version = Sys.ocaml_version
  end
module P =
  struct
  let print_implem ?input_file:_  ?output_file  ast =
    let pt = Ast2pt.str_item ast in
    FanUtil.with_open_out_file output_file (
      fun oc ->
        let fmt = Format.formatter_of_out_channel oc in
        let () = AstPrint.structure fmt pt in pp_print_flush fmt () )
  let print_interf ?input_file:_  ?output_file  ast =
    let pt = Ast2pt.sig_item ast in
    FanUtil.with_open_out_file output_file (
      fun oc ->
        let fmt = Format.formatter_of_out_channel oc in
        let () = AstPrint.signature fmt pt in pp_print_flush fmt () )
  end