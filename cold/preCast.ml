open Format

let sigi_parser:
  (?directive_handler:(Ast.sigi -> Ast.sigi option) ->
     FanLoc.t -> char LibUtil.XStream.t -> Ast.sigi option)
    ref
  = ref (fun ?directive_handler:_  _  _  -> failwith "No interface parser")

let stru_parser =
  ref
    (fun ?directive_handler:_  _  _  -> failwith "No implementation parser")

let sigi_printer =
  ref
    (fun ?input_file:_  ?output_file:_  _  -> failwith "No interface printer")

let stru_printer =
  ref
    (fun ?input_file:_  ?output_file:_  _  ->
       failwith "No implementation printer")

let callbacks = Queue.create ()

let iter_and_take_callbacks f =
  let rec loop () = loop (f (Queue.take callbacks)) in
  try loop () with | Queue.Empty  -> ()

let register_stru_printer f = stru_printer := f

let register_sigi_printer f = sigi_printer := f

let register_printer f g = stru_printer := f; sigi_printer := g

let current_printer () = ((stru_printer.contents), (sigi_printer.contents))

let register_text_printer () =
  let print_implem ?input_file:_  ?output_file  ast =
    let pt = match ast with | None  -> [] | Some ast -> Ast2pt.stru ast in
    FanUtil.with_open_out_file output_file
      (fun oc  ->
         let fmt = Format.formatter_of_out_channel oc in
         let () = AstPrint.structure fmt pt in pp_print_flush fmt ()) in
  let print_interf ?input_file:_  ?output_file  ast =
    let pt = match ast with | None  -> [] | Some ast -> Ast2pt.sigi ast in
    FanUtil.with_open_out_file output_file
      (fun oc  ->
         let fmt = Format.formatter_of_out_channel oc in
         let () = AstPrint.signature fmt pt in pp_print_flush fmt ()) in
  register_stru_printer print_implem; register_sigi_printer print_interf

let register_bin_printer () =
  let print_interf ?(input_file= "-")  ?output_file  ast =
    let pt = match ast with | None  -> [] | Some ast -> Ast2pt.sigi ast in
    let open FanUtil in
      with_open_out_file output_file
        (dump_pt FanConfig.ocaml_ast_intf_magic_number input_file pt) in
  let print_implem ?(input_file= "-")  ?output_file  ast =
    let pt = match ast with | None  -> [] | Some ast -> Ast2pt.stru ast in
    let open FanUtil in
      with_open_out_file output_file
        (dump_pt FanConfig.ocaml_ast_impl_magic_number input_file pt) in
  register_stru_printer print_implem; register_sigi_printer print_interf

let _ = sigi_parser := Syntax.parse_interf

let _ = stru_parser := Syntax.parse_implem

module CurrentParser =
  struct
    let parse_interf ?directive_handler  loc strm =
      sigi_parser.contents ?directive_handler loc strm
    let parse_implem ?directive_handler  loc strm =
      stru_parser.contents ?directive_handler loc strm
  end

module CurrentPrinter =
  struct
    let print_interf ?input_file  ?output_file  ast =
      sigi_printer.contents ?input_file ?output_file ast
    let print_implem ?input_file  ?output_file  ast =
      stru_printer.contents ?input_file ?output_file ast
  end