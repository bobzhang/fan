open Format;
module Syntax = Syntax;

let sigi_parser: ( ?directive_handler: (Ast.sigi -> Ast.sigi option ) ->
   FanLoc.t -> char LibUtil.XStream.t   ->  Ast.sigi option  ) ref
 =
  ref (fun ?directive_handler:(_) _ _ -> failwith "No interface parser");
let stru_parser =
  ref (fun ?directive_handler:(_)  _ _ -> failwith "No implementation parser");
let sigi_printer =
  ref (fun ?input_file:(_) ?output_file:(_)  _ -> failwith "No interface printer");
let stru_printer =
  ref (fun ?input_file:(_)  ?output_file:(_) _ -> failwith "No implementation printer");
let callbacks = Queue.create ();
let loaded_modules = ref [];

(* iter and remove from the Queue *)
let iter_and_take_callbacks f =
  let rec loop () = loop (f (Queue.take callbacks)) in
  try loop () with [ Queue.Empty -> () ];

(* Register callbacks here *)    
let declare_dyn_module m f = begin
    loaded_modules := [ m :: !loaded_modules ];
      Queue.add (m, f) callbacks;
  end;

let register_stru_parser f = stru_parser := f;
let register_sigi_parser f = sigi_parser := f;
let register_parser f g =
  begin  stru_parser := f; sigi_parser := g  end;
let current_parser () = (!stru_parser, !sigi_parser);

let register_stru_printer f = stru_printer := f;
let register_sigi_printer f = sigi_printer := f;
let register_printer f g =
  begin  stru_printer := f; sigi_printer := g  end;
let current_printer () = (!stru_printer, !sigi_printer);


  
let plugin (module Id:Sig.Id) (module Maker:Sig.PLUGIN) =
  declare_dyn_module Id.name (fun _ -> let module M = Maker (struct end) in ());

let syntax_plugin (module Id:Sig.Id) (module Maker:Sig.SyntaxPlugin) =
  declare_dyn_module Id.name (fun _ -> let module M = Maker Syntax in ());
  
let syntax_extension (module Id:Sig.Id) (module Maker:Sig.SyntaxExtension) =
  declare_dyn_module Id.name (fun _ -> let module M = Maker Syntax in ());

let printer_plugin (module Id:Sig.Id) (module Maker:Sig.PrinterPlugin) =
  declare_dyn_module Id.name
    (fun _ -> let module M = Maker Syntax in
    register_printer M.print_implem M.print_interf);

let replace_printer (module Id:Sig.Id) (module P:Sig.PrinterImpl) =
  declare_dyn_module Id.name (fun _ ->
    register_printer P.print_implem P.print_interf);

let replace_parser (module Id:Sig.Id) (module Maker: Sig.ParserImpl) =
    declare_dyn_module Id.name
      (fun _ ->  register_parser Maker.parse_implem Maker.parse_interf);

let parser_plugin (module Id:Sig.Id) (module Maker:Sig.ParserPlugin) =
  declare_dyn_module Id.name (fun _
    -> let module M = Maker Syntax in
    register_parser M.parse_implem M.parse_interf );

let enable_ocaml_printer () = begin
  let module Id = struct
    let name = "Printers.OCaml"
    let version = Sys.ocaml_version
  end in 
  let module P = struct
    let print_implem ?input_file:(_)  ?output_file ast =
      let pt = match ast with
        [None -> [] | Some ast ->  Ast2pt.stru ast] in
      FanUtil.with_open_out_file output_file
        (fun oc ->
          let fmt = Format.formatter_of_out_channel oc in
          let () = AstPrint.structure fmt pt in 
          pp_print_flush fmt (););
      let print_interf ?input_file:(_)  ?output_file ast =
        let pt = match ast with
          [None -> []| Some ast -> Ast2pt.sigi ast] in
        FanUtil.with_open_out_file output_file
          (fun oc ->
            let fmt = Format.formatter_of_out_channel oc in
            let () = AstPrint.signature fmt pt in
            pp_print_flush fmt ();)
  end in 
  replace_printer (module Id) (module P);
 (* FIXME can be simplified *)
end;

let enable_dump_ocaml_ast_printer () =
  let module Id : Sig.Id = struct
    let name = "DumpOCamlAst";
    let version = Sys.ocaml_version
  end in 
 let module P = struct 
   let print_interf ?(input_file = "-") ?output_file ast =
     let pt =
       match ast with
       |None -> []
       |Some ast -> Ast2pt.sigi ast in
    FanUtil.(with_open_out_file
               output_file
               (dump_pt
                 FanConfig.ocaml_ast_intf_magic_number input_file pt));
  let print_implem ?(input_file = "-") ?output_file ast =
    let pt =
      match ast with
      |None -> []  
      |Some ast -> Ast2pt.stru ast in
    FanUtil.(with_open_out_file
               output_file
               (dump_pt FanConfig.ocaml_ast_impl_magic_number input_file pt))
 end in 
  replace_printer (module Id) (module P);

let enable_dump_ast_printer () =
  let module Id = struct
    let name = "DumFanAst"
    let version = Sys.ocaml_version
  end in 
  let module P = struct 
    let print_interf ?input_file:(_)  ?output_file ast =
      FanUtil.(with_open_out_file output_file
                 (dump_ast FanConfig.intf_magic_number ast))
    let print_implem ?input_file:(_) ?output_file ast =
      FanUtil.(with_open_out_file output_file
                 (dump_ast FanConfig.impl_magic_number ast))
  end in 
  replace_printer (module Id) (module P);;

let enable_null_printer () =
  let module Id = struct
    let name = "Printers.Null"
    let version = Sys.ocaml_version
  end in 
  let module P = struct
    let print_interf ?input_file:(_) ?output_file:(_) _ = ()
    let print_implem ?input_file:(_)  ?output_file:(_)  _ = ()
  end in
  replace_printer (module Id) (module P);;

let enable_auto isatty  =
  if isatty () then
    enable_ocaml_printer ()
  else
    enable_dump_ocaml_ast_printer ();;

  
sigi_parser := Syntax.parse_interf;;
stru_parser := Syntax.parse_implem;;

module CurrentParser = struct
  let parse_interf ?directive_handler loc strm =
    !sigi_parser ?directive_handler loc strm
  let parse_implem ?directive_handler loc strm =
    !stru_parser ?directive_handler loc strm
end;

module CurrentPrinter = struct
  let print_interf ?input_file ?output_file ast =
    !sigi_printer ?input_file ?output_file ast
  let print_implem ?input_file ?output_file ast =
    !stru_printer ?input_file ?output_file ast
end;


