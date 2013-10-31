let with_open_out_file = Fan_util.with_open_out_file
let dump_pt = Fan_util.dump_pt
let simple_wrap = Fan_util.simple_wrap
let pp_print_flush = Format.pp_print_flush
let eprintf = Format.eprintf
open Util
type 'a parser_fun = Locf.t -> char Streamf.t -> 'a option 
type 'a printer_fun =
  ?input_file:string -> ?output_file:string -> 'a option -> unit 
let parsetree_of_interf ?(input_file= "-")  ?output_file  ast =
  let pt = match ast with | None  -> [] | Some ast -> Ast2pt.sigi ast in
  (with_open_out_file output_file) @@
    (dump_pt Configf.ocaml_ast_intf_magic_number input_file pt)
let parsetree_of_implem ?(input_file= "-")  ?output_file  ast =
  let pt = match ast with | None  -> [] | Some ast -> Ast2pt.stru ast in
  (with_open_out_file output_file) @@
    (dump_pt Configf.ocaml_ast_impl_magic_number input_file pt)
type backend = 
  {
  descr: string;
  implem: FAst.stru printer_fun;
  interf: FAst.sigi printer_fun} 
let backends: (string,backend) Hashtbl.t = Hashtbl.create 50
let () =
  Hashtbl.add backends "p"
    {
      descr = "compiled to parsetree";
      implem = parsetree_of_implem;
      interf = parsetree_of_interf
    }
let sigi_printer = ref parsetree_of_interf
let stru_printer = ref parsetree_of_implem
let () =
  let print_implem ?input_file:_  ?output_file  ast =
    let pt = match ast with | None  -> [] | Some ast -> Ast2pt.stru ast in
    (with_open_out_file output_file) @@
      (fun oc  ->
         let fmt = Format.formatter_of_out_channel oc in
         Format.fprintf fmt "@[%a@]@\n" AstPrint.structure pt;
         pp_print_flush fmt ()) in
  let print_interf ?input_file:_  ?output_file  ast =
    let pt = match ast with | None  -> [] | Some ast -> Ast2pt.sigi ast in
    (with_open_out_file output_file) @@
      (fun oc  ->
         let fmt = Format.formatter_of_out_channel oc in
         Format.fprintf fmt "@[%a@]@\n" AstPrint.signature pt;
         pp_print_flush fmt ()) in
  Hashtbl.add backends "o"
    {
      descr = "Compiles to textual OCaml";
      implem = print_implem;
      interf = print_interf
    }
let () =
  let print_interf ?input_file:_  ?output_file  ast =
    let pt = match ast with | None  -> [] | Some ast -> Ast2pt.sigi ast in
    (with_open_out_file output_file) @@
      (fun oc  ->
         let fmt = Format.formatter_of_out_channel oc in
         Printast.interface fmt pt) in
  let print_implem ?input_file:_  ?output_file  ast =
    let pt = match ast with | None  -> [] | Some ast -> Ast2pt.stru ast in
    (with_open_out_file output_file) @@
      (fun oc  ->
         let fmt = Format.formatter_of_out_channel oc in
         Printast.implementation fmt pt) in
  Hashtbl.add backends "dparsetree"
    {
      descr = "Compiles to parsetree decorated with location";
      implem = print_implem;
      interf = print_interf
    }
let () =
  let obj =
    object 
      inherit  Objs.print
      method! loc fmt l = Location_util.fmt_location ~file:false fmt l
    end in
  let ast_of_interf ?input_file:_  ?output_file  ast =
    (with_open_out_file output_file) @@
      (fun oc  ->
         let fmt = Format.formatter_of_out_channel oc in
         match ast with
         | None  -> ()
         | Some xs -> Format.fprintf fmt "@[%a@]@\n" obj#sigi xs) in
  let ast_of_implem ?input_file:_  ?output_file  ast =
    (with_open_out_file output_file) @@
      (fun oc  ->
         let fmt = Format.formatter_of_out_channel oc in
         match ast with
         | None  -> ()
         | Some xs -> Format.fprintf fmt "@[%a@]@\n" obj#stru xs) in
  Hashtbl.add backends "dfan"
    {
      descr = "Compiles to Fan's original representation";
      implem = ast_of_implem;
      interf = ast_of_interf
    }
let () =
  let ast_of_interf ?input_file:_  ?output_file  ast =
    (with_open_out_file output_file) @@
      (fun oc  ->
         let fmt = Format.formatter_of_out_channel oc in
         match ast with
         | None  -> ()
         | Some xs ->
             Format.fprintf fmt "@[%a@]@\n" ObjsN.dump#sigi
               (Objs.strip_sigi xs)) in
  let ast_of_implem ?input_file:_  ?output_file  ast =
    (with_open_out_file output_file) @@
      (fun oc  ->
         let fmt = Format.formatter_of_out_channel oc in
         match ast with
         | None  -> ()
         | Some xs ->
             Format.fprintf fmt "@[%a@]@\n" ObjsN.dump#stru
               (Objs.strip_stru xs)) in
  Hashtbl.add backends "dfanl"
    {
      descr = "Compiles to Fan's original representation without location";
      implem = ast_of_implem;
      interf = ast_of_interf
    }
let parse_implem loc cs =
  let l = (simple_wrap loc cs) @@ (Gramf.parse Syntaxf.implem) in
  match l with | [] -> None | l -> Some (Ast_gen.sem_of_list l)
let parse_interf loc cs =
  let l = (simple_wrap loc cs) @@ (Gramf.parse Syntaxf.interf) in
  match l with | [] -> None | l -> Some (Ast_gen.sem_of_list l)
let parse_file name pa =
  let loc = Locf.mk name in
  let print_warning = eprintf "%a:\n%s@." Locf.print in
  let () = Fan_warnings.current := print_warning in
  let ic = if name = "-" then stdin else open_in_bin name in
  let clear () = if name = "-" then () else close_in ic in
  let cs = Streamf.of_channel ic in finally ~action:clear cs (pa loc)
module CurrentPrinter =
  struct
    let print_interf ?input_file  ?output_file  ast =
      ! sigi_printer ?input_file ?output_file ast
    let print_implem ?input_file  ?output_file  ast =
      ! stru_printer ?input_file ?output_file ast
  end
let wrap parse_fun ~print_location  lb =
  try
    let token_stream = (lb |> Lex_fan.from_lexbuf) |> Gramf.filter in
    match Streamf.peek token_stream with
    | Some (`EOI _) -> (Streamf.junk token_stream; raise End_of_file)
    | _ -> parse_fun token_stream
  with
  | End_of_file |Sys.Break |Locf.Exc_located (_,(End_of_file |Sys.Break )) as
      x -> raise x
  | Locf.Exc_located (loc,y) ->
      (Format.eprintf "@[<0>%a%s@]@." print_location loc
         (Printexc.to_string y);
       raise Exit)
  | x -> (Format.eprintf "@[<0>%s@]@." (Printexc.to_string x); raise Exit)
let toplevel_phrase token_stream =
  match Gramf.parse_origin_tokens Syntaxf.top_phrase token_stream with
  | Some stru ->
      let stru = Ast_filters.apply_implem_filters stru in Ast2pt.phrase stru
  | None  -> raise End_of_file
let use_file token_stream =
  let loop () =
    let (pl,stopped_at_directive) =
      Gramf.parse_origin_tokens Syntaxf.implem token_stream in
    if stopped_at_directive <> None
    then match pl with | _ -> (pl, false)
    else (pl, true) in
  let (pl0,eoi) = loop () in
  let pl =
    if eoi
    then []
    else
      (let rec loop () =
         let (pl,stopped_at_directive) =
           Gramf.parse_origin_tokens Syntaxf.implem token_stream in
         if stopped_at_directive <> None then pl @ (loop ()) else pl in
       loop ()) in
  List.map (fun x  -> Ast2pt.phrase (Ast_filters.apply_implem_filters x))
    (pl0 @ pl)
