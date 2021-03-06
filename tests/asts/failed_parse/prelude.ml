let with_open_out_file = Fan_util.with_open_out_file
let dump_pt = Fan_util.dump_pt
let simple_wrap = Fan_util.simple_wrap
let pp_print_flush = Format.pp_print_flush
let eprintf = Format.eprintf
open Util
type 'a parser_fun = Locf.t -> char Streamf.t -> 'a option
type 'a printer_fun =
  ?input_file:string -> ?output_file:string -> 'a option -> unit
let parsetree_of_interf ?(input_file= "-")  ?output_file  =
  function
  | ast ->
      let pt = match ast with | None  -> [] | Some ast -> Ast2pt.sigi ast in
      (with_open_out_file output_file) @@
        (dump_pt Configf.ocaml_ast_intf_magic_number input_file pt)
let parsetree_of_implem ?(input_file= "-")  ?output_file  =
  function
  | ast ->
      let pt = match ast with | None  -> [] | Some ast -> Ast2pt.stru ast in
      (with_open_out_file output_file) @@
        (dump_pt Configf.ocaml_ast_impl_magic_number input_file pt)
type backend =
  {
  descr: string;
  implem: Astf.stru printer_fun;
  interf: Astf.sigi printer_fun;}
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
  let print_implem ?input_file:_  ?output_file  =
    function
    | ast ->
        let pt = match ast with | None  -> [] | Some ast -> Ast2pt.stru ast in
        (with_open_out_file output_file) @@
          ((function
            | oc ->
                let fmt = Format.formatter_of_out_channel oc in
                (Format.fprintf fmt "@[%a@]@." Ast_print.structure pt;
                 pp_print_flush fmt ()))) in
  let print_interf ?input_file:_  ?output_file  =
    function
    | ast ->
        let pt = match ast with | None  -> [] | Some ast -> Ast2pt.sigi ast in
        (with_open_out_file output_file) @@
          ((function
            | oc ->
                let fmt = Format.formatter_of_out_channel oc in
                (Format.fprintf fmt "@[%a@]@." Ast_print.signature pt;
                 pp_print_flush fmt ()))) in
  Hashtbl.add backends "o"
    {
      descr = "Compiles to textual OCaml";
      implem = print_implem;
      interf = print_interf
    }
let () =
  let print_interf ?input_file:_  ?output_file  =
    function
    | ast ->
        let pt = match ast with | None  -> [] | Some ast -> Ast2pt.sigi ast in
        (with_open_out_file output_file) @@
          ((function
            | oc ->
                let fmt = Format.formatter_of_out_channel oc in
                Parsetree_util.print_interface fmt pt)) in
  let print_implem ?input_file:_  ?output_file  =
    function
    | ast ->
        let pt = match ast with | None  -> [] | Some ast -> Ast2pt.stru ast in
        (with_open_out_file output_file) @@
          ((function
            | oc ->
                let fmt = Format.formatter_of_out_channel oc in
                Parsetree_util.print_implementation fmt pt)) in
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
      method! loc =
        function
        | fmt ->
            (function | l -> Location_util.fmt_location ~file:false fmt l)
    end in
  let ast_of_interf ?input_file:_  ?output_file  =
    function
    | ast ->
        (with_open_out_file output_file) @@
          ((function
            | oc ->
                let fmt = Format.formatter_of_out_channel oc in
                (match ast with
                 | None  -> ()
                 | Some xs -> Format.fprintf fmt "@[%a@]@." obj#sigi xs))) in
  let ast_of_implem ?input_file:_  ?output_file  =
    function
    | ast ->
        (with_open_out_file output_file) @@
          ((function
            | oc ->
                let fmt = Format.formatter_of_out_channel oc in
                (match ast with
                 | None  -> ()
                 | Some xs -> Format.fprintf fmt "@[%a@]@." obj#stru xs))) in
  Hashtbl.add backends "dfan"
    {
      descr = "Compiles to Fan's original representation";
      implem = ast_of_implem;
      interf = ast_of_interf
    }
let () =
  let ast_of_interf ?input_file:_  ?output_file  =
    function
    | ast ->
        (with_open_out_file output_file) @@
          ((function
            | oc ->
                let fmt = Format.formatter_of_out_channel oc in
                (match ast with
                 | None  -> ()
                 | Some xs ->
                     Format.fprintf fmt "@[%a@]@." ObjsN.dump#sigi
                       (Strip.sigi xs)))) in
  let ast_of_implem ?input_file:_  ?output_file  =
    function
    | ast ->
        (with_open_out_file output_file) @@
          ((function
            | oc ->
                let fmt = Format.formatter_of_out_channel oc in
                (match ast with
                 | None  -> ()
                 | Some xs ->
                     Format.fprintf fmt "@[%a@]@." ObjsN.dump#stru
                       (Strip.stru xs)))) in
  Hashtbl.add backends "dfanl"
    {
      descr = "Compiles to Fan's original representation without location";
      implem = ast_of_implem;
      interf = ast_of_interf
    }
let parse_implem =
  function | loc -> (function | cs -> Gramlib.parse Syntaxf.implem loc cs)
let parse_interf =
  function
  | loc ->
      (function
       | cs ->
           let l = (simple_wrap loc cs) @@ (Gramlib.parse Syntaxf.interf) in
           (match l with | [] -> None | l -> Some (Ast_gen.sem_of_list l)))
let parse_file =
  function
  | name ->
      (function
       | pa ->
           let loc = Locf.mk name in
           let print_warning = eprintf "%a:\n%s@." Locf.print in
           let () = Fan_warnings.current := print_warning in
           let ic = if name = "-" then stdin else open_in_bin name in
           let clear =
             function | () -> if name = "-" then () else close_in ic in
           let cs = Streamf.of_channel ic in
           finally ~action:clear cs (pa loc))
module CurrentPrinter =
  struct
    let print_interf ?input_file  ?output_file  =
      function | ast -> ! sigi_printer ?input_file ?output_file ast
    let print_implem ?input_file  ?output_file  =
      function | ast -> ! stru_printer ?input_file ?output_file ast
  end
let wrap =
  function
  | parse_fun ->
      (fun ~print_location  ->
         function
         | lb ->
             (try
                let token_stream = lb |> Lex_fan.from_lexbuf in
                match Streamf.peek token_stream with
                | Some (`EOI _) ->
                    (Streamf.junk token_stream; raise End_of_file)
                | _ -> parse_fun token_stream
              with
              | End_of_file |Sys.Break |Locf.Exc_located
                (_,(End_of_file |Sys.Break )) as x -> raise x
              | Locf.Exc_located (loc,y) ->
                  (Format.eprintf "@[<0>%a%s@]@." print_location loc
                     (Printexc.to_string y);
                   raise Exit)
              | x ->
                  (Format.eprintf "@[<0>%s@]@." (Printexc.to_string x);
                   raise Exit)))
let toplevel_phrase =
  function
  | token_stream ->
      let stru = Gramf.parse_tokens Syntaxf.top_phrase token_stream in
      let stru = Ast_filters.apply_implem_filters stru in Ast2pt.phrase stru
let use_file =
  function
  | token_stream ->
      let s = Gramf.parse_tokens_eoi Syntaxf.use_file token_stream in
      List.map
        (function | x -> Ast2pt.phrase (Ast_filters.apply_implem_filters x))
        s
