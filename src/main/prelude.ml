%import{
Fan_util:
  with_open_out_file
  dump_pt
  simple_wrap
  ;
Format:
  pp_print_flush
  eprintf
  ;
};;

open Util




type 'a parser_fun  = Locf.t -> char Streamf.t -> 'a option

type 'a printer_fun  =
      ?input_file:string ->
        ?output_file:string ->
        'a option -> unit



  
let parsetree_of_interf ?(input_file = "-") ?output_file ast =
  let pt =
    match ast with
    |None -> []
    |Some ast -> Ast2pt.sigi ast in
  with_open_out_file output_file @@
  dump_pt
    Configf.ocaml_ast_intf_magic_number input_file pt

let parsetree_of_implem ?(input_file = "-") ?output_file ast =
    let pt =
      match ast with
      |None -> []  
      |Some ast -> Ast2pt.stru ast in
    with_open_out_file output_file @@
    dump_pt Configf.ocaml_ast_impl_magic_number input_file pt    

type backend = {
    descr : string ;
    implem : Astf.stru printer_fun;
    interf : Astf.sigi printer_fun;
  }
      
let backends :
    (string, backend)  Hashtbl.t =
  Hashtbl.create 50

let () =
  Hashtbl.add backends "p" {
  descr = "compiled to parsetree";
  implem = parsetree_of_implem ;
  interf = parsetree_of_interf
}

(** default to [parsetree]*)
let sigi_printer =
  ref parsetree_of_interf

let stru_printer =
  ref parsetree_of_implem


let () =
  let print_implem ?input_file:(_)  ?output_file ast =
    let pt =
      match ast with
      |None -> [] | Some ast ->  Ast2pt.stru ast in
    with_open_out_file output_file @@
    fun oc ->
      let fmt = Format.formatter_of_out_channel oc in
      begin 
        Format.fprintf fmt "@[%a@]@." Ast_print.structure pt ;
        pp_print_flush fmt ()
      end in
  let print_interf ?input_file:(_) ?output_file ast =
    let pt =
      match ast with
      |None -> []
      | Some ast -> Ast2pt.sigi ast in
    with_open_out_file output_file @@
    fun oc ->
      let fmt = Format.formatter_of_out_channel oc in
      begin 
        Format.fprintf fmt "@[%a@]@." Ast_print.signature pt ;
        pp_print_flush fmt ()
      end in
  Hashtbl.add backends "o" {
    descr =  "Compiles to textual OCaml";
    implem = print_implem;
    interf =  print_interf}

let () = 
  let print_interf ?input_file:(_) ?output_file ast =
      let pt =
        match ast with
        |None -> []
        |Some ast -> Ast2pt.sigi ast in
      with_open_out_file output_file @@
      fun oc ->
        let fmt = Format.formatter_of_out_channel oc in
        Printast.interface fmt pt in
  let print_implem ?input_file:(_) ?output_file ast =
    let pt =
      match ast with
      |None -> []  
      |Some ast -> Ast2pt.stru ast in
    with_open_out_file output_file @@
    fun oc ->
      let fmt = Format.formatter_of_out_channel oc in
      Printast.implementation fmt pt in
  Hashtbl.add backends "dparsetree" {
  descr =  "Compiles to parsetree decorated with location";
  implem = print_implem;
  interf = print_interf}
    
let () =
  let module N = Astf_print.Make (struct
    let pp_print_loc fmt l =
      Location_util.fmt_location  ~file:false fmt l
   end ) in 
  let ast_of_interf ?input_file:(_) ?output_file ast =
    with_open_out_file output_file @@ fun oc ->
      let fmt = Format.formatter_of_out_channel oc in
      match ast with
      | None -> ()
      | Some xs  ->
          Format.fprintf fmt "@[%a@]@." N.pp_print_sigi  xs in
  let ast_of_implem ?input_file:(_)  ?output_file ast =
    with_open_out_file output_file @@ fun oc ->
      let fmt = Format.formatter_of_out_channel oc in
      match ast with
      | None -> ()
      | Some xs  ->
          Format.fprintf fmt "@[%a@]@." N.pp_print_stru  xs in
  Hashtbl.add backends "dfan" {
  descr = "Compiles to Fan's original representation";
  implem = ast_of_implem;
  interf = ast_of_interf
}

let () =
  let ast_of_interf ?input_file:(_) ?output_file ast =
    with_open_out_file output_file @@ fun oc ->
      let fmt = Format.formatter_of_out_channel oc in
      match ast with
      | None -> ()
      | Some xs  ->
          Format.fprintf fmt "@[%a@]@."
            Astfn_print.pp_print_sigi  (Strip.sigi xs) in
  let ast_of_implem ?input_file:(_)  ?output_file ast =
    with_open_out_file output_file @@ fun oc ->
      let fmt = Format.formatter_of_out_channel oc in
      match ast with
      | None -> ()
      | Some xs  ->
          Format.fprintf fmt "@[%a@]@."
            Astfn_print.pp_print_stru  (Strip.stru xs) in
  Hashtbl.add backends "dfanl" {
  descr = "Compiles to Fan's original representation without location";
  implem = ast_of_implem;
  interf = ast_of_interf
}

(********************************)
(* prepare for parsing wrapper  *)
(********************************)


let parse_implem loc cs = Gramlib.parse Syntaxf.implem loc cs


let parse_interf loc cs =
  let l = simple_wrap loc cs @@ Gramlib.parse Syntaxf.interf  in
  match l with
  | [] -> None   
  | l -> Some (Ast_gen.sem_of_list l)

let parse_file  name pa = begin 
  let loc = Locf.mk name in
  let print_warning = eprintf "%a:\n%s@." Locf.print in
  let  () = Fan_warnings.current := print_warning in
  let ic = if name = "-" then stdin else open_in_bin name in
  let clear () = if name = "-" then () else close_in ic in
  let cs = Streamf.of_channel ic in
  finally ~action:clear  cs (pa loc)
end

        
module CurrentPrinter  = struct
  let print_interf ?input_file ?output_file ast =
    !sigi_printer ?input_file ?output_file ast
  let print_implem ?input_file ?output_file ast =
    !stru_printer ?input_file ?output_file ast
end



(*************************************************************************)
(** toplevel support *)    

let wrap parse_fun ~print_location lb =
  try
    let token_stream = lb |> Lex_fan.from_lexbuf  in
    match Streamf.peek token_stream with
    | Some (`EOI _) -> (Streamf.junk token_stream; raise End_of_file)
    | _ -> parse_fun token_stream
  with
  | End_of_file | Sys.Break
  | (Locf.Exc_located (_, (End_of_file | Sys.Break))) as x ->
      raise x
  | Locf.Exc_located (loc, y)  -> begin
      Format.eprintf "@[<0>%a%s@]@." print_location loc (Printexc.to_string y);
      raise Exit; (* commuiniation with toplevel special case here*)
  end
   | x ->  begin 
      Format.eprintf "@[<0>%s@]@." (Printexc.to_string x );
      raise Exit
  end 


let toplevel_phrase token_stream =
  let stru = Gramf.parse_tokens Syntaxf.top_phrase token_stream in 
  let stru =
    (* Syntaxf.Ast_filters.fold_topphrase_filters (fun t filter -> filter t) stru in *)
    Ast_filters.apply_implem_filters stru in
  Ast2pt.phrase stru
  


  (* let loop () = *)
  (*     let (pl, stopped_at_directive) = *)
  (*       Gramf.parse_origin_tokens Syntaxf.implem token_stream in *)
  (*     if stopped_at_directive <> None then (\* only support [load] and [directory] *\) *)
  (*       with stru match pl with *)
  (*       | _ -> (pl, false)  *)
  (*     else (pl, true) in *)
  (* let (pl0, eoi) = loop () in *)
  (* let pl = *)
  (*   if eoi then [] *)
  (*   else *)
  (*     let rec loop () = *)
  (*       let (pl, stopped_at_directive) = *)
  (*         Gramf.parse_origin_tokens Syntaxf.implem  token_stream in   *)
  (*       if stopped_at_directive <> None then pl @ loop () else pl in loop () in *)
  (* FIXME semantics imprecise, the filter will always be applied *)
let use_file token_stream =
  let s  = Gramf.parse_tokens_eoi Syntaxf.use_file token_stream in
  List.map
    (fun x -> Ast2pt.phrase (Ast_filters.apply_implem_filters x) )
    s 
        (* (Ast_basic.list_of_sem s []) *) (* (pl0 @ pl) *)

        

(* local variables: *)
(* compile-command: "cd .. && pmake main_annot/prelude.cmo" *)
(* end: *)
