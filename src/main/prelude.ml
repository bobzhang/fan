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
}

open Util




type 'a parser_fun  = Locf.t -> char Fstream.t -> 'a option

type 'a printer_fun  =
      ?input_file:string ->
        ?output_file:string ->
        'a option -> unit


let sigi_printer =
  ref (fun ?input_file:(_) ?output_file:(_)  _ -> failwith "No interface printer")

let stru_printer =
  ref (fun ?input_file:(_)
      ?output_file:(_) _ -> failwith "No implementation printer")

        


(*************************************************************************)
(** preparing for printer wrapper *)      
let register_text_printer () =
  let print_implem ?input_file:(_)  ?output_file ast =
    let pt =
      match ast with
      |None -> [] | Some ast ->  Ast2pt.stru ast in
    with_open_out_file output_file @@
    fun oc ->
      let fmt = Format.formatter_of_out_channel oc in
      let () = AstPrint.structure fmt pt in 
      pp_print_flush fmt () in
  let print_interf ?input_file:(_) ?output_file ast =
    let pt =
      match ast with
      |None -> []
      | Some ast -> Ast2pt.sigi ast in
    with_open_out_file output_file @@
    fun oc ->
      let fmt = Format.formatter_of_out_channel oc in
      let () = AstPrint.signature fmt pt in
      pp_print_flush fmt () in
  begin
    stru_printer := print_implem;
    sigi_printer := print_interf
  end

        
(** used by flag -printer p *)    
let register_bin_printer () =
  let print_interf ?(input_file = "-") ?output_file ast =
      let pt =
        match ast with
        |None -> []
        |Some ast -> Ast2pt.sigi ast in
      with_open_out_file output_file @@
      dump_pt
        Configf.ocaml_ast_intf_magic_number input_file pt in
  let print_implem ?(input_file = "-") ?output_file ast =
    let pt =
      match ast with
      |None -> []  
      |Some ast -> Ast2pt.stru ast in
    with_open_out_file output_file @@
    dump_pt Configf.ocaml_ast_impl_magic_number input_file pt in
  begin
    stru_printer := print_implem;
    sigi_printer := print_interf
  end;;



let register_parsetree_printer () =
  let print_interf ?input_file:(_) ?output_file ast =
      let pt =
        match ast with
        |None -> []
        |Some ast -> Ast2pt.sigi ast in
      with_open_out_file output_file @@
      fun oc ->
        let fmt = Format.formatter_of_out_channel oc in
        Printast.interface fmt pt in
      (* dump_pt *)
      (*   Configf.ocaml_ast_intf_magic_number input_file pt in *)
  let print_implem ?input_file:(_) ?output_file ast =
    let pt =
      match ast with
      |None -> []  
      |Some ast -> Ast2pt.stru ast in
    with_open_out_file output_file @@
    fun oc ->
      let fmt = Format.formatter_of_out_channel oc in
      Printast.implementation fmt pt in
  begin
    stru_printer := print_implem;
    sigi_printer := print_interf
  end;;



(*************************************************************************)
(** prepare for parsing wrapper *)


let parse_implem loc cs =
  let l =simple_wrap loc cs  @@ Fgram.parse Syntaxf.implem  in
  match l with
  | [] -> None
  | l -> Some (Ast_gen.sem_of_list l)


let parse_interf loc cs =
  let l = simple_wrap loc cs @@ Fgram.parse Syntaxf.interf  in
  match l with
  | [] -> None   
  | l -> Some (Ast_gen.sem_of_list l)

let parse_file  name pa = begin 
  let loc = Locf.mk name in
  let print_warning = eprintf "%a:\n%s@." Locf.print in
  let  () = Fan_warnings.current := print_warning in
  let ic = if name = "-" then stdin else open_in_bin name in
  let clear () = if name = "-" then () else close_in ic in
  let cs = Fstream.of_channel ic in
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
    let token_stream = lb |> Lex_fan.from_lexbuf |> Fgram.filter in
    match Fstream.peek token_stream with
    | Some (`EOI _,_) -> (Fstream.junk token_stream; raise End_of_file)
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
  match Fgram.parse_origin_tokens Syntaxf.top_phrase token_stream with
  | Some stru ->
        let stru =
          (* Syntaxf.Ast_filters.fold_topphrase_filters (fun t filter -> filter t) stru in *)
          Ast_filters.apply_implem_filters stru in
        Ast2pt.phrase stru
  | None -> raise End_of_file          



let use_file token_stream =
  let loop () =
      let (pl, stopped_at_directive) =
        Fgram.parse_origin_tokens Syntaxf.implem token_stream in
      if stopped_at_directive <> None then (* only support [load] and [directory] *)
        with stru match pl with
        | _ -> (pl, false) 
      else (pl, true) in
  let (pl0, eoi) = loop () in
  let pl =
    if eoi then []
    else
      let rec loop () =
        let (pl, stopped_at_directive) =
          Fgram.parse_origin_tokens Syntaxf.implem  token_stream in  
        if stopped_at_directive <> None then pl @ loop () else pl in loop () in
  (* FIXME semantics imprecise, the filter will always be applied *)
  List.map (fun x -> Ast2pt.phrase (Ast_filters.apply_implem_filters x) ) (pl0 @ pl)

        

(* local variables: *)
(* compile-command: "cd .. && pmake main_annot/prelude.cmo" *)
(* end: *)
