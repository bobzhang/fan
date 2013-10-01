open Format
open FAst   
open LibUtil


{:import| Fan_util:
  with_open_out_file
  dump_pt
  (* wrap *)
  simple_wrap
  ;
|};;

let sigi_printer =
  ref (fun (* ?input_file:(_) *) ?output_file:(_)  _ -> failwith "No interface printer")

let stru_printer =
  ref (fun (* ?input_file:(_) *)
      ?output_file:(_) _ -> failwith "No implementation printer")

type 'a parser_fun  = loc -> char Fstream.t -> 'a option

type 'a printer_fun  =
      (* ?input_file:string -> *)
        ?output_file:string ->
        'a option -> unit
        


(*************************************************************************)
(** preparing for printer wrapper *)      
let register_text_printer () =
  let print_implem (* ?input_file:(_) *)  ?output_file ast =
    let pt =
      match ast with
      |None -> [] | Some ast ->  Ast2pt.stru ast in
    with_open_out_file output_file
      (fun oc ->
        let fmt = Format.formatter_of_out_channel oc in
        let () = AstPrint.structure fmt pt in 
        pp_print_flush fmt ();) in
  let print_interf (* ?input_file:(_)  *) ?output_file ast =
    let pt =
      match ast with
      |None -> []
      | Some ast -> Ast2pt.sigi ast in
    with_open_out_file output_file
      (fun oc ->
        let fmt = Format.formatter_of_out_channel oc in
        let () = AstPrint.signature fmt pt in
        pp_print_flush fmt ()) in
  begin
    stru_printer := print_implem;
    sigi_printer := print_interf
  end

        
(** used by flag -printer p *)    
let register_bin_printer () =
  let print_interf (* ?(input_file = "-")  *)?output_file ast =
      let pt =
        match ast with
        |None -> []
        |Some ast -> Ast2pt.sigi ast in
      (with_open_out_file
                 output_file
                 (dump_pt
                    FConfig.ocaml_ast_intf_magic_number (* input_file *) pt)) in
  let print_implem (* ?(input_file = "-") *) ?output_file ast =
    let pt =
      match ast with
      |None -> []  
      |Some ast -> Ast2pt.stru ast in
    with_open_out_file
      output_file
      (dump_pt FConfig.ocaml_ast_impl_magic_number (* input_file *) pt) in
  begin
    stru_printer := print_implem;
    sigi_printer := print_interf
  end;;




(*************************************************************************)
(** prepare for parsing wrapper *)


let parse_implem loc cs =
  let l =simple_wrap loc cs  @@ Fgram.parse Fsyntax.implem  in
  match l with
  | [] -> None
  | l -> Some (AstLib.sem_of_list l)


let parse_interf loc cs =
  let l = simple_wrap loc cs @@ Fgram.parse Fsyntax.interf  in
  match l with
  | [] -> None   
  | l -> Some (AstLib.sem_of_list l)

let parse_file  name pa = begin 
  let loc = FLoc.mk name in
  let print_warning = eprintf "%a:\n%s@." FLoc.print in
  let  () = Fsyntax.current_warning := print_warning in
  let ic = if name = "-" then stdin else open_in_bin name in
  let clear () = if name = "-" then () else close_in ic in
  let cs = Fstream.of_channel ic in
  finally ~action:clear  cs (pa loc)
end

        
module CurrentPrinter  = struct
  let print_interf (* ?input_file *) ?output_file ast =
    !sigi_printer (* ?input_file *) ?output_file ast
  let print_implem (* ?input_file *) ?output_file ast =
    !stru_printer (* ?input_file *) ?output_file ast
end




    

(*************************************************************************)
(** toplevel support *)    

let wrap parse_fun ~print_location lb =
  try
    let token_stream = lb |> Fan_lex.from_lexbuf |> Fgram.filter in
    match token_stream with parser (* FIXME *)
    |  (`EOI, _)  -> raise End_of_file
    |  -> parse_fun token_stream 
  with
  | End_of_file | Sys.Break
  | (FLoc.Exc_located (_, (End_of_file | Sys.Break))) as x ->
    raise x
  | FLoc.Exc_located (loc, y)  -> begin
      Format.eprintf "@[<0>%a%s@]@." print_location loc (Printexc.to_string y);
      raise Exit; (* commuiniation with toplevel special case here*)
  end
   | x ->  begin 
      Format.eprintf "@[<0>%s@]@." (Printexc.to_string x );
      raise Exit
  end 


let toplevel_phrase token_stream =
  match Fgram.parse_origin_tokens Fsyntax.top_phrase token_stream with
  | Some stru ->
        let stru =
          (* Fsyntax.Ast_filters.fold_topphrase_filters (fun t filter -> filter t) stru in *)
          Ast_filters.apply_implem_filters stru in
        Ast2pt.phrase stru
  | None -> raise End_of_file          



let use_file token_stream =
  let loop () =
      let (pl, stopped_at_directive) = Fgram.parse_origin_tokens Fsyntax.implem token_stream in
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
          Fgram.parse_origin_tokens Fsyntax.implem  token_stream in  
        if stopped_at_directive <> None then pl @ loop () else pl in loop () in
  (* FIXME semantics imprecise, the filter will always be applied *)
  List.map (fun x -> Ast2pt.phrase (Ast_filters.apply_implem_filters x) ) (pl0 @ pl)

        

(* local variables: *)
(* compile-command: "cd .. && pmake main_annot/preCast.cmo" *)
(* end: *)
