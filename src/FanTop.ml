open Ast;
open LibUtil;
open Fan;

let wrap parse_fun lb =
  let () = iter_and_take_callbacks (fun (_, f) -> f ()) in
  try
    let not_filtered_token_stream = FanLexUtil.from_lexbuf lb in
    let token_stream = Gram.filter  not_filtered_token_stream in
    match token_stream with parser (* FIXME *)
  [ [< (`EOI, _) >] -> raise End_of_file
  | [< >] -> parse_fun token_stream ]
  with
  [ End_of_file | Sys.Break | (FanLoc.Exc_located (_, (End_of_file | Sys.Break))) as x ->
    raise x
  | (FanLoc.Exc_located (loc, y) ) -> begin
      Format.eprintf "@[<0>%a%s@]@."
        Toploop.print_location loc (Printexc.to_string y);
      raise Exit; (* commuiniation with toplevel special case here*)
  end
   | x ->  begin 
      Format.eprintf "@[<0>%s@]@." (Printexc.to_string x );
      raise Exit
  end ] ;


let toplevel_phrase token_stream =
  match Gram.parse_origin_tokens
      (Syntax.top_phrase : Gram.t (option str_item)) token_stream with
    [ Some str_item ->
        let str_item =
          (* Syntax.AstFilters.fold_topphrase_filters (fun t filter -> filter t) str_item in *)
          AstFilters.apply_implem_filters str_item in
        Ast2pt.phrase str_item
    | None -> raise End_of_file ];

  
let use_file token_stream =
  let rec loop () =
      let (pl, stopped_at_directive) = Gram.parse_origin_tokens Syntax.implem token_stream in
      if stopped_at_directive <> None then (* only support [load] and [directory] *)
        with str_item match pl with
        [ [ {| #load $str:s |} ] ->
            begin  Topdirs.dir_load Format.std_formatter s; loop ()  end
        | [ {| #directory $str:s |} ] ->
            begin  Topdirs.dir_directory s; loop ()  end
        | [ {| #default_quotation $str:s |} ] ->
            begin AstQuotation.set_default (FanToken.resolve_name (`Sub [],s)); loop () end 
        | _ -> (pl, false) ]
      else (pl, true) in
  let (pl0, eoi) = loop () in
  let pl =
    if eoi then []
    else
      let rec loop () =
        let (pl, stopped_at_directive) =
          Gram.parse_origin_tokens Syntax.implem  token_stream in  
        if stopped_at_directive <> None then pl @ loop () else pl in loop () in
  (* FIXME semantics imprecise, the filter will always be applied *)
  List.map (fun x -> Ast2pt.phrase (AstFilters.apply_implem_filters x) ) (pl0 @ pl);

let revise_parser =  wrap toplevel_phrase; 

begin
  Syntax.current_warning :=
    (fun loc txt ->
      Toploop.print_warning  loc Format.err_formatter
        (Warnings.Camlp4 txt));
  iter_and_take_callbacks (fun (_, f) -> f ());
end;

AstParsers.use_parsers
    ["revise";"stream";(* "debug"; *)"macro"(* ;"ListComprehension" *)];
  

let normal () = begin
  Toploop.parse_toplevel_phrase := Parse.toplevel_phrase;
  Toploop.parse_use_file := Parse.use_file;
end;
    
let revise ()  = begin
  Toploop.parse_toplevel_phrase := revise_parser;
  Toploop.parse_use_file := wrap use_file;
end;

begin 
  Hashtbl.replace Toploop.directive_table "revise"
    (Toploop.Directive_none (fun () -> revise ()));

  Hashtbl.replace Toploop.directive_table "pwd"
    (Toploop.Directive_none (fun () -> prerr_endline (Sys.getcwd ())));
  Hashtbl.replace Toploop.directive_table "normal"
    (Toploop.Directive_none (fun () -> normal ()))
end;










(* let token() = begin *)
(*   Toploop.parse_toplevel_phrase := wrap fake ; *)
(* end; *)
(* let fake token_stream = begin  *)
(*   try *)
(*     XStream.iter (fun (tok,_) -> *)
(*       if tok= `INT (3,"3") then raise Not_found *)
(*       else *)
(*         Format.fprintf Format.std_formatter *)
(*           "@[%a@]@." FanToken.print tok ) token_stream *)
(*   with *)
(*     [Not_found -> ()]; *)
(*   prerr_endline "got it"; *)
(*   Parsetree.Ptop_dir "pwd" Parsetree.Pdir_none; *)
(* end; *)

