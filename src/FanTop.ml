open LibUtil;
(* module P = PreCast.Make (struct end) ; *)
(* module P = Fan.P; *)
(* open Fan.P; *)
open Fan;
let wrap parse_fun lb =
  let () = iter_and_take_callbacks (fun (_, f) -> f ()) in
  let not_filtered_token_stream = FanLexUtil.from_lexbuf lb in
  let token_stream = Gram.filter  not_filtered_token_stream in
  try  match token_stream with parser (* FIXME *)
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
      (Syntax.top_phrase : Gram.t (option Ast.str_item)) token_stream with
    [ Some str_item ->
        let str_item =
          (* Syntax.AstFilters.fold_topphrase_filters (fun t filter -> filter t) str_item in *)
          AstFilters.apply_topphrase_filters str_item in
        Ast2pt.phrase str_item
    | None -> raise End_of_file ];


    
let fake token_stream = begin 
  try
    XStream.iter (fun (tok,_) ->
      if tok= `INT (3,"3") then raise Not_found
      else
        Format.fprintf Format.std_formatter
          "@[%a@]@." FanToken.print tok ) token_stream
  with
    [Not_found -> ()];
  prerr_endline "got it";
  Parsetree.Ptop_dir "pwd" Parsetree.Pdir_none;
end;

  
let use_file token_stream =
  let rec loop () =
      let (pl, stopped_at_directive) =
        Gram.parse_origin_tokens Syntax.implem token_stream
      in
      if stopped_at_directive <> None then (* only support [load] and [directory] *)
        with "str_item" match pl with
        [ [ {| #load $str:s |} ] ->
            begin  Topdirs.dir_load Format.std_formatter s; loop ()  end
        | [ {| #directory $str:s |} ] ->
            begin  Topdirs.dir_directory s; loop ()  end
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
  List.map Ast2pt.phrase (pl0 @ pl);


let revise_parser =  wrap toplevel_phrase; 
let _  =   begin
    Toploop.parse_toplevel_phrase := revise_parser;

    Toploop.parse_use_file := wrap use_file;

    Syntax.current_warning :=
    fun loc txt ->
      Toploop.print_warning  loc Format.err_formatter
        (Warnings.Camlp4 txt);
      
      iter_and_take_callbacks (fun (_, f) -> f ());
  end;

let open FanParsers in  begin
   pa_r (module Fan);
   pa_rp (module Fan);
   pa_q (module Fan);
   pa_g (module Fan);
   pa_l (module Fan);
   pa_m (module Fan);
end;

let normal () = begin
  Toploop.parse_toplevel_phrase := Parse.toplevel_phrase;
end;
    
let revise ()  = begin
  Toploop.parse_toplevel_phrase := revise_parser;
end;

let token() = begin
  Toploop.parse_toplevel_phrase := wrap fake ;
end;


Hashtbl.replace Toploop.directive_table "revise"
    (Toploop.Directive_none (fun () -> revise ()));





(* parser [ [<x;'z;'y>] -> z ] *)






