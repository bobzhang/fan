

module P = MakePreCast.Make FanLexer.Make ;
open P;
open FanSig;  
  let wrap parse_fun lb =
    let () = iter_and_take_callbacks (fun (_, f) -> f ()) in
    let not_filtered_token_stream = Lexer.from_lexbuf lb in
    let token_stream = Gram.filter  not_filtered_token_stream in
    try  match token_stream with parser
      [ [< (EOI, _) >] -> raise End_of_file
      | [< >] -> parse_fun token_stream ]
    with
    [ End_of_file | Sys.Break | (FanLoc.Exc_located _ (End_of_file | Sys.Break))
        as x -> raise x
    | x ->
        let x =
          match x with
         [ FanLoc.Exc_located loc x -> begin
              Toploop.print_location Format.err_formatter loc;
              x
            end
          | x -> x ] in begin
              Format.eprintf "@[<0>%s@]@." (Printexc.to_string x );
              raise Exit
          end ];

let toplevel_phrase token_stream =
  match Gram.parse_origin_tokens
      (Syntax.top_phrase : P.Gram.t (option Ast.str_item)) token_stream with
    [ Some str_item ->
        let str_item =
          Syntax.AstFilters.fold_topphrase_filters (fun t filter -> filter t) str_item in
        Ast2pt.phrase str_item
    | None -> raise End_of_file ];

let use_file token_stream =
  let (pl0, eoi) =
    loop () where rec loop () =
      let (pl, stopped_at_directive) =
        Gram.parse_origin_tokens Syntax.use_file token_stream
      in
      if stopped_at_directive <> None then
        match pl with
        [ [ <:str_item< #load $str:s >> ] ->
            do { Topdirs.dir_load Format.std_formatter s; loop () }
        | [ <:str_item< #directory $str:s >> ] ->
            do { Topdirs.dir_directory s; loop () }
        | _ -> (pl, False) ]
      else (pl, True)
  in
  let pl =
    if eoi then []
    else
      loop () where rec loop () =
        let (pl, stopped_at_directive) =
          Gram.parse_origin_tokens Syntax.use_file token_stream
        in
        if stopped_at_directive <> None then pl @ loop () else pl
  in List.map Ast2pt.phrase (pl0 @ pl);


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

let open Camlp4Parsers in  begin
   pa_r (module P);
   pa_rp (module P);
   (* pa_qb; *)
   pa_q (module P);
   pa_g (module P);
   pa_l (module P);
   pa_m (module P);
end;

let normal () = begin
  Toploop.parse_toplevel_phrase := Parse.toplevel_phrase;
end;
    
let revise ()  = begin
  Toploop.parse_toplevel_phrase := revise_parser;
end;
(* Camlp4Parsers.pa_r (module P); *)
(* Camlp4Parsers.pa_rq (module P); *)






(* parser [ [<x;'z;'y>] -> z ] *)






