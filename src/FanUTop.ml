open LibUtil;;
open Fan;;
let _ =  begin
  Topdirs.dir_directory "+compiler-libs"
end;;



(* FIXME copied from [FanTop] *)
let wrap parse_fun lb =
  let () = iter_and_take_callbacks (fun (_, f) -> f ()) in
  try
    let not_filtered_token_stream = FanLexUtil.from_lexbuf lb in
    let token_stream = Gram.filter  not_filtered_token_stream in
    match token_stream with parser (* FIXME *)
    [ [< (`EOI, _) >] -> raise End_of_file
    | [< >] -> parse_fun token_stream ]
  with
  | End_of_file | Sys.Break | (FanLoc.Exc_located (_, (End_of_file | Sys.Break))) as x ->
    raise x
  | (FanLoc.Exc_located (loc, y) ) -> begin
      Format.eprintf "@[<0>%a%s@]@."
        Toploop.print_location loc (Printexc.to_string y);
      raise Exit; (* commuiniation with toplevel special case here*)
  end
   | x ->  begin 
      Format.eprintf "@[<0>%s@]@." (Printexc.to_string x );
      raise Exit
  end 

let toplevel_phrase token_stream =
  match Gram.parse_origin_tokens Syntax.top_phrase token_stream with
  | Some stru ->
        let stru =
          (* Syntax.AstFilters.fold_topphrase_filters (fun t filter -> filter t) stru in *)
          AstFilters.apply_implem_filters stru in
        Ast2pt.phrase stru
  | None -> raise End_of_file 

  

let revise_parser str _bol =
  let () = iter_and_take_callbacks (fun (_,f) -> f ()) in  (* *)
  let eof = ref false in
  let lexbuf = UTop.lexbuf_of_string eof  str in
  try
    let not_filtered_token_stream = FanLexUtil.from_lexbuf lexbuf in
    let token_stream = Gram.filter not_filtered_token_stream in
    match XStream.peek token_stream with
    | Some (`EOI,_) -> (XStream.junk token_stream;raise End_of_file)
    | _ -> UTop.Value (toplevel_phrase token_stream) 
  with
  | End_of_file | Sys.Break | (FanLoc.Exc_located (_, (End_of_file | Sys.Break))) as x
      ->
        raise x
  |(FanLoc.Exc_located(_loc,y)) ->
        (UTop.Error ([(0,0)],Printexc.to_string y))
  

let normal () = begin
  UTop.parse_toplevel_phrase := UTop.parse_toplevel_phrase_default;
  Toploop.parse_use_file := Parse.use_file;
end;;

AstParsers.use_parsers
    ["revise";"stream";"macro"];;

let revise () = begin
  UTop.parse_toplevel_phrase := revise_parser;
  (* Toploop.parse_use_file := wrap use_file; *)
end;;


revise();;
let () = UTop_main.main ();;

begin 
  Hashtbl.replace Toploop.directive_table "revise"
    (Toploop.Directive_none (fun () -> revise ()));

  Hashtbl.replace Toploop.directive_table "pwd"
    (Toploop.Directive_none (fun () -> prerr_endline (Sys.getcwd ())));
  Hashtbl.replace Toploop.directive_table "normal"
    (Toploop.Directive_none (fun () -> normal ()))
end;;


(* ocamlfind ocamlmktop -custom -o m -thread -linkpkg -package utop FanUTop.cma *)
