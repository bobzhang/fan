
(**   A mini library adapted  for Utop *)



let print_fan_error pp exn =
    Format.fprintf pp "@[<0>%s@]@." (Printexc.to_string exn)

let get_fan_error_message exn =
  let (loc, exn) =
    match exn with
    | Locf.Exc_located (loc, exn) ->
        ((loc.loc_start.pos_cnum, loc.loc_end.pos_cnum), exn)
    | exn -> ((0, 0), exn)  (* FIXME *)in
  let msg = UTop.get_message print_fan_error exn in
  let idx = ref (String.length msg - 1) in
  begin 
    while !idx > 0 && msg.[!idx] = '\n' do
      decr idx
    done;
    if !idx + 1 < String.length msg then
      (loc, String.sub msg 0 (!idx + 1))
    else
      (loc, msg)
  end

  

let revise_parser str _bol =
  let eof = ref false in
  let lexbuf = UTop.lexbuf_of_string eof  str in
  try
    let stream = Lex_fan.from_lexbuf lexbuf in
    (* let token_stream = Gramf.filter not_filtered_token_stream in *)
    match Streamf.peek stream with
    | Some (`EOI _) -> (Streamf.junk stream;raise End_of_file)
    | _ -> UTop.Value (Mktop.toplevel_phrase stream) 
  with
  | End_of_file | Sys.Break | (Locf.Exc_located (_, (End_of_file | Sys.Break))) as x
      ->
        raise x
  |(Locf.Exc_located(_loc,y)) ->
        (UTop.Error ([(0,0)],Printexc.to_string y))
  

let normal () = begin
  UTop.parse_toplevel_phrase := UTop.parse_toplevel_phrase_default;
  Toploop.parse_use_file := Parse.use_file;
end;;


let fan () = begin
  UTop.parse_toplevel_phrase := revise_parser;
  (* Toploop.parse_use_file := wrap use_file; *) (* FIXME added later*)
end;;






begin
  (* Topdirs.dir_directory "+compiler-libs"; *)
  Hashtbl.replace Toploop.directive_table "fan"
    (Toploop.Directive_none (fun () -> fan ()));

  Hashtbl.replace Toploop.directive_table "pwd"
    (Toploop.Directive_none (fun () -> prerr_endline (Sys.getcwd ())));
  Hashtbl.replace Toploop.directive_table "normal"
    (Toploop.Directive_none (fun () -> normal ()));
  Ast_parsers.use_parsers ["fan"]
end;;

begin
  (* Topdirs.dir_install_printer *)
  (*   Format.std_formatter *)
  (*   (Longident.Ldot ((Longident.Lident "Gram"),"dump")); *)
  (** The only place where side effect  happen *)
  (* AstParsers.use_parsers ["revise";"stream";"macro"]; *)
  fan();
end;;


let () = UTop_main.main ();;

