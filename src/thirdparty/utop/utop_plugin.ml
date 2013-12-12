
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

  

let revise_parser str _eos_is_error =
  let eof = ref false in
  let lexbuf = UTop.lexbuf_of_string eof  str in
  try
    let stream = Lex_fan.from_lexbuf lexbuf in
    match Streamf.peek stream with
    | Some (`EOI _) -> (Streamf.junk stream;raise End_of_file)
    | _ -> UTop.Value (Mktop.toplevel_phrase stream) 
  with
  | End_of_file | Sys.Break | (Locf.Exc_located (_, (End_of_file | Sys.Break))) 
      ->
        raise UTop.Need_more
  |(Locf.Exc_located(_loc,y)) ->
        (UTop.Error ([(0,0)],Printexc.to_string y))


        
(* let parse_use_file str _eos_is_error = *)
  (* let eof = ref false in *)
  (* let lexbuf = UTop.lexbuf_of_string eof  str in *)
  (* try *)
  (*   let stream = Lex_fan.from_lexbuf lexbuf in *)
  (*   match Streamf.peek stream with *)
  (*   | Some (`EOI _) -> (Streamf.junk stream;raise End_of_file) *)
  (*   | _ -> UTop.Value (Mktop.use_file stream)  *)
  (* with *)
  (* | End_of_file | Sys.Break | (Locf.Exc_located (_, (End_of_file | Sys.Break)))  *)
  (*     -> *)
  (*       raise UTop.Need_more *)
  (* |(Locf.Exc_located(_loc,y)) -> *)
  (*       (UTop.Error ([(0,0)],Printexc.to_string y)) *)
  (* assert false *)

(* let parse_use_file str _eos_is_error =  *)
(*   Mktop.wrap Mktop.use_file ~print_location:Toploop.print_location str  *)


let normal () = begin
  UTop.parse_toplevel_phrase := UTop.parse_toplevel_phrase_default;
  UTop.parse_use_file := UTop.parse_use_file_default;
end;;


let fan () = begin
  UTop.parse_toplevel_phrase := revise_parser;
  Toploop.parse_use_file :=
    Mktop.wrap Mktop.use_file ~print_location:Toploop.print_location;

  (* UTop.parse_use_file := parse_use_file; (\* FIXME added later*\) *)
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

