  
(* eval_ast <<
 *   open Camlp4.PreCast;
 *   prerr_endline "hehe";
 *   module X = Pervasives;
 * >>; *)

  
(* eval_ast <<
 *      open Pervasives;
 *      value f x = x ;
 *      print_int (f 3);
 *      (\* print_newline (); *\)
 *      print_int 2;   
 * >> ;
 *        
 * eval_ast <<
 *   print_int 3 ;
 *   print_int (f 3);
 *   Format.printf "%d" (f 3);
 * >> ;
 * 
 * eval_ast <<
 *   module X = Pervasives;
 *   print_newline ();
 * >> ; *)

(* Fan_camlp4syntax.add_quotation_of_str_item_with_filter
 *     ~name:"eval" ~entry:Syntax.str_items ~filter:(fun s -> begin 
 *       eval_ast s;
 *       << >>;
 *     end 
 *  ); *)
    
(* Env.summary (Toploop.toplevel_env.val); *)
(* exception PPerror;  
 * value loop ppf = begin
 *    let lb = Lexing.from_function refill_lexbuf in 
 *    let () = Location.init lb "//toplevel//" in  
 *    let () = Location.input_name.val := "//toplevel//" in 
 *    let () = Location.input_lexbuf.val := Some lb in 
 *    let () = Sys.catch_break True in 
 *    while True do let snap = Btype.snapshot ();
 *      try
 *        (Lexing.flush_input lb;
 *         Location.reset ();
 *         first_line.val := True;
 *         let phr =
 *           try Toploop.parse_toplevel_phrase.val lb with [ Exit -> raise PPerror ];
 *         Env.reset_missing_cmis ();
 *         ignore (Toploop.execute_phrase True ppf phr))
 *      with [ End_of_file -> exit 0
 *      | Sys.Break -> (fprintf ppf "Interrupted.@."; Btype.backtrack snap)
 *      | PPerror -> ()
 *      | x -> (Errors.report_error ppf x; Btype.backtrack snap) ] done
 * end ; *)
  
(* value first_line = ref True;
 * value got_eof = ref False;
 * value read_input_default prompt buffer len =
 *   (output_string Pervasives.stdout prompt;
 *    flush Pervasives.stdout;
 *    let i = ref 0;
 *    try
 *      (while True do if i.val >= len then raise Exit else ();
 *         let c = input_char Pervasives.stdin; buffer.[i.val] := c; incr i;
 *         if c = '\n' then raise Exit else () done;
 *       ((i.val), False))
 *    with [ End_of_file -> ((i.val), True) | Exit -> ((i.val), False) ]);
 * value read_interactive_input = ref read_input_default;
 * value refill_lexbuf buffer len =
 *   if got_eof.val
 *   then (got_eof.val := False; 0)
 *   else
 *     let prompt =
 *       if Clflags.noprompt.val
 *       then ""
 *       else
 *         if first_line.val
 *         then "# "
 *         else
 *           if Clflags.nopromptcont.val
 *           then ""
 *           else if Lexer.in_comment () then "* " else "  "
 *     in
 *       (first_line.val := False;
 *        let (len, eof) = read_interactive_input.val prompt buffer len;
 *        if eof
 *        then
 *          (Location.echo_eof ();
 *           if len > 0 then got_eof.val := True else ();
 *           len)
 *        else len); *)
(* value main () = begin
 *   if (not (prepare Format.err_formatter)) then exit 2 else () ;
 *   loop err_formatter;
 * end ; *)
(* main (); *)
(* Sys.catch_break True;     *)
(* Toploop.toplevel_env.val :=
 *   Env.open_pers_signature "Camlp4" Toploop.toplevel_env.val; *)




















