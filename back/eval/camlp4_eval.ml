
open Format;
module C2O =
  Camlp4.Struct.Camlp4Ast2OCamlAst.Make
    Camlp4.PreCast.Ast;
value s2s s : Parsetree.toplevel_phrase = Obj.magic (C2O.phrase s);
  
<:fan< lang "str_item" ; >> ;
value _loc = Camlp4.PreCast.Loc.mk "<string>" ;
value preload_objects = ref [];
value prepare ppf = begin 
  Toploop.set_paths ();
  try begin
    let res =
      List.for_all (Topdirs.load_file ppf) (List.rev !preload_objects) in
    let () = !Toploop.toplevel_startup_hook () in 
    res
  end  with [x ->
    try let () = Errors.report_error ppf x in  False
    with x -> begin 
      Format.fprintf ppf "Uncaught exception: %s\n" (Printexc.to_string x);
      False
    end]
end ;


value first_line = ref True;
value got_eof = ref False;
value read_input_default prompt buffer len =
  (output_string Pervasives.stdout prompt;
   flush Pervasives.stdout;
   let i = ref 0;
   try
     (while True do if i.val >= len then raise Exit else ();
        let c = input_char Pervasives.stdin; buffer.[i.val] := c; incr i;
        if c = '\n' then raise Exit else () done;
      ((i.val), False))
   with [ End_of_file -> ((i.val), True) | Exit -> ((i.val), False) ]);
value read_interactive_input = ref read_input_default;
value refill_lexbuf buffer len =
  if got_eof.val
  then (got_eof.val := False; 0)
  else
    let prompt =
      if Clflags.noprompt.val
      then ""
      else
        if first_line.val
        then "# "
        else
          if Clflags.nopromptcont.val
          then ""
          else if Lexer.in_comment () then "* " else "  "
    in
      (first_line.val := False;
       let (len, eof) = read_interactive_input.val prompt buffer len;
       if eof
       then
         (Location.echo_eof ();
          if len > 0 then got_eof.val := True else ();
          len)
       else len);

exception PPerror;  
value loop ppf = begin
   let lb = Lexing.from_function refill_lexbuf in 
   let () = Location.init lb "//toplevel//" in  
   let () = Location.input_name.val := "//toplevel//" in 
   let () = Location.input_lexbuf.val := Some lb in 
   let () = Sys.catch_break True in 
   while True do let snap = Btype.snapshot ();
     try
       (Lexing.flush_input lb;
        Location.reset ();
        first_line.val := True;
        let phr =
          try Toploop.parse_toplevel_phrase.val lb with [ Exit -> raise PPerror ];
        Env.reset_missing_cmis ();
        ignore (Toploop.execute_phrase True ppf phr))
     with [ End_of_file -> exit 0
     | Sys.Break -> (fprintf ppf "Interrupted.@."; Btype.backtrack snap)
     | PPerror -> ()
     | x -> (Errors.report_error ppf x; Btype.backtrack snap) ] done
end ;
  

(*
  FIXME do the type checking at compile time, only load lambda at the runtime?
  If that environment does not work, so we can only do parsing to the ast at
  compile time??
 *)
value eval_ast ast = do{
  let snap = Btype.snapshot ();
  try (
    Env.reset_missing_cmis ();
    ignore (Toploop.execute_phrase True std_formatter (s2s ast)))
  with
    [Sys.Break ->
      (eprintf "Interrupted.@."; Btype.backtrack snap)
    |PPerror -> ()
    | x -> (Errors.report_error err_formatter x ;
            Btype.backtrack snap)]
};

open Camlp4.PreCast; (* module Lexer conflict withs Lexer *)

value main () = begin
  if (not (prepare Format.err_formatter)) then exit 2 else () ;
  loop std_formatter;
end ;
Toploop.initialize_toplevel_env ();
Sys.catch_break True;    
main ();

eval_ast <<
     open Pervasives;
     value f x = x ;
     print_int (f 3);
     (* print_newline (); *)
     print_int 2;   
>> ;
       
eval_ast <<
  print_int 3 ;
  print_int (f 3);
  Format.printf "%d" (f 3);
>> ;

eval_ast <<
  module X = Pervasives;
  print_newline ();
>> ;      
