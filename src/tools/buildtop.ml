




(* let ppf_id fmt (x:Ident.t) = Format.fprintf "%s" (Ident.name x);; *)



let _ =  begin
  Hashtbl.replace Toploop.directive_table "pwd"
    (Toploop.Directive_none (fun () -> prerr_endline (Sys.getcwd ())));
  Findlib.init ();
  Topdirs.dir_directory "+compiler-libs";
  Topdirs.dir_remove_directory ".";
  let b = "_build" in 
  if Sys.file_exists b then
    Topdirs.dir_directory b
  else ();
  Toploop.max_printer_steps:=1000;
end;;
let () = UTop_main.main ();;


(* Local Variables: *)
(* compile-command: "ocamlfind ocamlc -package compiler-libs -package utop -c buildtop.ml && ocamlfind ocamlmktop -verbose -custom -o otop -thread -linkpkg -package utop buildtop.cmo" *)
(* End:     *)
  
