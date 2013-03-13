
(* let ppf_id fmt (x:Ident.t) = Format.fprintf "%s" (Ident.name x);; *)



let _ =  begin
  Topdirs.dir_directory "+compiler-libs"
end;;
let () = UTop_main.main ();;


(* Local Variables: *)
(* compile-command: "ocamlfind ocamlc -package compiler-libs -package utop -c myutop_main.ml && ocamlfind ocamlmktop -custom -o m -thread -linkpkg -package utop myutop_main.cmo" *)
(* End:     *)
