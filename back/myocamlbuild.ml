(**
   configuration syntax extensions
   the key is used by ocamlfind query to get its path.
   for example: ocamlfind query bitstring
 *)    
let syntax_lib_file
    = ["bitstring",[`D "bitstring.cma" ;
		    `D "bitstring_persistent.cma";
		    `D "pa_bitstring.cmo"]
         ;"ulex",     [`D "pa_ulex.cma"]
         ;"bolt",     [`D "bolt_pp.cmo"]
         ;"xstrp4",   [`D "xstrp4.cma"]
         ;"sexplib",     [`P ("type-conv", "Pa_type_conv.cma"); `D "pa_sexp_conv.cma"]
         ;"mikmatch_pcre", [`D "pa_mikmatch_pcre.cma"]
         ;"meta_filter",    [`D "meta_filter.cma"]
         ;"text", [`D "text.cma"; `D "text-pcre-syntax.cma"]
         ;"type_conv", [`D "pa_type_conv.cma"]
         ;"js_of_ocaml", [`D "pa_js.cmo"]   
     ]
let syntax_lib_file_cache
    = ".syntax_lib_file_cache"
let syntax_path syntax_lib_file = (
  if Sys.file_exists syntax_lib_file_cache then begin
    Log.dprintf 2 "read from .syntax_lib_file_cache";
    let chin = open_in syntax_lib_file_cache in 
    let lst = Marshal.from_channel chin in
    (* List.iter (fun (package,(x,y)) -> (flag x y )) lst ; *)
    List.iter (fun (x,_) ->
      try
        let (a,b) = List.assoc x lst in
        flag a b 
      with
        Not_found ->
          Log.dprintf 2 "syntax package %s not setup" x ) syntax_lib_file;
    close_in chin ;
  end 
  else begin
    Log.dprintf 2  ".syntax_lib_file_cache not found";
    let chan = open_out syntax_lib_file_cache in
    let args = ref [] in 
    flip List.iter syntax_lib_file (fun (package, files) ->
      try
        (let package_path =
	  try
	    trim_endline & run_and_read ("ocamlfind query " ^ package )
	  with Failure _ ->
	    prerr_endlinef "package %s does not exist" package;
	    raise Next 
        in
        if Sys.file_exists package_path then
	  let all_path_files  =
	    List.map (fun file ->
	      match file with
	      | `D file ->
		  if Sys.file_exists (package_path//file)
		  then (package_path // file)
		  else
		    (prerr_endlinef "%s does not exist "
                       (package_path//file);
		     raise Next)
	      | `P (package,file) ->
		  let sub_pack =
		    try
		      trim_endline & run_and_read ("ocamlfind query " ^ package)
		    with Failure _ -> begin 
		      prerr_endlinef "%s does not exist in subpackage definition" package;
		      raise Next
		    end 
		  in
		  if Sys.file_exists (sub_pack//file) then
		    (sub_pack // file)
		  else
		    (prerr_endlinef "%s does not exist " (sub_pack//file);
		     raise Next )
	             ) files
	  in begin
            args :=
              (package,
               (["ocaml"; "pp"; "use_"^ package],
                (S(List.map (fun file -> A file)
		     all_path_files)))) ::!args
          end 
        else begin 
	  prerr_endlinef "package %s does not exist" package;
        end 
        )
      with Next -> ());
    Marshal.to_channel chan !args [];
    List.iter (fun (package, (x,y)) -> flag x y ) !args;
    close_out chan
  end )

(* should be depracated, we use syntax_cache *)
(* let find_syntaxes () = ["camlp4o"; "camlp4r"] *)
let extensions () = 
  let pas = List.filter 
      (fun x ->
        String.contains_string x  0 "pa_" <> None) (find_packages ()) in 
  let tbl = List.map 
      (fun pkg -> 
        let dir = 
          trim_endline (run_and_read ("ocamlfind query " ^ pkg))in 
        (pkg, dir)) pas in 
  tbl
(** not turned on by default *)    
let _ = 
  if !Options.debug then begin 
    List.iter (fun (pkg,dir) -> Printf.printf "%s,%s\n" pkg dir)
      (extensions ()); 
    Printf.printf "%s\n" (site_lib())
  end
