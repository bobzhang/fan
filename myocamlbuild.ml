(*  interactive with toplevel
    #directory "+ocamlbuild";;
    #load "ocamlbuildlib.cma";;
    for interactive debugging
 *)
open Ocamlbuild_plugin
open Ocamlbuild_pack  
open Command
open Format  
open Tags.Operators
open Tags


(*
  Extend module [Options]
 *)  
module Options = struct
  include Options
  (* create flags for camlp4.opt, camlp6, camlp6.opt series
     should be deprecated when fan is mature enough
   *)    
  let p4s = ref ["camlp4",".opt"; "camlp4t",".byte"; "camlp4t",".native"]
  let p4_suffix = ref ["";"o";"r";"of";"rf";"orf";"oof"]
  let p4_opt = ref false
  (* modules to be documented *)    
  let doc_modules = ref StringSet.empty
  let verbose = ref false
  let debug = ref false
  (* handle .mllib files *)    
  let lib_files: (string, string list)Hashtbl.t = Hashtbl.create 50
  (* handle .itarget files *)    
  let target_files: (string,string list)Hashtbl.t = Hashtbl.create 50 
  let version = "1.0"
  (* compile time, not precise enough *)    
  let time = Unix.(
    let {tm_mon;tm_mday;tm_year;
         tm_hour;tm_min;
         tm_sec} =gmtime (time ()) in
    sprintf "%02d-%02d-%04d %02d:%02d:%02d UTC"
      (tm_mon+1) tm_mday (tm_year + 1900)
      tm_hour tm_min tm_sec
   )
end

(* Utility modules *)    
module Util = struct   
  let flip f x y = f y x
      (* ad-hoc trim endline *)    
  let trim_endline str = 
    let len = String.length (str) in 
    if len = 0 then str 
    else if str.[len-1] = '\n' 
    then String.sub str 0 (len-1)
    else str
  type 'a cont = 'a -> exn
      (* usage
         [calcc (fun k -> body)] k will get quick return 
       *)    
  let callcc  (type u) (f: u cont -> u)  =
    let module M = struct exception Return of u end in
    try f (fun x -> raise (M.Return x))
    with M.Return u -> u
  module String = struct
    include String
    let ends_with s e =
      let ne = String.length e
      and ns = String.length s in (ns >= ne) &&
      (callcc (fun k ->
        let diff = ns - ne in 
        for i = 0 to ne - 1 do
          if unsafe_get s (diff + i) <> unsafe_get e i then
            raise & k false
        done;
        true ))
  end 
      (* mainly in place of _tags file, internal usage only  *)          
  let merge_files files =
    String.concat "or"
      (List.map (fun f -> "<" ^ f ^ ">") files)
  let merge_tags tags = String.concat "," tags
  let input s = Configuration.parse_string s   
  let prerr_endlinef fmt =
    ksprintf (fun str-> if !Options.verbose then prerr_endline str) fmt
  let run_and_read      = Ocamlbuild_pack.My_unix.run_and_read
  let blank_sep_strings = Ocamlbuild_pack.Lexers.blank_sep_strings
  let opt_bind x f = match x with
  |Some v -> f v
  |None -> None

  let buffer_size = 8192
  let buffer = String.create buffer_size
  let file_copy input_name output_name =  Unix.(
    let () = Log.dprintf 1 "%s --> %s\n" input_name output_name in 
    let fd_in = openfile input_name [O_RDONLY] 0 in
    let fd_out = openfile output_name [O_WRONLY; O_CREAT; O_TRUNC] 0o666 in
    let rec copy_loop () = match read fd_in buffer 0 buffer_size with
    |  0 -> ()
    | r -> ignore (write fd_out buffer 0 r); copy_loop ()
    in
    copy_loop ();
    close fd_in;
    close fd_out)
  let update f path = let (//) = Filename.concat in 
    Sys.readdir path |> Array.iter
      (fun x ->
        let x = (if path<>"." then path // x else x ) in 
        if f x  then
          let target =  "_build"  // x in
          try 
            let {Unix.st_mtime=m_x ;_ } = Unix.stat x in
            if Sys.file_exists target then
              let {Unix.st_mtime=m_y;_} = Unix.stat target in
              if m_x > m_y then begin 
                Sys.remove target;
                file_copy x target 
              end 
              else ()
            else begin
              let subdir = "_build"// path in 
              if not (Sys.file_exists subdir) then  (* FIXME  more precise *)
                Unix.(mkdir subdir 0o755)
              else ();
              (file_copy x target)
            end
          with
            e -> begin
              Format.eprintf "%s" (Printexc.to_string e);
              Format.eprintf "ignoreing a file %s" x ;
            end 
      )


  type file_type =
    | Inferred | Ml | Mli | Pp_ml | Ppo_ml | Cmo | Cma | Cmi | Cmx
    | Cmxa | Cmxs  | Mllib    | Mldylib  | Odocl | Itarget 
  let string_of_file_type = function
    | Inferred -> ".inferred.mli" | Mli -> ".mli"
    | Ml -> ".ml"  | Pp_ml -> "_pp.ml" | Ppo_ml -> "_ppo.ml"
    | Cmo -> ".cmo" | Cma -> ".cma"    | Cmi -> ".cmi"
    | Cmx-> ".cmx"  | Cmxa -> ".cmxa"  | Cmxs -> ".cmxs"
    | Mllib -> ".mllib"  | Mldylib -> ".mldylib"
    | Odocl -> ".odocl"  | Itarget -> ".itarget"

          (* mainly used to represent _tags file *)          
  module Opt = struct     
    let (|*>) = tag_file
    let (|**>) files tag = List.iter (fun f -> f |*> tag) files
    let (//) = Filename.concat      
    let (/*>) base ty =
      base ^ (string_of_file_type ty)
    let (|-?) fileA files = dep ["ocamldep"; "file:"^fileA] files 
    let (|-??) fileAs files = List.iter (fun fileA -> fileA |-? files) fileAs
    let (<+>) files tags = begin 
      let src = (merge_files files ^ ":" ^ merge_tags tags) in
      Log.dprintf 2 "tags: %s\n" src;
      input src
    end 
  end
  module StringSet = struct
    include StringSet
    let of_list = List.fold_left (flip StringSet.add) StringSet.empty
  end 
      (* cross products for modules to add extensions *)
  let add_extensions extensions modules =
    List.fold_right begin fun x ->
      List.fold_right begin fun ext acc ->
        (if List.exists (String.ends_with x)
            [".cmo"; ".cma"; ".cmx"; ".cmxs"]
        then x
        else x -.-ext)
        :: acc
      end extensions
    end modules []
  let file_deps   (bs:string list) (tybs: file_type list)  (deps:string list)
      (tydeps:file_type list) = let open List in let open Opt in 
      let files = concat & map (fun f ->
        (map (fun ty -> f /*> ty) tydeps)) deps in
      let bases = concat & map (fun f ->
        (map (fun ty -> f /*> ty) tybs)) bs in
      bases |-?? files
  let deps_mli f files = file_deps [f]  [Ml;Pp_ml;Ppo_ml] files [Inferred]
  let deps_mli_table (tbl: (string* string list ) list) =
    List.iter (fun (s,lst) ->
      deps_mli s lst ) tbl

(*****************************************************************************************)
(***************************** mainly for camlp4 driver, deprecated now ******************)      
  let p4  = Pathname.concat "camlp4"
  let pa  = Pathname.concat (p4 "Camlp4Parsers")
  let pr  = Pathname.concat (p4 "Camlp4Printers")
  let fi  = Pathname.concat (p4 "Camlp4Filters")
  let top = Pathname.concat (p4 "Camlp4Top")
  let pa_r  = pa "Camlp4OCamlRevisedParser"
  let pa_o  = pa "Camlp4OCamlParser"
  let pa_q  = pa "Camlp4QuotationExpander"
  let pa_qc = pa "Camlp4QuotationCommon"
  let pa_rq = pa "Camlp4OCamlRevisedQuotationExpander"
  let pa_oq = pa "Camlp4OCamlOriginalQuotationExpander"
  let pa_rp = pa "Camlp4OCamlRevisedParserParser"
  let pa_op = pa "Camlp4OCamlParserParser"
  let pa_g  = pa "Camlp4GrammarParser"
  let pa_l  = pa "Camlp4ListComprehension"
  let pa_macro = pa "Camlp4MacroParser"
  let pa_debug = pa "Camlp4DebugParser"
  let pr_dump  = pr "Camlp4OCamlAstDumper"
  let pr_r = pr "Camlp4OCamlRevisedPrinter"
  let pr_o = pr "Camlp4OCamlPrinter"
  let pr_a = pr "Camlp4AutoPrinter"
  let mk_camlp4_bin name  modules = let open Opt in 
  let name = name in
  let byte = name-.-"byte" in
  let native = name-.-"native" in
  let deps = modules @ [p4 "Camlp4Bin"] in
  let cmos = add_extensions ["cmo"] deps in
  let cmxs = add_extensions ["cmx"] deps in begin
    rule byte
      ~prod:byte
      begin fun _ _ ->
        Cmd(S[!Options.ocamlc;  
              A "dynlink.cma"; A"unix.cma";
              T(tags_of_pathname byte++"ocaml"++"link"++"byte");
              P (p4 "camlp4lib.cma");
              A"-linkall";
              atomize cmos;
              A"-o";
              Px (byte)])
      end;
    rule native
      ~prod:native
      begin fun _ _ ->
        Cmd(S[!Options.ocamlopt;
              A "dynlink.cmxa"; A"unix.cmxa";
              T(tags_of_pathname native++"ocaml"++"link"++"native");
              P (p4 "camlp4lib.cmxa");
              A"-linkall"; atomize cmxs;
              A"-o";
              Px  native])
      end
  end
  let mk_camlp4_series prefix modules = begin
    let mk_camlp4 s pa pr= mk_camlp4_bin (prefix^s) (pa @ pr @modules) in 
    mk_camlp4 "r"  [pa_r; pa_rp] [pr_a];
    mk_camlp4 "rf"
      [pa_r; pa_qc; pa_q; pa_rp; pa_g; pa_macro; pa_l] [pr_a];
    mk_camlp4 "o" [pa_r; pa_o; pa_rp; pa_op] [pr_a] ;
    mk_camlp4 "of"
      [pa_r; pa_qc; pa_q; pa_o; pa_rp; pa_op; pa_g; pa_macro; pa_l]
      [pr_a];
    mk_camlp4 "oof"
      [pa_r; pa_o; pa_rp; pa_op; pa_qc; pa_oq; pa_g; pa_macro; pa_l]
      [pr_a] ;
    mk_camlp4 "orf"
      [pa_r; pa_o; pa_rp; pa_op; pa_qc; pa_rq; pa_g; pa_macro; pa_l]
      [pr_a] ;
  end
  let use_p4="use_camlp4"
(*****************************************************************************************)            
end

    
open Util;;
open Opt;;            




let () =  begin 
  (* mk_camlp4_series "camlp4t" ["eval.cma"]; *)
end;;


    

(*stolen from Ocaml_specific.ml*)
module Driver = struct
  (* FIXME what will happen when
     the tag is a/b?
     the declaration is safe to put in after_rules
     usage: make local binaries and apply binaries immediately
   *)        
  let mk_local t tag =
    let name = tag /*> t in  
    let use_tag = "use_"^name in  ((function ()-> begin
      flag ["ocaml";"pp"; use_tag] (A name);
      dep ["ocamldep"; use_tag] [name];
      Log.dprintf 2  "create tag :%s" use_tag;
    end), use_tag)

  (**
     stolen from ocamlbuild source p4_flags can not be applied twice.
     it's already applied in [ocaml_specific.ml]
   *)    
  (* let p4_flags  = List.iter (fun p4 -> flag ["ocaml"; "pp"; p4] (A p4)) *)

  (* let p4_flags' = List.iter (fun (p4, flags) -> flag ["ocaml"; "pp"; p4] flags) *)

  let fan  ?(printer=A "o")
      tag i o env build = (
    let ml = env i and pp_ml = env o in
    (**  add a pp here to triger the rule pp,
         camlp4rf ==> camlp4rf  don't tag file pp,camlp4rf, it will be inherited by
         .cmo file, and cause trouble there  *)
    let tags = (((tags_of_pathname ml) ++ "ocaml" ++ "pp") ) ++ tag in
    (*
     * add a ocamldep here to trigger the rule
     *     ocamldep, use_geneq => examples/geneq.cma
     *     Rule.build_deps_of_tags will try to build the deps  *)
    let _deps = Rule.build_deps_of_tags build (tags ++ "ocamldep") in
    let pp = Command.reduce (Flags.of_tags tags) in
    match pp with
    | N -> begin
        Log.dprintf 0 "could not find pp flags for source %s, using cat instead" ml;
        Cmd (S[A"cat"; P ml; Sh ">"; Px pp_ml])
    end
    | _ ->
        Cmd (S[pp;  A "-printer"; printer; A "-o"; Px pp_ml; P ml])
    (* let pp = match pp with | N -> default | _ -> pp in *)
    (* Cmd (S [ pp; P ml; A "-printer";printer; A "-o"; Px pp_ml ]) *)
   )
  let infer_with_error_channel ?(ocamlc=Options.ocamlc) flags tag =
    let infer ml dlambda env build = let open Ocaml_utils in
    let ml = env ml and dlambda = env dlambda in
    let tags = tags_of_pathname ml ++ "ocaml" in
    Ocaml_compiler.prepare_compile build ml ;
    Cmd(S( [!ocamlc; ocaml_ppflags tags; ocaml_include_flags ml] @
          List.map (fun f -> A f) flags @
          [(if Tags.mem "thread" tags then A"-thread" else N);
          T(tags++tag); P ml; Sh"2>"; Px dlambda]) ) in
    infer 
  let infer_dlambda =  infer_with_error_channel ["-dlambda"] "infer_dlambda"
  let infer_drawlambda = infer_with_error_channel ["-drawlambda"] "infer_drawlambda"
  let infer_dparsetree = infer_with_error_channel ["-c";"-dparsetree"] "infer_dparsetree"
  let infer_dtypedtree = infer_with_error_channel ["-c"; "-dtypedtree"] "infer_dtypedtree"
  let infer_instr =  infer_with_error_channel ["-dinstr"] "infer_instr"
  let infer_dclambda =  infer_with_error_channel
      ~ocamlc:Options.ocamlopt ["-dclambda"] "infer_dclambda"
  let infer_dcmm = infer_with_error_channel
      ~ocamlc:Options.ocamlopt ["-dcmm"] "infer_dcmm" 
  let infer_dlinear = infer_with_error_channel
      ~ocamlc:Options.ocamlopt ["-dlinear"] "infer_dlinear";;
  let mk_odocl _ _ =
    let modules = String.concat "\n" (StringSet.elements !Options.doc_modules) in
    Cmd (S[A"echo"; Quote(Sh modules); Sh">"; P ("foo" /*> Odocl)])

  (* generate files for .mllib .mldylib .itarget *)    
  let mk_files file lst env build =
    let lst = String.concat "\n" lst  in
    Cmd (S[A"echo"; Quote(Sh lst); Sh ">"; P file ])

  let mk_lib tbl suffix env build =
    let m = env "%" in 
    let lst =
      try  Hashtbl.find tbl  m
      with Not_found -> begin
      Log.dprintf 2
        "Warning: %s not defined in table, using default  rule" m;
      raise Rule.Failed
    end in
    mk_files (m/*>suffix) lst env build
  let mk_mllib = mk_lib Options.lib_files Mllib
  let mk_mldylib = mk_lib Options.lib_files Mldylib
  let mk_itarget = mk_lib Options.target_files Itarget
  let mk_version _ _ = (
    let cmd =
      sprintf "let version = %S\n\
let compile_time = %S" 
    Options.version Options.time in
    Cmd (S[A"echo"; Quote (Sh cmd); Sh ">"; P"version.ml"]))
end;;
    
open Driver;;
(** My rules *)
 begin (
   (* let open List in *)
   (* p4_flags & concat & *)
   (*   map (fun s -> map (fun (pre,post) -> pre ^ s ^post ) *)
   (*       !Options.p4s )!Options.p4_suffix; *)
   rule "ocaml: ml & ml.depends  -> .dlambda" ~prod:"%.dlambda" ~deps:["%.ml";"%.ml.depends"]
     (infer_dlambda "%.ml" "%.dlambda");
   rule "ocaml: ml & ml.depends  -> .drawlambda"
     ~prod:"%.drawlambda" ~deps:["%.ml";"%.ml.depends"]
     (infer_drawlambda "%.ml" "%.drawlambda");
   rule "ocaml: ml  -> .dparsetree"
     ~prod:"%.dparsetree" ~deps:["%.ml"]
     (infer_dparsetree "%.ml" "%.dparsetree");
   rule "ocaml: ml -> .dtypedtree"
     ~prod:"%.dtypedtree" ~deps:["%.ml"]
     (infer_dtypedtree "%.ml" "%.dtypedtree");
   rule "ocaml: ml & ml.depends  -> .dinstr"
     ~prod:"%.dinstr" ~deps:["%.ml";"%.ml.depends"]
     (infer_instr "%.ml" "%.dinstr");
   rule "ocaml: ml & ml.depends  -> .dclambda"
     ~prod:"%.dclambda" ~deps:["%.ml";"%.ml.depends"]
     (infer_dclambda "%.ml" "%.dclambda");
   rule "ocaml: ml & ml.depends  -> .dcmm"
     ~prod:"%.dcmm" ~deps:["%.ml";"%.ml.depends"]
     (infer_dcmm "%.ml" "%.dcmm");
   rule "ocaml: ml & ml.depends  -> .dlinear"
     ~prod:"%.dlinear" ~deps:["%.ml";"%.ml.depends"]
     (infer_dlinear "%.ml" "%.dlinear");
   rule "ocaml: mldylib & cmx* & o* -> cmxs"
     ~tags:["ocaml"; "native"; "shared"; "library"]
     ~prods:["%.cmxs"]
     ~dep:"%.mldylib"
     (Ocaml_compiler.native_shared_library_link_mldylib "%.mldylib" "%.cmxs");
   rule "foo.odocl" ~prod:"foo.odocl" mk_odocl;
   rule "generate %.mllib" ~prod:"%.mllib" mk_mllib;
   rule "generate %.mldylib" ~prod:"%.mldylib" mk_mldylib;
   rule "generate %.itarget" ~prod:"%.itarget" mk_itarget;
   rule "version.ml" ~prod:"version.ml" mk_version;
   rule "preprocess: ml -> _ppr.ml" ~dep: "%.ml" ~prod:"%_ppr.ml"
    (fan ~printer:(A"r") "%_ppr.ml" "%.ml" "%_ppr.ml");
   rule "preprocess: ml -> _ppo.ml" ~dep: "%.ml" ~prod: "%_ppo.ml"
    (fan ~printer:(A"o") "%_ppo.ml" "%.ml" "%_ppo.ml");
   let myocamldoc tags =
     Ocaml_tools.ocamldoc_l_dir tags in 
   (* -- "extension:html" in  when you want use plugins
      you may want to remove extension  *)
   rule "ocamldoc: use plugin"
     ~dep:"%.odocl" ~stamp:"%.docdir/html.stamp" ~prod:"%.docdir/index.html"
     ~insert:`top
     (Ocaml_tools.document_ocaml_project ~ocamldoc:myocamldoc
       "%.odocl" "%.docdir/index.html" "%.docdir");
   rule "dypgen %.dyp -> %.ml "
     ~tags:["dypgen"] ~prods:["%.ml"] ~deps:["%.dyp"]
     begin fun env _ ->
       let dyp = env "%.dyp" in
       Cmd (S[A"dypgen.opt"; A"--no-mli";
              A"--merge-warning";
              A"--pv-token";
              A"--cpp-options"; A"-w" ; Px dyp])
     end ;
   
   rule "ocaml: mlx -> ml"  ~tags:["ocaml"]
     ~prods:["%.ml"]
     ~deps:["%.mlx"] begin fun env _ ->
       let mlx = env "%.mlx" in
       let ml = env "%.ml" in 
       (Cmd(S[A"cat"; P mlx; Sh ">"; Px ml]))
         (* (Oca-ml_compiler.byte_compile_ocaml_implem "-impl %.mlx" "%.cmo") *)
     end;
   
   (* rule "ocaml dependencies mlx"  ~prod:"%.mlx.depends" *)
   (*   ~dep:"%.mlx" *)
   (*   (Ocaml_tools.ocamldep_command "-impl %.mlx" "%.mlx.depends"); *)

   )
 end 


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
let menhir_opts = S [A"--dump";A"--explain"; A"--infer";]

let site_lib () =
  trim_endline (run_and_read ("ocamlfind printconf destdir"))

let argot_installed  () =
  try
    let path = (trim_endline & run_and_read "ocamlfind query argot") in 
    if Sys.(file_exists path) then  begin 
      flag ["ocaml"; "doc"]
        (S[A"-i";
           A path;
           A"-g";
           A"argot.cmo";
           (* A"-search"; *)
         ]);
      Log.dprintf 2 "argot plugin hooked to ocamldoc"
    end 
    else Log.dprintf 2 "argot not installed"
  with
    e -> Log.dprintf 2 "argot not installed"

(** handle package *)    
let find_packages () =
  blank_sep_strings &
    Lexing.from_string &
    run_and_read "ocamlfind list | cut -d' ' -f1"      

(** list extensions for debug purpose *)
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


exception Next
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
let find_syntaxes () = ["camlp4o"; "camlp4r"]
let ocamlfind x = S[A"ocamlfind"; x]
module Default = struct
  let before_options () = (
    Options.ocamlc     := ocamlfind & S[A"ocamlc";
                                        A"-annot";
                                        A "-w";
                                        A "+a-4-32-30";
                                        (* A "-4"; (\* otherwise, a lot of fragile pattern will be detected*\)  *)
                                        (* A "-bin-annot"; *)
                                        (* A"-warn-error"; *)
                                        (* A"A" *)
                                        (* A" 4-6-7-9-27..29"; *)
                                      ];
    Options.ocamlopt   := ocamlfind & S[A"ocamlopt";
                                        A"-annot";
                                        A"-w";
                                        A"+a-4-32-30";
                                        (* A"-unsafe"; *)
                                        A"-inline";
                                        A"100";
                                        (* A"-4"; *)
                                        (* A "-warn-error"; *)
                                        (* A "A" *)
                                        (* A"-bin-annot" *)
                                      ];
    Options.ocamldep   := ocamlfind & A"ocamldep";
    Options.ocamldoc   := ocamlfind & A"ocamldoc";
    (* Options.make_links := false; *)
    (* Options.ocamldoc := S [A "ocamldoc"]; *)
    (** ocamlfind does not accept -search
        ocamldoc.opt does not work on mac
     *)
    Options.ocamlmktop := ocamlfind & A"ocamlmktop")
  let after_rules () = (
    (*when one link an ocaml library/binary/package, should use -linkpkg*)
    flag ["ocaml"; "byte"; "link";"program"] & A"-linkpkg";
    flag ["ocaml"; "native"; "link";"program"] & A"-linkpkg";
    List.iter ( fun pkg ->
      flag ["ocaml"; "compile";  "pkg_"^pkg] & S[A"-package"; A pkg];
      flag ["ocaml"; "ocamldep"; "pkg_"^pkg] & S[A"-package"; A pkg];
      flag ["ocaml"; "doc";      "pkg_"^pkg] & S[A"-package"; A pkg];
      flag ["ocaml"; "link";     "pkg_"^pkg] & S[A"-package"; A pkg];
      flag ["ocaml"; "infer_interface"; "pkg_"^pkg] & S[A"-package"; A pkg];
      flag ["menhir"] menhir_opts; (* add support for menhir*)
    ) (find_packages ());
    (* Like -package but for extensions syntax. Morover -syntax is
     * useless when linking. *)
    List.iter ( fun syntax ->
      flag ["ocaml"; "compile";  "syntax_"^syntax] & S[A"-syntax"; A syntax];
      flag ["ocaml"; "ocamldep"; "syntax_"^syntax] & S[A"-syntax"; A syntax];
      flag ["ocaml"; "doc";      "syntax_"^syntax] & S[A"-syntax"; A syntax];
      flag ["ocaml"; "infer_interface";  "syntax_"^syntax] & S[A"-syntax"; A syntax];
    ) (find_syntaxes ());
    (* The default "thread" tag is not compatible with ocamlfind.
       Indeed, the default rules add the "threads.cma" or
       "threads.cmxa" options when using this tag. When using the
       "-linkpkg" option with ocamlfind, this module will then be
       added twice on the command line.
       To solve this, one approach is to add the "-thread" option when using
       the "threads" package using the previous plugin.
     *)
    flag ["ocaml"; "pkg_threads"; "compile"]  (S[A "-thread"]);
    flag ["ocaml"; "pkg_threads"; "link"]     (S[A "-thread"]);
    flag ["ocaml"; "pkg_threads"; "infer_interface"] (S[A "-thread"]);
    

    (****************************************************************)
    (* internal parsers *)
    flag["pp"  ; "ocaml"; "use_macro"]  (S[A"-parser"; A"macro"]);
    flag["pp"  ; "ocaml"; "use_map"] (S[A"-filter"; A"map"]);
    flag["pp"  ; "ocaml"; "use_meta"] (S[A"-filter"; A"meta"]);
    flag["pp"  ; "ocaml"; "use_trash"] (S[A"-filter"; A"trash"]);
    flag["pp"  ; "ocaml"; "use_lift"] (S[A"-filter"; A"lift"]);
    flag["pp"  ; "ocaml"; "use_fold"] (S[A"-filter"; A"fold"]);
    flag["pp"  ; "ocaml"; "use_debug"] (S[A"-parser"; A"debug"]);
    (****************************************************************)
    flag ["link";"ocaml";"g++";] (S[A"-cc"; A"g++"]);
    (* flag ["ocaml"; "doc"]  (S [A"-keep-code"]); *)
    (* argot_installed (); *)
    syntax_path syntax_lib_file ;
    flag ["ocaml"; "doc"; "use_camlp4"] (S[A"-I"; A"+camlp4"]);
   )
end 

(**************************************************************)
module PackageLinkFix =  struct
  let packages_in_dir dir = Array.fold_right (fun f l
    -> if (Pathname.check_extension f "mlpack") then
      (dir / (Pathname.remove_extension f)) :: l
    else l) (Sys.readdir dir)  []

  let byte_dep_mlpack arg out env _build =
    let arg = env arg and out = env out in
    Echo (([arg; ":"] @
           (List.map
              (fun s -> " "^s)
              (string_list_of_file (arg))) @
           ["\n"]), out)

  let ()  = 
    rule "ocaml dependencies mlpack"
      ~prod:"%.ml.depends"
      ~dep:"%.mlpack"
      (byte_dep_mlpack "%.mlpack" "%.ml.depends")
  let mlpack_dirs = ["src";"cold"] (* *)      
  let after_rules () = 
        List.iter
          (fun p ->
            dep ["ocaml"; "byte"; "pack"; "extension:cmo"; "file:"^p^".cmo"]
              [p^".ml.depends"])
      (List.concat (List.map packages_in_dir (List.map Pathname.mk mlpack_dirs)))
end

;;    
(**************************************************************)
    

type actions =  (unit -> unit) list ref
let before_options : actions = ref []
and after_options : actions = ref []
and before_rules : actions = ref []
and after_rules : actions = ref []
let (+>) x l =  l := x :: !l

(** demo how to use external libraries
    ocaml_lib ~extern:true "llvm";
    ocaml_lib ~extern:true "llvm_analysis";
    ocaml_lib ~extern:true "llvm_bitwriter";
    dep ["link"; "ocaml"; "use_plus_stubs"] ["plus_stubs.o"];
    flag["link"; "ocaml"; "byte"] (S[A"-custom"]);
    dep ["ocamldep"; "file:test_lift_filter_r.ml"] ["test_type_r.ml"];
    dep ["ocamldep"; "file:test_lift_filter.ml"] ["test_type.ml"];
    dep ["ocamldep"; "file:test_dump.ml"] ["test_type_r.ml"];
    dep ["ocamldep"; "file:test_lift_filter.pp.ml"] ["test_type.ml"];
    demo how to use dep
        dep ["ocamldep"; "file:test/test_string.ml"]
        ["test/test_data/string.txt";
        "test/test_data/char.txt"];
    *)


(** replace of _tags file *)
let tags_table : ((string list * string list) list) ref =  ref [];;

let before_options_dispatch = ref (fun () -> ())
let after_rules_dispatch = ref (fun () -> ())
let apply  before_options_dispatch after_rules_dispatch = (
  Default.before_options +> before_options;
  Default.after_rules +> after_rules;
  before_options_dispatch +> before_options;
  after_rules_dispatch +> after_rules;
  PackageLinkFix.after_rules +> after_rules;
  dispatch begin function
    | Before_options -> begin
        List.iter (fun f -> f () ) !before_options;
    end
    | After_rules -> begin
        List.iter (fun f -> f ()) !after_rules;
    end
    | _ -> ()
  end ;
 );;



(**************************************************************)
(*****************  Insert most your code here ****************)                           
(**************************************************************)

let root1 = "src";;
let root2 = "cold";;
let tmp = "tmp"    

let define_context_for_root r =
  let def = Pathname.define_context in   begin 
    def (r // "Camlp4/Printers") [ r //"Camlp4/Struct"; r // "Camlp4"; r] ;
    def (r // "Grammar") [r];
    def (r // "Lex") [r];
    def (r // "Lib") [r];
    def (r // "PLib") [r];
    def ("test") ["src"];
    def "testr" ["src"];
    def "llvm" ["src"];
    def ("testr"//"loc") ["src"];
    def ("testr"//"lex") ["src"];    
    def ("demo"//"plc") ["src"];
    def ("demo"//"graph") ["src"];

    (* the toplevel directory can see src, this is only for debugging convenience, you should never put any
       library code in toplevel, only test files
     *)
  end ;;
define_context_for_root root1;;
define_context_for_root root2;;

let boot_flags =
  S[P ("boot"//"fan"); (* symlink fan to either fan.byte or fan.native *)
    A"-printer"; A"p"];;

rule "code_boot: ml -> ml" ~dep: "src/%.ml" ~prod:(tmp//"%.ml")
    (fan  (tmp//"%.ml") "src/%.ml" (tmp//"%.ml"));;

rule "code_boot: mli -> mli" ~dep: "src/%.mli" ~prod:(tmp//"%.mli")
    (fan  (tmp//"%.mli") "src/%.mli" (tmp//"%.mli"));;

rule "code_boot: mlpack -> mlpack" ~dep: "src/%.mlpack" ~prod:(tmp//"%.mlpack")
    (fan  (tmp//"%.mlpack") "src/%.mlpack" (tmp//"%.mlpack"));;
rule "code_boot: mll -> mll" ~dep: "src/%.mll" ~prod:(tmp//"%.mll")
    (fan  (tmp//"%.mll") "src/%.mll" (tmp//"%.mll"));;

let () =
  Options.ocaml_lflags :=  [ "-linkall"] ;
  after_rules_dispatch := fun () -> begin
    flag ["ocaml"; "pp"; "use_fan"] boot_flags;
    flag ["ocaml"; "pp"; "use_fan"; "pp:doc"] (S[A"-printer"; A"o"]);
    "src/Camlp4Ast.ml" |-? ["src/Ast.ml"];
    "src/Lib/Ctyp.ml" |-? ["src/Lib/CommonStructure.ml" ];
    "src/Lib/Expr.ml" |-? ["src/Lib/CommonStructure.ml"; "src/Lib/ExprPatt.ml" ];
    "src/Lib/Patt.ml" |-? ["src/Lib/CommonStructure.ml"; "src/Lib/ExprPatt.ml" ];
    "src/StdMeta.ml"   |-? ["src/MetaTemplate.ml"];
    "src/FanAst.ml"   |-? ["src/MetaTemplate.ml"; "src/Ast.ml"]; 
  end;;


copy_rule "src/FanDriver.byte -> boot/FanDriver.byte"
  ~insert:`top "src/FanDriver.byte" "boot/FanDriver.byte";;
copy_rule "src/FanDriver.native -> boot/FanDriver.native"
  ~insert:`top "src/FanDriver.native" "boot/FanDriver.native";;



let _ = begin 
  apply !before_options_dispatch !after_rules_dispatch
end


(* let fan_quot_src =
 *   ["lib_common";
 *    "fan_sig";
 *    "fan_config";
 *    "fan_basic";
 *    "fan_dynamic_plugins";
 *    "fan_lang_meta";
 *    "fan_lang_include";
 *  ];;
 * (\** for fan library fan.cma *\)
 * let fan_src = ["fan_lang_asthook";
 *                "fan_config";
 *                "fan_basic";
 *           
 *                "fan_ctyp";
 *                "fan_dynamic_plugins";
 *                "fan_easy";
 *                "fan_expr";
 *                "fan_ident";
 *                "fan_module_type";
 *                "fan_patt";
 *                "fan_sig";
 *                "fan_transform";
 *                "fan_filter";
 *                "fan_lang_meta";
 *                "fan_lang_include";
 *                "fan_frame";
 *                (\* "fan_lang_compile"; *\)
 *                
 *                "common_base";
 *                "meta_base";
 *                "lib_common";
 *                "plugins/gen_print";
 *                "plugins/gen_meta";
 *                "plugins/gen_eq";
 *                "plugins/gen_map";
 *                "plugins/gen_fold";
 *              ];;
 * Options.doc_modules :=  StringSet.of_list &  ["fan_basic"];;
 * (\** use fan_lang_meta as a preprocess *\)
 * let use_parsings =
 *   [  "fan_expr.ml";
 *      "fan_patt.ml";
 *      "fan_ctyp.ml";
 *      "fan_transform.ml";
 *      "fan_ident.ml";
 *      "fan_module_type.ml";
 *      "fan_lang_asthook.ml";
 *      "fan_lang_compile.ml";
 *      "fan_frame.ml";
 *      "fan_filter.ml";
 *      "fan_ctyp_transform.ml";
 *      "fan_test_plugins.ml";
 *      "fan_eval.ml";
 *      "fan_easy.ml";
 *      "common_base.ml";
 *      "meta_base.ml";
 *      "code_template.ml";
 *      "meta_template.ml";
 *      "miniml/mlast_anti.ml";
 *    ];;
 * let ocaml_files = [ "camlp4ast";      
 *     "ident";      
 *     "lexing";     
 *     "location";   
 *     "longident";  
 *     "parsetree";  
 *     "asttypes";   
 *     "path";       
 *     "typedtree";  
 *     "types";
 *     "primitive";
 *     "env"; ];;
 * 
 * begin
 *   let tbl = List.map (fun s ->
 *     "ocaml"//(s^"_g"),
 *     ["ocaml"//s^"_i"]) ocaml_files in
 *   deps_mli_table tbl ;
 *   ["ocaml/compiler_ext.ml"; "ocaml/camlp4_ext.ml;";] |-??
 *   ((List.map (fun s -> "ocaml"// (s^"_i") /*> Inferred)) ocaml_files);
 * end ;;
 * 
 * (\* file level dependency *\)
 * begin
 *   ["common_base.ml";"common_base.cmo";"common_base.cmx"]
 *   |-?? ["code_template.cmo"];
 * end ;;
 * let mllib_table =
 *   [ "dyplib/dyp",
 *     ["dyp";"priority_by_relation";"automaton";"dyplex"];
 *     "fan_quot", fan_quot_src ;
 *     "fan", fan_src;
 *   ] in
 * List.iter (fun (name,modules) ->
 *   Hashtbl.replace Options.lib_files name modules) mllib_table ;;
 * let itarget_table =
 *   ["fan", [  "fan.cma" ;]] in
 * List.iter (fun (name,modules) ->
 *   Hashtbl.replace Options.target_files name modules) itarget_table;;
 * let (declare_fan,use_fan) = mk_local Cma "fan" ;;
 * let (declare_fan_quot,use_fan_quot) = mk_local Cma "fan_quot" ;;
 * 
 * let backend = "dyplib/dypgen_backend_camlp4.ml";;
 * begin
 *   tags_table :=
 *     [
 *      ["**/fan_*.ml";"**/*_ppr.ml"; "plugins/*.ml";
 *       "code_template.ml"; "common_base.ml"; "meta_base.ml";
 *       "meta_template.ml"; "test_dyp/test_parser.ml"; "**/lang_*.ml";
 *       "fan/*.ml" ] ,
 *      (\* ["camlp4trf.byte";use_p4]; *\)
 *      ["camlp4trf.byte";use_p4];
 *      ["**/fan_*.mli"],[use_p4];
 *      ["fan_test*.ml"; "miniml/mlast.ml"],[use_fan];
 *      ["ulex/gen_ulex.ml"],["camlp4trf.byte"; "use_camlp4"; use_fan;"use_camlp4"];
 *      ["ulex/cset.ml"], ["camlp4to.byte"; use_fan; "use_camlp4"];
 *      ["ulex/ulex.ml"], ["camlp4to.byte"; use_fan; "use_camlp4"];
 *      ["ulex/test*.ml"], ["camlp4to.byte"; "use_ulex"];
 *      ["eval.{ml,cmo,byte}"],
 *      ["pkg_compiler-libs.toplevel";"pkg_dynlink";"pkg_camlp4.lib"];
 *      ["eval.cma"], ["pkg_compiler-libs.toplevel"];
 *      ["fan.{cma}"], ["pkg_compiler-libs.toplevel"];
 *      ["test_fan_eval.{ml,cmo,byte}"],["pkg_dynlink"];
 *      ["eval/*.{ml,cmo,cma,byte}"],["pkg_compiler-libs"];
 *      ["miniml/mlast_parse.ml";"miniml/mlast_anti.ml"],["camlp4rf";use_p4];
 *      ["plugins/*.ml"], [use_fan_quot];
 *      ["ocaml/*.ml"], ["pkg_compiler-libs"; use_p4];
 *      ["ocaml/compiler_ext.ml";"ocaml/camlp4_ext.ml";"ocaml/test_meta.ml"],
 *      ["camlp4to.byte";use_fan];
 *      ["ocaml/camlp4_ext.ml"], [use_p4];
 *      ["ocaml/*.byte"], ["pkg_compiler-libs.common"];
 *      [backend], ["camlp4rf";use_p4];
 *      ["test_dyp/*.ml"; "test_dyp/*.byte";
 *       "test_meta/*.ml"; "test_meta/*.byte"], ["pkg_dyp"];
 *      ["Lexer.ml"],["pkg_camlp4.lib"];
 *      ["tdriver.ml";"tdriver.byte"],["pkg_camlp4.lib"];
 *     ];
 * end  *)

(* let () = begin
 *   after_rules_dispatch := fun () -> 
 *     List.iter (fun (xs,ys) -> xs <+> ys ) !tags_table;
 *     flag ["ocaml"; "library"; "file:eval.cma"]
 *       & A"-linkpkg"; (\* link toplevel*\)
 *     (\* flag ["ocaml"; "library"; "file:fan.cma"] & A"-linkpkg"; (\\* link toplevel*\\) *\)
 *     let () = (backend::use_parsings) |**> [use_fan_quot] in
 *     declare_fan_quot ();
 *     declare_fan ();
 * end  *)

(* let paths = [|"."; "eval" ; "dyplib"|]
 * let update_ml_files  = update (function x ->
 *     Filename.check_suffix x ".ml"
 *   || Filename.check_suffix x ".mli");;
 * Array.iter update_ml_files  paths;; *)
(* flag ["ocaml"; "compile"; "include_camlp4"] (S[A"-I";P "Camlp4"]);; *)
(* flag ["ocaml"; "ocamldep"; "include_camlp4"] (S[A"-I";P "Camlp4"]);; *)
(* copy boot/Camlp4Ast.ml to Camlp4/Struct/Camlp4Ast.ml *)
