open Ocamlbuild_plugin
open Ocamlbuild_pack  
open Command
open Format  
open Tags.Operators
open Tags




(**  add a pp here to triger the rule pp,
     fan ==> fan  don't tag file pp,fan, it will be inherited by
     .cmo file, and cause trouble there *)
let fan  ?(printer=A "o") tag i o env build = 
    let ml = env i and pp_ml = env o in
    let tags = (((tags_of_pathname ml) ++ "ocaml" ++ "pp") ) ++ tag in
    (* add a ocamldep here to trigger the rule
       ocamldep, use_geneq => examples/geneq.cma
       Rule.build_deps_of_tags will try to build the deps  *)
    let _deps = Rule.build_deps_of_tags build (tags ++ "ocamldep") in
    let pp = Command.reduce (Flags.of_tags tags) in
    match pp with
    | N -> begin
        Log.dprintf 0 "could not find pp flags for source %s, using cat instead" ml;
        Cmd (S[A"cat"; P ml; Sh ">"; Px pp_ml])
    end
    | _ ->
        Cmd (S[pp;  A "-printer"; printer; A "-o"; Px pp_ml; P ml])
  
module Options = struct
  include Options
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
  let sep ?(pred=function (' '|'\n'|'\t') -> true | _ -> false) str  =
    let len = String.length str in 
    let rec aux start current  acc=
      if current >= len then
        match start with
        |Some x -> String.sub str x (current - x):: acc
        |None -> acc
      else 
        match start, str.[current] with
        | None,x ->
            if pred x  then 
              aux start (current+1) acc
            else
              aux (Some current)
                (current+1)
                acc
        | Some v,x ->
            if pred x then 
              aux
                None
                (current+1)
                (String.sub str v (current - v) :: acc)
            else
              aux
                start
                (current+1)
                acc  in
    List.rev (aux None 0 [])
      

  let flip f x y = f y x
        
  module String = struct
    
    include String
        (** ad-hoc trim endline *)    
    let trim_endline str = 
      let len = String.length (str) in 
      if len = 0 then str 
      else if str.[len-1] = '\n' 
      then String.sub str 0 (len-1)
      else str
        
    let ends_with s e =
      let ne = String.length e in
      let ns = String.length s in 
      try (String.sub s (ns-ne) ne )=e
      with e -> false
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

end

    
open Util;;
open Opt;;            



ocaml_lib ~extern:true "ocamlcommon" ~dir:"+compiler-libs";

ocaml_lib ~extern:true "ocamlcommon" ~dir:"+compiler-libs" ~tag_name:"use_ocamlbytecomp";
ocaml_lib ~extern:true "ocamlbytecomp" ~dir:"+compiler-libs" ~tag_name:"use_ocamlbytecomp";

ocaml_lib ~extern:true "ocamlcommon" ~dir:"+compiler-libs" ~tag_name:"use_ocamlnativecomp";
ocaml_lib ~extern:true "ocamloptcomp" ~dir:"+compiler-libs" ~tag_name:"use_ocamlnativecomp";

  
ocaml_lib ~extern:true "ocamlcommon" ~tag_name:"use_ocamltoplevel" ~dir:"+compiler-libs";
ocaml_lib ~extern:true "ocamlbytecomp" ~tag_name:"use_ocamltoplevel" ~dir:"+compiler-libs";
ocaml_lib ~extern:true "ocamltoplevel" ~tag_name:"use_ocamltoplevel" ~dir:"+compiler-libs";


(*stolen from Ocaml_specific.ml*)
module Driver = struct
  (* FIXME what will happen when the tag is a/b?
     the declaration is safe to put in after_rules
     usage: make local binaries and apply binaries immediately *)        
  let mk_local t tag =
    let name = tag /*> t in  
    let use_tag = "use_"^name in  ((function ()-> begin
      flag ["ocaml";"pp"; use_tag] (A name);
      dep ["ocamldep"; use_tag] [name];
      Log.dprintf 2  "create tag :%s" use_tag;
    end), use_tag)


     
          
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
  let infer_dsource = infer_with_error_channel ["-dsource"] "infer_source"
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

   rule "ocaml: ml & ml.depends  -> .dsource"
     ~prod:"%.dsource" ~deps:["%.ml";"%.ml.depends"]
     (infer_dsource "%.ml" "%.dsource");
   
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
  )
 end 





let ocamlfind x = S[A"ocamlfind"; x]

module Default = struct
  let before_options () = (
    Options.ocamlc := ocamlfind &
      S[A"ocamlc";
        A"-annot";
        A "-w";
        A "+a-4-32-30";
        (* A "-4"; (\* otherwise, a lot of fragile pattern will be detected*\)  *)
        (* A "-bin-annot"; *)
        (* A"-warn-error"; *)
        (* A"A" *)
        (* A" 4-6-7-9-27..29"; *)];
    Options.ocamlopt   :=
      ocamlfind &
      S[A"ocamlopt"; (* A"-annot"; *) A"-w"; A"+a-4-32-30";
        (* A"-unsafe"; *) A"-inline"; A"100"; (* A"-4"; *)
        (* A "-warn-error"; *)
        (* A "A" *)
        (* A"-bin-annot" *)];
    Options.ocamldep   :=
      ocamlfind &
      A"ocamldep";
    Options.ocamldoc   :=
      ocamlfind &
      A"ocamldoc";
    Options.make_links := false; (* no symlink *)
    (* Options.ocamldoc := S [A "ocamldoc"]; *)
    (** ocamlfind does not accept -search
        ocamldoc.opt does not work on mac *)
    Options.ocamlmktop := ocamlfind & A"ocamlmktop")
end 

(**************************************************************)
(**************************************************************)
 

 type actions =  (unit -> unit) list ref
let before_options : actions = ref []
and after_options : actions = ref []
and before_rules : actions = ref []
and after_rules : actions = ref []
let (+>) x l =  l := x :: !l



(** replace of _tags file *)
let tags_table : ((string list * string list) list) ref =  ref [];;

let before_options_dispatch = ref (fun () -> ())
let after_rules_dispatch = ref (fun () -> ())
let apply  before_options_dispatch after_rules_dispatch = (
  Default.before_options +> before_options;
  (* Default.after_rules +> after_rules; *)
  before_options_dispatch +> before_options;
  after_rules_dispatch +> after_rules;
  
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
let root3 = "debug";;
let tmp = "tmp"    

let define_context_for_root r =
  let def = Pathname.define_context in   begin 
    def ("test") ["src"];
    def "testr" ["src"];
    def "llvm" ["src"];
    def "jslib" ["src"];
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
define_context_for_root root3;;
let boot_flags =
  S[P ("boot"//"fan"); (* symlink fan to either fan.byte or fan.native *)
    A"-printer"; A"p"];;

rule "src->tmp: ml -> ml" ~dep: "src/%.ml" ~prod:(tmp//"%.ml")
  (fan  (tmp//"%.ml") "src/%.ml" (tmp//"%.ml"));;

rule "code_boot: mli -> mli" ~dep: "src/%.mli" ~prod:(tmp//"%.mli")
  (fan  (tmp//"%.mli") "src/%.mli" (tmp//"%.mli"));;

rule "code_boot: mlpack -> mlpack" ~dep: "src/%.mlpack" ~prod:(tmp//"%.mlpack")
  (fan  (tmp//"%.mlpack") "src/%.mlpack" (tmp//"%.mlpack"));;

rule "code_boot: mllib -> mllib" ~dep: "src/%.mllib" ~prod:(tmp//"%.mllib")
  (fan  (tmp//"%.mllib") "src/%.mllib" (tmp//"%.mllib"));;

rule "code_boot: mll -> mll" ~dep: "src/%.mll" ~prod:(tmp//"%.mll")
  (fan  (tmp//"%.mll") "src/%.mll" (tmp//"%.mll"));;

let () =
  let ast = "src/fAst.mli" in
  let ast_n = "src/fAstN.ml" in
  let objs = "src/objs.ml" in
  let objs_n = "src/objsN.ml" in
  Options.ocaml_lflags :=  [ "-linkall"] ;
  after_rules_dispatch := fun () -> begin
    flag ["ocaml"; "pp"; "use_fan"] boot_flags;
    (* flag ["ocaml"; "pp"; "use_fan"; "pp:doc"] (S[A"-printer"; A"o"]); *)
    ast_n |-? [ast];
    objs |-? [ast];
    objs_n |-? [ast_n];
    (* "src/fanAst.ml"   |-? [ast]; *)
    "src/fanAstN.ml"  |-? [ast_n; ast];
    "src/astLoc.ml" |-? [ast];
    "src/fanDyn.ml" |-? [ast];
    "src/fanMeta.ml" |-? [ast];
  end;;

copy_rule "src/fan.byte -> boot/fan.byte"
~insert:`top "src/fan.byte" "boot/fan.byte";;
copy_rule "src/fan.native -> boot/fan.native"
~insert:`top "src/fan.native" "boot/fan.native";;



let _ = begin 
  apply !before_options_dispatch !after_rules_dispatch
end


