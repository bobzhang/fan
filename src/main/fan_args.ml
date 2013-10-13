open Util
open Format
    
let just_print_filters () =
  let pp = eprintf  in 
  let p_tbl f tbl = Hashtbl.iter (fun k _v -> fprintf f "%s@;" k) tbl in
  begin
    pp  "@[for interface:@[<hv2>%a@]@]@." p_tbl Ast_filters.interf_filters ;
    pp  "@[for phrase:@[<hv2>%a@]@]@." p_tbl Ast_filters.implem_filters ;
    pp  "@[for top_phrase:@[<hv2>%a@]@]@." p_tbl Ast_filters.topphrase_filters 
  end
let just_print_parsers () =
  let pp = eprintf in
  let p_tbl f tbl = Hashtbl.iter (fun k _v -> fprintf f "%s@;" k) tbl in begin
    pp "@[Loaded Parsers:@;@[<hv2>%a@]@]@." p_tbl Ast_parsers.registered_parsers
  end
  
let just_print_applied_parsers () =
  let pp = eprintf in
  pp "@[Applied Parsers:@;@[<hv2>%a@]@]@."
    (fun f q -> Queue.iter (fun (k,_) -> fprintf f "%s@;" k) q  ) Ast_parsers.applied_parsers
  
type file_kind =
  | Intf of string
  | Impl of string
  | Str of string
  | ModuleImpl of string
  | IncludeDir of string 
  

    
let print_loaded_modules = ref false






let output_file = ref None              


(** parse the file, apply the filter and pipe it to the backend *)  
let process_intf  name =
  let v = 
  match PreCast.parse_file name PreCast.parse_interf with
  | None ->
        None
  | Some x ->
      let x = Ast_filters.apply_interf_filters x in
      Some x  in
  PreCast.CurrentPrinter.print_interf
    ?input_file:(Some name)
    ?output_file:(!output_file) v 


let process_impl name =
  let v = 
  match PreCast.parse_file name PreCast.parse_implem with
  | None -> None
  | Some x ->
      let x = Ast_filters.apply_implem_filters x in
      Some x  in
  PreCast.CurrentPrinter.print_implem
    ~input_file:name
    ?output_file:(!output_file) v 

          
      
let input_file x =
  match x with
  | Intf file_name ->
      begin
        if file_name <> "-" then 
          FConfig.compilation_unit :=
            Some (String.capitalize (Filename.(chop_extension (basename file_name))));
        FConfig.current_input_file := file_name;
        process_intf  file_name
      end
  | Impl file_name ->
      begin
        if file_name <> "-" then 
          FConfig.compilation_unit :=
            Some (String.capitalize (Filename.(chop_extension (basename file_name))));
        FConfig.current_input_file := file_name;
        process_impl  file_name;
      end
  | Str s ->
      let (f, o) = Filename.open_temp_file "from_string" ".ml" in
      (output_string o s;
       close_out o;
       FConfig.current_input_file := f;
       process_impl  f;
       at_exit (fun () -> Sys.remove f))
        
  | ModuleImpl file_name -> Control_require.add  file_name

  | IncludeDir dir ->
      Ref.modify FConfig.dynload_dirs (cons dir) 


(** FIXME the command line parsing  can not handle prefix problem,
    e.g. -p -px will cause some problem *)    
let initial_spec_list =
  [
   ("-I", Arg.String (fun x -> input_file (IncludeDir x)),
    "<directory>  Add directory in search patch for object files.");

   ("-intf", Arg.String (fun x -> input_file (Intf x)),
    "<file>  Parse <file> as an interface, whatever its extension.");

   ("-impl", Arg.String (fun x -> input_file (Impl x)),
    "<file>  Parse <file> as an implementation, whatever its extension.");

   ("-str", Arg.String (fun x -> input_file (Str x)),
    "<string>  Parse <string> as an implementation.");

   ("-o", Arg.String (fun x -> output_file := Some x),
    "<file> Output on <file> instead of standard output.");

   ("-unsafe", Arg.Set FConfig.unsafe,
    "Generate unsafe accesses to array and strings.");

   (* ("-verbose", Arg.Set FConfig.verbose, "More verbose in parsing errors."); *)

   ("-where", Arg.Unit (fun () -> (print_endline FConfig.fan_plugins_library;exit 0))
      , " Print location of standard library and exit");
   ("-loc", Arg.Set_string FLoc.name,
    "<name>   Name of the location variable (default: " ^ !FLoc.name ^ ").");
   
   ("-v", Arg.Unit  (fun () -> begin eprintf "Fan version %s@." FConfig.version; exit 0 end),
    "Print Fan version and exit.");

   ("-compilation-unit",
    Arg.Unit (function () -> 
      ((match !FConfig.compilation_unit with
      | Some v -> printf "%s@." v
      | None -> printf "null");
       exit 0)), 
    "Print the current compilation unit");

   ("-plugin", Arg.String Control_require.add , "load plugin cma or cmxs files");

   ("-loaded-modules", Arg.Set print_loaded_modules, "Print the list of loaded modules.");
   
   ("-loaded-filters", Arg.Unit just_print_filters, "Print the registered filters.");
   
   ("-loaded-parsers", Arg.Unit just_print_parsers, "Print the loaded parsers.");
   
   ("-used-parsers", Arg.Unit just_print_applied_parsers, "Print the applied parsers.");

   ("-dlang",
       (Arg.String
          (fun s  ->
            Ast_quotation.default :=
              (Ast_quotation.resolve_name (`Sub [], s)))),
       " Set the default language");
   ("-printer",
    Arg.Symbol( ["p";"o";"dparsetree"],
    function x ->
      if x = "o" then
        PreCast.register_text_printer ()
      else if x = "p" then
        PreCast.register_bin_printer ()
      else PreCast.register_parsetree_printer ()),
    "choose different backends according to the option");
 ];;
      


(** handle the file name *)  
let anon_fun name =
  let check = Filename.check_suffix name in
  input_file
  (
   (* if name = "-" then *)
   if check ".mli" then Intf name
  else if check ".ml" then Impl name
  else if check Dyn_load.objext then ModuleImpl name
  else if check Dyn_load.libext then ModuleImpl name
  else raise (Arg.Bad ("don't know what to do with " ^ name)));;







(* local variables: *)
(* compile-command: "cd .. && pmake main_annot/fan_args.cmo" *)
(* end: *)
