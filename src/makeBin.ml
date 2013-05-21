open Ast

open Format
open LibUtil

let (objext,libext) =
  if Dynlink.is_native then
    (".cmxs",".cmxs")
  else (".cmo",".cma")

(*be careful, since you can register your own [stru_parser],
  if you do it in-consistently, this may result in an
  in-consistent behavior *)  
    
let just_print_filters () =
  let pp = eprintf (* and f = Format.std_formatter *) in 
  let p_tbl f tbl = Hashtbl.iter (fun k _v -> fprintf f "%s@;" k) tbl in
  begin
    pp  "@[for interface:@[<hv2>%a@]@]@." p_tbl AstFilters.interf_filters ;
    pp  "@[for phrase:@[<hv2>%a@]@]@." p_tbl AstFilters.implem_filters ;
    pp  "@[for top_phrase:@[<hv2>%a@]@]@." p_tbl AstFilters.topphrase_filters 
  end


    
let just_print_parsers () =
  let pp = eprintf in
  let p_tbl f tbl = Hashtbl.iter (fun k _v -> fprintf f "%s@;" k) tbl in begin
    pp "@[Loaded Parsers:@;@[<hv2>%a@]@]@." p_tbl AstParsers.registered_parsers
  end
  
let just_print_applied_parsers () =
  let pp = eprintf in
  pp "@[Applied Parsers:@;@[<hv2>%a@]@]@."
    (fun f q -> Queue.iter (fun (k,_) -> fprintf f "%s@;" k) q  ) AstParsers.applied_parsers




  
  
type file_kind =
  | Intf of string
  | Impl of string
  | Str of string
  | ModuleImpl of string
  | IncludeDir of string 
  
let search_stdlib = ref false
    
let print_loaded_modules = ref false

let task f x =
  let () = FanConfig.current_input_file := x in
  f x 


    
(* let printers : (string, (module Sig.PRECAST_PLUGIN)) Hashtbl.t   = Hashtbl.create 30 *)
(* let dyn_loader = ref (fun () -> failwith "empty in dynloader"); *)

(* let dynloader = DynLoader.mk ~ocaml_stdlib:!search_stdlib ()  *)
    
let rcall_callback = ref (fun () -> ())
let loaded_modules = ref SSet.empty

let add_to_loaded_modules name =
  loaded_modules := SSet.add name !loaded_modules;;

(* no repeat loading *)
let real_load name = 
  if not (SSet.mem name !loaded_modules ) then begin
    add_to_loaded_modules name;
    DynLoader.load  name
  end
;;  
        
Printexc.register_printer
        (function
          |FanLoc.Exc_located (loc, exn) ->
              Some (sprintf "%s:@\n%s" (FanLoc.to_string loc) (Printexc.to_string exn))
          | _ -> None );;
      
let rewrite_and_load  x =
  let y = x ^ objext in
  begin
    real_load y;
    !rcall_callback ();
  end

    
let print_warning = eprintf "%a:\n%s@." FanLoc.print

let output_file = ref None              
let parse_file  ?directive_handler name pa = begin 
  
  let loc = FanLoc.mk name in
  let  () = Syntax.current_warning := print_warning in
  let ic = if name = "-" then stdin else open_in_bin name in
  let cs = XStream.of_channel ic in
  let clear () = if name = "-" then () else close_in ic in
  let phr =
    try pa ?directive_handler loc cs
    with x -> begin  clear (); raise x end in
  let () = clear () in
  phr
end

let  rec sig_handler  : sigi -> sigi option =  with sigi
    (function
      | {| #load $str:s |}-> begin rewrite_and_load  s; None end

      (* | {| #directory $str:s |} -> *)
      (*     begin DynLoader.include_dir (!DynLoader.instance ()) s ; None end *)
            
      | {| #use $str:s|} ->
          (parse_file
             ~directive_handler:sig_handler s PreCast.CurrentParser.parse_interf )
      | {| #default_quotation $str:s |} ->
          (AstQuotation.default :=
            FanToken.resolve_name (`Sub [], s); None )
      | {| #$({:ident'@_|filter|}) $str:s |} -> (* FIXME simplify later*)
          ( AstFilters.use_interf_filter s; None)
      | (* {|#import|} *) `DirectiveSimple(_loc,`Lid(_,"import")) -> None
      | {| #$lid:x $_|} -> (* FIXME pattern match should give _loc automatically *)
          FanLoc.raise _loc
            (XStream.Error (x ^ " is abad directive Fan can not handled "))
      | _ -> None (* FIXME assert false *))

           
let rec str_handler = with stru
    (function
      | {| #load $str:s |} -> begin rewrite_and_load  s; None end

      (* | {| #directory $str:s |} -> *)
      (*     begin DynLoader.include_dir (!DynLoader.instance ()) s ; None end *)
            
      | {| #use $str:s |} ->
          (* Some  *)(parse_file  ~directive_handler:str_handler s
                        PreCast.CurrentParser.parse_implem )
      | {| #default_quotation $str:s |} ->
          begin AstQuotation.default :=
            FanToken.resolve_name (`Sub [],s) ;
            None end
      | {| #lang_clear |} -> begin 
          AstQuotation.clear_map ();
          AstQuotation.clear_default ();
          None
      end
      | {| #filter $str:s|} ->
          begin AstFilters.use_implem_filter s; None ; end
      | (* {|#import|} *) `DirectiveSimple(_loc,`Lid(_,"import")) -> None                  
            (* | {| #import |} -> None (\* FIXME *\) *)
      | {| #$lid:x $_ |} ->
          (* FIXME pattern match should give _loc automatically *)
          FanLoc.raise _loc (XStream.Error (x ^ "bad directive Fan can not handled "))
      | _ -> None (* ignored  assert false *))

           
let process  ?directive_handler name pa pr clean fold_filters =
  match parse_file  ?directive_handler name pa with
  |None ->
      pr ?input_file:(Some name) ?output_file:!output_file None 
  |Some x ->
      Some (clean (fold_filters x)) |>
      pr ?input_file:(Some name) ?output_file:!output_file
      

(** [entrance] *)  
let process_intf  name =
  process ~directive_handler:sig_handler
    name PreCast.CurrentParser.parse_interf PreCast.CurrentPrinter.print_interf
    (* (new Objs.clean_ast)#sigi *) (fun x -> x)
    AstFilters.apply_interf_filters

          
let process_impl  name =
  process ~directive_handler:str_handler
    name
    PreCast.CurrentParser.parse_implem
    PreCast.CurrentPrinter.print_implem
    (fun x -> x)
    AstFilters.apply_implem_filters
    (* gimd *)

      
let input_file x =
  (* let dyn_loader = !DynLoader.instance () in  *)
  begin
    !rcall_callback ();
    (match x with
    | Intf file_name -> begin
        FanConfig.compilation_unit :=
          Some (String.capitalize (Filename.(chop_extension (basename file_name))));
        task process_intf  file_name
    end
    | Impl file_name -> begin
              FanConfig.compilation_unit :=
                Some (String.capitalize (Filename.(chop_extension (basename file_name))));
        task process_impl  file_name;
    end
    | Str s ->
        let (f, o) = Filename.open_temp_file "from_string" ".ml" in
        (output_string o s;
         close_out o;
         task process_impl  f;
         at_exit (fun () -> Sys.remove f))
          
    | ModuleImpl file_name -> rewrite_and_load  file_name
    | IncludeDir dir ->
        Ref.modify FanConfig.dynload_dirs (cons dir) );
    !rcall_callback ();
  end

(** FIXME the command line parsing  can not handle prefix problem,
    e.g. -p -px will cause some problem *)    
let initial_spec_list =
  [
   ("-I", FanArg.String (fun x -> input_file (IncludeDir x)),
    "<directory>  Add directory in search patch for object files.");

   ("-nostdlib", FanArg.Clear search_stdlib, "No automatic search for object files in library directory.");

   ("-intf", FanArg.String (fun x -> input_file (Intf x)),
    "<file>  Parse <file> as an interface, whatever its extension.");

   ("-impl", FanArg.String (fun x -> input_file (Impl x)),
    "<file>  Parse <file> as an implementation, whatever its extension.");

   ("-str", FanArg.String (fun x -> input_file (Str x)),
    "<string>  Parse <string> as an implementation.");

   ("-o", FanArg.String (fun x -> output_file := Some x),
    "<file> Output on <file> instead of standard output.");

   ("-unsafe", FanArg.Set FanConfig.unsafe,
    "Generate unsafe accesses to array and strings.");

   ("-verbose", FanArg.Set FanConfig.verbose, "More verbose in parsing errors.");

   ("-loc", FanArg.Set_string FanLoc.name, "<name>   Name of the location variable (default: " ^ !FanLoc.name ^ ").");

   ("-QD", FanArg.String (fun x -> AstQuotation.dump_file := Some x),
    "<file> Dump quotation expander result in case of syntax error.");
   ("-v", FanArg.Unit  (fun () -> begin eprintf "Fan version %s@." FanConfig.version; exit 0 end),
    "Print Fan version and exit.");

   ("-compilation-unit",
    FanArg.Unit (function () -> 
      ((match !FanConfig.compilation_unit with
      | Some v -> printf "%s@." v
      | None -> printf "null");
       exit 0)), 
    "Print the current compilation unit");

   ("-loaded-modules", FanArg.Set print_loaded_modules, "Print the list of loaded modules.");
   
   ("-loaded-filters", FanArg.Unit just_print_filters, "Print the registered filters.");
   
   ("-loaded-parsers", FanArg.Unit just_print_parsers, "Print the loaded parsers.");
   
   ("-used-parsers", FanArg.Unit just_print_applied_parsers, "Print the applied parsers.");
   
   ("-parser", FanArg.String rewrite_and_load ,
    
    "<name>  Load the parser Gparsers/<name>.cm(o|a|xs)");

   ("-printer", FanArg.Symbol( ["p";"o"],
    function x ->
      if x = "p" then
        PreCast.register_bin_printer ()
      else
        PreCast.register_text_printer ()),"p  for binary and o  for text ");
   ("-ignore", FanArg.String ignore, "ignore the next argument");
  
 ];;
      
let () = Syntax.Options.adds initial_spec_list;;

(** handle the file name *)  
let anon_fun name =
  input_file
    (if Filename.check_suffix name ".mli" then Intf name
    else if Filename.check_suffix name ".ml" then Impl name
    else if Filename.check_suffix name objext then ModuleImpl name
    else if Filename.check_suffix name libext then ModuleImpl name
    else raise (FanArg.Bad ("don't know what to do with " ^ name)))
    
let main () =
  try
    let call_callback () =
      PreCast.iter_and_take_callbacks
        (fun (name, module_callback) ->
           (add_to_loaded_modules name; module_callback ())) in
    let () = call_callback () in
    let () = rcall_callback := call_callback in
    let () =
      FanArg.parse
        Syntax.Options.init_spec_list
        anon_fun "fan <options> <file>\nOptions are:\n" in
     call_callback () 

  with exc -> begin eprintf "@[<v0>%s@]@." (Printexc.to_string exc); exit 2 end;;




