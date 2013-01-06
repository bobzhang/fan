open Format
open LibUtil
let just_print_the_version () = printf "%s@." FanConfig.version; exit 0
let just_print_compilation_unit () =
  (match FanConfig.compilation_unit.contents with
   | Some v -> printf "%s@." v
   | None  -> printf "null");
  exit 0
let print_version () =
  eprintf "Camlp4 version %s@." FanConfig.version; exit 0
let warn_noassert () =
  eprintf
    "camlp4 warning: option -noassert is obsolete\nYou should give the -noassert option to the ocaml compiler instead.@."
let just_print_filters () =
  let pp = eprintf in
  let p_tbl f tbl = Hashtbl.iter (fun k  _v  -> fprintf f "%s@;" k) tbl in
  pp "@[for interface:@[<hv2>%a@]@]@." p_tbl AstFilters.interf_filters;
  pp "@[for phrase:@[<hv2>%a@]@]@." p_tbl AstFilters.implem_filters;
  pp "@[for top_phrase:@[<hv2>%a@]@]@." p_tbl AstFilters.topphrase_filters
let just_print_parsers () =
  let pp = eprintf in
  let p_tbl f tbl = Hashtbl.iter (fun k  _v  -> fprintf f "%s@;" k) tbl in
  pp "@[Loaded Parsers:@;@[<hv2>%a@]@]@." p_tbl AstParsers.registered_parsers
let just_print_applied_parsers () =
  let pp = eprintf in
  pp "@[Applied Parsers:@;@[<hv2>%a@]@]@."
    (fun f  q  -> Queue.iter (fun (k,_)  -> fprintf f "%s@;" k) q)
    AstParsers.applied_parsers
let _ =
  AstParsers.use_parsers ["revise"; "stream"; "macro"; "ListComprehension"]
type file_kind =  
  | Intf of string
  | Impl of string
  | Str of string
  | ModuleImpl of string
  | IncludeDir of string 
let search_stdlib = ref true
let print_loaded_modules = ref false
let task f x = let () = FanConfig.current_input_file := x in f x
module Camlp4Bin(PreCast:Sig.PRECAST) =
  struct
  let printers: (string,(module Sig.PRECAST_PLUGIN)) Hashtbl.t =
    Hashtbl.create 30 let rcall_callback = ref (fun ()  -> ())
  let loaded_modules = ref SSet.empty
  let add_to_loaded_modules name =
    loaded_modules := (SSet.add name loaded_modules.contents)
  let _ =
    Printexc.register_printer
      (function
       | FanLoc.Exc_located (loc,exn) ->
           Some
             (sprintf "%s:@\n%s" (FanLoc.to_string loc)
                (Printexc.to_string exn))
       | _ -> None) module DynLoader = DynLoader.Make(struct
                      
                      end)
  let (objext,libext) =
    if DynLoader.is_native then (".cmxs", ".cmxs") else (".cmo", ".cma")
  let rewrite_and_load n x =
    let dyn_loader = DynLoader.instance.contents () in
    let find_in_path = DynLoader.find_in_path dyn_loader in
    let real_load name =
      add_to_loaded_modules name; DynLoader.load dyn_loader name in
    let load n =
      if
        (SSet.mem n loaded_modules.contents) ||
          (List.mem n PreCast.loaded_modules.contents)
      then ()
      else (add_to_loaded_modules n; DynLoader.load dyn_loader (n ^ objext)) in
    (match (n, (String.lowercase x)) with
     | (("Printers"|""),("pr_o.cmo"|"o"|"ocaml"|"camlp4ocamlprinter.cmo")) ->
         PreCast.enable_ocaml_printer ()
     | (("Printers"|""),("pr_dump.cmo"|"p"|"dumpocaml"|"camlp4ocamlastdumper.cmo"))
         -> PreCast.enable_dump_ocaml_ast_printer ()
     | (("Printers"|""),("d"|"dumpcamlp4"|"camlp4astdumper.cmo")) ->
         PreCast.enable_dump_camlp4_ast_printer ()
     | (("Printers"|""),("a"|"auto"|"camlp4autoprinter.cmo")) ->
         (load "Camlp4Autoprinter";
          (let (module P)  = Hashtbl.find printers "camlp4autoprinter" in
           P.apply (module PreCast)))
     | _ ->
         let y = "Camlp4" ^ (n ^ ("/" ^ (x ^ objext))) in
         real_load (try find_in_path y with | Not_found  -> x));
    rcall_callback.contents ()
  let print_warning = eprintf "%a:\n%s@." FanLoc.print
  let output_file = ref None
  let parse_file ?directive_handler  name pa =
    let loc = FanLoc.mk name in
    PreCast.Syntax.current_warning := print_warning;
    (let ic = if name = "-" then stdin else open_in_bin name in
     let cs = XStream.of_channel ic in
     let clear () = if name = "-" then () else close_in ic in
     let phr =
       try pa ?directive_handler loc cs with | x -> (clear (); raise x) in
     clear (); phr)
  let rec sig_handler =
    function
    | `SgDir (_loc,"load",`Str (_,s)) -> (rewrite_and_load "" s; None)
    | `SgDir (_loc,"directory",`Str (_,s)) ->
        (DynLoader.include_dir (DynLoader.instance.contents ()) s; None)
    | `SgDir (_loc,"use",`Str (_,s)) ->
        Some
          (parse_file ~directive_handler:sig_handler s
             PreCast.CurrentParser.parse_interf)
    | `SgDir (_loc,"default_quotation",`Str (_,s)) ->
        (AstQuotation.default := s; None)
    | `SgDir (_loc,"filter",`Str (_,s)) ->
        (AstFilters.use_interf_filter s; None)
    | `SgDir (loc,x,_) ->
        FanLoc.raise loc
          (XStream.Error (x ^ " is abad directive camlp4 can not handled "))
    | _ -> assert false
  let rec str_handler =
    function
    | `StDir (_loc,"load",`Str (_,s)) -> (rewrite_and_load "" s; None)
    | `StDir (_loc,"directory",`Str (_,s)) ->
        (DynLoader.include_dir (DynLoader.instance.contents ()) s; None)
    | `StDir (_loc,"use",`Str (_,s)) ->
        Some
          (parse_file ~directive_handler:str_handler s
             PreCast.CurrentParser.parse_implem)
    | `StDir (_loc,"default_quotation",`Str (_,s)) ->
        (AstQuotation.default := s; None)
    | `StDir (_loc,"lang_at",`ExApp (_,`Str (_,tag),`Str (_,quot))) ->
        (AstQuotation.default_at_pos tag quot; None)
    | `StDir (_loc,"lang_clear",`ExNil _) ->
        (AstQuotation.clear_map (); AstQuotation.clear_default (); None)
    | `StDir (_loc,"filter",`Str (_,s)) ->
        (AstFilters.use_implem_filter s; None)
    | `StDir (loc,x,_) ->
        FanLoc.raise loc
          (XStream.Error (x ^ "bad directive camlp4 can not handled "))
    | _ -> assert false
  let process ?directive_handler  name pa pr clean fold_filters =
    (((parse_file ?directive_handler name pa) |> fold_filters) |> clean) |>
      (pr ?input_file:(Some name) ?output_file:(output_file.contents))
  let process_intf name =
    process ~directive_handler:sig_handler name
      PreCast.CurrentParser.parse_interf PreCast.CurrentPrinter.print_interf
      (new FanAst.clean_ast)#sig_item AstFilters.apply_interf_filters
  let process_impl name =
    process ~directive_handler:str_handler name
      PreCast.CurrentParser.parse_implem PreCast.CurrentPrinter.print_implem
      (new FanAst.clean_ast)#str_item AstFilters.apply_implem_filters
  let input_file x =
    let dyn_loader = DynLoader.instance.contents () in
    rcall_callback.contents ();
    (match x with
     | Intf file_name ->
         (FanConfig.compilation_unit :=
            (Some
               (String.capitalize
                  (let open Filename in chop_extension (basename file_name))));
          task process_intf file_name)
     | Impl file_name ->
         (FanConfig.compilation_unit :=
            (Some
               (String.capitalize
                  (let open Filename in chop_extension (basename file_name))));
          task process_impl file_name)
     | Str s ->
         let (f,o) = Filename.open_temp_file "from_string" ".ml" in
         (output_string o s;
          close_out o;
          task process_impl f;
          at_exit (fun ()  -> Sys.remove f))
     | ModuleImpl file_name -> rewrite_and_load "" file_name
     | IncludeDir dir -> DynLoader.include_dir dyn_loader dir);
    rcall_callback.contents ()
  let initial_spec_list =
    [("-I", (FanArg.String ((fun x  -> input_file (IncludeDir x)))),
       "<directory>  Add directory in search patch for object files.");
    ("-nolib", (FanArg.Clear search_stdlib),
      "No automatic search for object files in library directory.");
    ("-intf", (FanArg.String ((fun x  -> input_file (Intf x)))),
      "<file>  Parse <file> as an interface, whatever its extension.");
    ("-impl", (FanArg.String ((fun x  -> input_file (Impl x)))),
      "<file>  Parse <file> as an implementation, whatever its extension.");
    ("-str", (FanArg.String ((fun x  -> input_file (Str x)))),
      "<string>  Parse <string> as an implementation.");
    ("-unsafe", (FanArg.Set FanConfig.unsafe),
      "Generate unsafe accesses to array and strings.");
    ("-noassert", (FanArg.Unit warn_noassert),
      "Obsolete, do not use this option.");
    ("-verbose", (FanArg.Set FanConfig.verbose),
      "More verbose in parsing errors.");
    ("-loc", (FanArg.Set_string FanLoc.name),
      ("<name>   Name of the location variable (default: " ^
         (FanLoc.name.contents ^ ").")));
    ("-QD", (FanArg.String ((fun x  -> AstQuotation.dump_file := (Some x)))),
      "<file> Dump quotation expander result in case of syntax error.");
    ("-o", (FanArg.String ((fun x  -> output_file := (Some x)))),
      "<file> Output on <file> instead of standard output.");
    ("-v", (FanArg.Unit print_version), "Print Camlp4 version and exit.");
    ("-version", (FanArg.Unit just_print_the_version),
      "Print Camlp4 version number and exit.");
    ("-compilation-unit", (FanArg.Unit just_print_compilation_unit),
      "Print the current compilation unit");
    ("-vnum", (FanArg.Unit just_print_the_version),
      "Print Camlp4 version number and exit.");
    ("-no_quot", (FanArg.Clear FanConfig.quotations),
      "Don't parse quotations, allowing to use, e.g. \"<:>\" as token.");
    ("-loaded-modules", (FanArg.Set print_loaded_modules),
      "Print the list of loaded modules.");
    ("-loaded-filters", (FanArg.Unit just_print_filters),
      "Print the registered filters.");
    ("-loaded-parsers", (FanArg.Unit just_print_parsers),
      "Print the loaded parsers.");
    ("-used-parsers", (FanArg.Unit just_print_applied_parsers),
      "Print the applied parsers.");
    ("-parser", (FanArg.String (rewrite_and_load "Parsers")),
      "<name>  Load the parser FanParsers/<name>.cm(o|a|xs)");
    ("-printer", (FanArg.String (rewrite_and_load "Printers")),
      "<name>  Load the printer Camlp4Printers/<name>.cm(o|a|xs)");
    ("-ignore", (FanArg.String ignore), "ignore the next argument");
    ("--", (FanArg.Unit ignore), "Deprecated, does nothing")]
  let _ = PreCast.Syntax.Options.adds initial_spec_list
  let anon_fun name =
    input_file
      (if Filename.check_suffix name ".mli"
       then Intf name
       else
         if Filename.check_suffix name ".ml"
         then Impl name
         else
           if Filename.check_suffix name objext
           then ModuleImpl name
           else
             if Filename.check_suffix name libext
             then ModuleImpl name
             else raise (FanArg.Bad ("don't know what to do with " ^ name)))
  let main () =
    try
      let dynloader =
        DynLoader.mk ~ocaml_stdlib:(search_stdlib.contents)
          ~camlp4_stdlib:(search_stdlib.contents) () in
      DynLoader.instance := ((fun ()  -> dynloader));
      (let call_callback () =
         PreCast.iter_and_take_callbacks
           (fun (name,module_callback)  ->
              let () = add_to_loaded_modules name in module_callback ()) in
       call_callback ();
       rcall_callback := call_callback;
       FanArg.parse PreCast.Syntax.Options.init_spec_list anon_fun
         "fan <options> <file>\nOptions are:\n";
       call_callback ();
       if print_loaded_modules.contents
       then SSet.iter (eprintf "%s@.") loaded_modules.contents
       else ())
    with | exc -> (eprintf "@[<v0>%s@]@." (Printexc.to_string exc); exit 2)
  let _ = main ()
  end