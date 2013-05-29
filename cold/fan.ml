open FAst

open MkFan

open Format

open LibUtil

let just_print_filters () =
  let pp = eprintf in
  let p_tbl f tbl = Hashtbl.iter (fun k  _v  -> fprintf f "%s@;" k) tbl in
  begin
    pp "@[for interface:@[<hv2>%a@]@]@." p_tbl AstFilters.interf_filters;
    pp "@[for phrase:@[<hv2>%a@]@]@." p_tbl AstFilters.implem_filters;
    pp "@[for top_phrase:@[<hv2>%a@]@]@." p_tbl AstFilters.topphrase_filters
  end

let just_print_parsers () =
  let pp = eprintf in
  let p_tbl f tbl = Hashtbl.iter (fun k  _v  -> fprintf f "%s@;" k) tbl in
  pp "@[Loaded Parsers:@;@[<hv2>%a@]@]@." p_tbl AstParsers.registered_parsers

let just_print_applied_parsers () =
  let pp = eprintf in
  pp "@[Applied Parsers:@;@[<hv2>%a@]@]@."
    (fun f  q  -> Queue.iter (fun (k,_)  -> fprintf f "%s@;" k) q)
    AstParsers.applied_parsers

type file_kind =  
  | Intf of string
  | Impl of string
  | Str of string
  | ModuleImpl of string
  | IncludeDir of string 

let print_loaded_modules = ref false

let loaded_modules = ref SSet.empty

let add_to_loaded_modules name =
  loaded_modules := (SSet.add name loaded_modules.contents)

let (objext,libext) =
  if Dynlink.is_native then (".cmxs", ".cmxs") else (".cmo", ".cma")

let require name =
  if not (SSet.mem name loaded_modules.contents)
  then begin add_to_loaded_modules name; DynLoader.load (name ^ libext) end

let output_file = ref None

let rec sig_handler: sigi -> sigi option =
  function
  | (`Directive (_loc,`Lid (_,"load"),`Str (_,s)) : FAst.sigi) ->
      begin require s; None end
  | (`Directive (_loc,`Lid (_,"use"),`Str (_,s)) : FAst.sigi) ->
      PreCast.parse_file ~directive_handler:sig_handler s
        PreCast.parse_interf
  | (`Directive (_loc,`Lid (_,"default_quotation"),`Str (_,s)) : FAst.sigi)
      ->
      begin
        AstQuotation.default := (FToken.resolve_name ((`Sub []), s)); None
      end
  | (`Directive (_loc,`Lid (_,"filter"),`Str (_,s)) : FAst.sigi) ->
      begin AstFilters.use_interf_filter s; None end
  | `DirectiveSimple (_loc,`Lid (_,"import")) -> None
  | (`Directive (_loc,`Lid (_,x),_) : FAst.sigi) ->
      FLoc.raise _loc
        (XStream.Error (x ^ " is abad directive Fan can not handled "))
  | _ -> None

let rec str_handler =
  function
  | (`Directive (_loc,`Lid (_,"load"),`Str (_,s)) : FAst.stru) ->
      begin require s; None end
  | (`Directive (_loc,`Lid (_,"use"),`Str (_,s)) : FAst.stru) ->
      PreCast.parse_file ~directive_handler:str_handler s
        PreCast.parse_implem
  | (`Directive (_loc,`Lid (_,"default_quotation"),`Str (_,s)) : FAst.stru)
      ->
      begin
        AstQuotation.default := (FToken.resolve_name ((`Sub []), s)); None
      end
  | (`DirectiveSimple (_loc,`Lid (_,"lang_clear")) : FAst.stru) ->
      begin
        AstQuotation.clear_map (); AstQuotation.clear_default (); None
      end
  | (`Directive (_loc,`Lid (_,"filter"),`Str (_,s)) : FAst.stru) ->
      begin AstFilters.use_implem_filter s; None end
  | `DirectiveSimple (_loc,`Lid (_,"import")) -> None
  | (`Directive (_loc,`Lid (_,x),_) : FAst.stru) ->
      FLoc.raise _loc
        (XStream.Error (x ^ "bad directive Fan can not handled "))
  | _ -> None

let process_intf name =
  let v =
    match PreCast.parse_file ~directive_handler:sig_handler name
            PreCast.parse_interf
    with
    | None  -> None
    | Some x -> let x = AstFilters.apply_interf_filters x in Some x in
  PreCast.CurrentPrinter.print_interf ?input_file:(Some name)
    ?output_file:(output_file.contents) v

let process_impl name =
  let v =
    match PreCast.parse_file ~directive_handler:str_handler name
            PreCast.parse_implem
    with
    | None  -> None
    | Some x -> let x = AstFilters.apply_implem_filters x in Some x in
  PreCast.CurrentPrinter.print_implem ?input_file:(Some name)
    ?output_file:(output_file.contents) v

let input_file x =
  match x with
  | Intf file_name ->
      begin
        FConfig.compilation_unit :=
          (Some
             (String.capitalize
                (let open Filename in chop_extension (basename file_name))));
        FConfig.current_input_file := file_name; process_intf file_name
      end
  | Impl file_name ->
      begin
        FConfig.compilation_unit :=
          (Some
             (String.capitalize
                (let open Filename in chop_extension (basename file_name))));
        FConfig.current_input_file := file_name; process_impl file_name
      end
  | Str s ->
      let (f,o) = Filename.open_temp_file "from_string" ".ml" in
      begin
        output_string o s; close_out o; FConfig.current_input_file := f;
        process_impl f; at_exit (fun ()  -> Sys.remove f)
      end
  | ModuleImpl file_name -> require file_name
  | IncludeDir dir -> Ref.modify FConfig.dynload_dirs (cons dir)

let initial_spec_list =
  [("-I", (FArg.String ((fun x  -> input_file (IncludeDir x)))),
     "<directory>  Add directory in search patch for object files.");
  ("-intf", (FArg.String ((fun x  -> input_file (Intf x)))),
    "<file>  Parse <file> as an interface, whatever its extension.");
  ("-impl", (FArg.String ((fun x  -> input_file (Impl x)))),
    "<file>  Parse <file> as an implementation, whatever its extension.");
  ("-str", (FArg.String ((fun x  -> input_file (Str x)))),
    "<string>  Parse <string> as an implementation.");
  ("-o", (FArg.String ((fun x  -> output_file := (Some x)))),
    "<file> Output on <file> instead of standard output.");
  ("-unsafe", (FArg.Set FConfig.unsafe),
    "Generate unsafe accesses to array and strings.");
  ("-verbose", (FArg.Set FConfig.verbose), "More verbose in parsing errors.");
  ("-where",
    (FArg.Unit
       ((fun ()  ->
           begin print_endline FConfig.fan_standard_library; exit 0 end))),
    " Print location of standard library and exit");
  ("-loc", (FArg.Set_string FLoc.name),
    ("<name>   Name of the location variable (default: " ^
       (FLoc.name.contents ^ ").")));
  ("-v",
    (FArg.Unit
       ((fun ()  ->
           begin eprintf "Fan version %s@." FConfig.version; exit 0 end))),
    "Print Fan version and exit.");
  ("-compilation-unit",
    (FArg.Unit
       ((fun ()  ->
           begin
             (match FConfig.compilation_unit.contents with
              | Some v -> printf "%s@." v
              | None  -> printf "null");
             exit 0
           end))), "Print the current compilation unit");
  ("-plugin", (FArg.String require), "load plugin cma or cmxs files");
  ("-loaded-modules", (FArg.Set print_loaded_modules),
    "Print the list of loaded modules.");
  ("-loaded-filters", (FArg.Unit just_print_filters),
    "Print the registered filters.");
  ("-loaded-parsers", (FArg.Unit just_print_parsers),
    "Print the loaded parsers.");
  ("-used-parsers", (FArg.Unit just_print_applied_parsers),
    "Print the applied parsers.");
  ("-printer",
    (FArg.Symbol
       (["p"; "o"],
         ((fun x  ->
             if x = "p"
             then PreCast.register_bin_printer ()
             else PreCast.register_text_printer ())))),
    "p  for binary and o  for text ")]

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
           else raise (FArg.Bad ("don't know what to do with " ^ name)))

let _ = PreCast.register_text_printer ()

let _ =
  Printexc.register_printer
    (function
     | FLoc.Exc_located (loc,exn) ->
         Some
           (sprintf "%s:@\n%s" (FLoc.to_string loc) (Printexc.to_string exn))
     | _ -> None)

let _ =
  begin
    Fsyntax.Options.add
      ("-dlang",
        (FArg.String
           (fun s  ->
              AstQuotation.default := (FToken.resolve_name ((`Sub []), s)))),
        " Set the default language");
    Fsyntax.Options.adds initial_spec_list
  end

let _ = AstParsers.use_parsers ["revise"; "stream"; "macro"]

let _ =
  try
    FArg.parse Fsyntax.Options.init_spec_list anon_fun
      "fan <options> <file>\nOptions are:\n"
  with
  | exc -> begin eprintf "@[<v0>%s@]@." (Printexc.to_string exc); exit 2 end