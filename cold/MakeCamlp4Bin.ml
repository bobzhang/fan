open Format

open Camlp4

open Camlp4Parsers

open Camlp4Filters

open FanUtil


module Camlp4Bin =
 functor (Loc : FanSig.Loc) ->
  functor (PreCast : (Sig.PRECAST with module Loc = Loc)) ->
   struct
    open PreCast

    module CleanAst = (Struct.CleanAst.Make)(PreCast.Ast)

    let printers =
     ((Hashtbl.create 30) : (string, (module Sig.PRECAST_PLUGIN )) Hashtbl.t)

    let dyn_loader = (ref ( fun ()  -> (failwith "empty in dynloader") ))

    let rcall_callback = (ref ( fun ()  -> () ))

    let loaded_modules = (ref SSet.empty)

    let add_to_loaded_modules =
     fun name -> (loaded_modules := ( (SSet.add name ( !loaded_modules )) ))

    let (objext, libext) =
     if DynLoader.is_native then (".cmxs", ".cmxs") else (".cmo", ".cma")

    let rewrite_and_load =
     fun n ->
      fun x ->
       let dyn_loader = ((!dyn_loader) () ) in
       let find_in_path = (DynLoader.find_in_path dyn_loader) in
       let real_load =
        fun name ->
         ( (add_to_loaded_modules name) ); (DynLoader.load dyn_loader name) in
       let load =
        fun n ->
         if (( (SSet.mem n ( !loaded_modules )) ) || (
              (List.mem n ( !PreCast.loaded_modules )) )) then
          ()
          
         else begin
          (
          (add_to_loaded_modules n)
          );
          (DynLoader.load dyn_loader ( (n ^ objext) ))
         end in
       (
       (match (n, ( (String.lowercase x) )) with
        | (("Parsers" | ""),
           (((("pa_r.cmo" | "r") | "ocamlr") | "ocamlrevised")
            | "camlp4ocamlrevisedparser.cmo")) ->
           (pa_r (module PreCast))
        | (("Parsers" | ""),
           ((("rr" | "reloaded") | "ocamlreloaded")
            | "camlp4ocamlreloadedparser.cmo")) ->
           (pa_rr (module PreCast))
        | (("Parsers" | ""),
           ((("pa_o.cmo" | "o") | "ocaml") | "camlp4ocamlparser.cmo")) ->
           ( (pa_r (module PreCast)) ); (pa_o (module PreCast))
        | (("Parsers" | ""),
           ((("pa_rp.cmo" | "rp") | "rparser")
            | "camlp4ocamlrevisedparserparser.cmo")) ->
           ( (pa_r (module PreCast)) ); (pa_rp (module PreCast))
        | (("Parsers" | ""),
           ((("pa_op.cmo" | "op") | "parser")
            | "camlp4ocamlparserparser.cmo")) ->
           (
           (pa_r (module PreCast))
           );
           (
           (pa_o (module PreCast))
           );
           (
           (pa_rp (module PreCast))
           );
           (pa_op (module PreCast))
        | (("Parsers" | ""),
           (((("pa_extend.cmo" | "pa_extend_m.cmo") | "g") | "grammar")
            | "camlp4grammarparser.cmo")) ->
           (pa_g (module PreCast))
        | (("Parsers" | ""),
           ((("pa_macro.cmo" | "m") | "macro") | "camlp4macroparser.cmo")) ->
           (pa_m (module PreCast))
        | (("Parsers" | ""), ("q" | "camlp4quotationexpander.cmo")) ->
           (pa_q (module PreCast))
        | (("Parsers" | ""),
           (("q_mlast.cmo" | "rq")
            | "camlp4ocamlrevisedquotationexpander.cmo")) ->
           (pa_rq (module PreCast))
        | (("Parsers" | ""),
           ("oq" | "camlp4ocamloriginalquotationexpander.cmo")) ->
           (
           (pa_r (module PreCast))
           );
           (
           (pa_o (module PreCast))
           );
           (pa_oq (module PreCast))
        | (("Parsers" | ""), "rf") ->
           (
           (pa_r (module PreCast))
           );
           (
           (pa_rp (module PreCast))
           );
           (
           (pa_q (module PreCast))
           );
           (
           (pa_g (module PreCast))
           );
           (
           (pa_l (module PreCast))
           );
           (pa_m (module PreCast))
        | (("Parsers" | ""), "debug") -> (pa_debug (module PreCast))
        | (("Parsers" | ""), "of") ->
           (
           (pa_r (module PreCast))
           );
           (
           (pa_o (module PreCast))
           );
           (
           (pa_rp (module PreCast))
           );
           (
           (pa_op (module PreCast))
           );
           (
           (pa_q (module PreCast))
           );
           (
           (pa_g (module PreCast))
           );
           (
           (pa_l (module PreCast))
           );
           (pa_m (module PreCast))
        | (("Parsers" | ""), ("comp" | "camlp4listcomprehension.cmo")) ->
           (pa_l (module PreCast))
        | (("Filters" | ""), ("lift" | "camlp4astlifter.cmo")) ->
           (f_lift (module PreCast))
        | (("Filters" | ""), ("exn" | "camlp4exceptiontracer.cmo")) ->
           (f_exn (module PreCast))
        | (("Filters" | ""), ("prof" | "camlp4profiler.cmo")) ->
           (f_prof (module PreCast))
        | (("Filters" | ""), ("map" | "camlp4mapgenerator.cmo")) ->
           (f_fold (module PreCast))
        | (("Filters" | ""), ("fold" | "camlp4foldgenerator.cmo")) ->
           (f_fold (module PreCast))
        | (("Filters" | ""), ("meta" | "camlp4metagenerator.cmo")) ->
           (f_meta (module PreCast))
        | (("Filters" | ""), ("trash" | "camlp4trashremover.cmo")) ->
           (f_trash (module PreCast))
        | (("Filters" | ""), ("striploc" | "camlp4locationstripper.cmo")) ->
           (f_striploc (module PreCast))
        | (("Printers" | ""),
           ((("pr_o.cmo" | "o") | "ocaml") | "camlp4ocamlprinter.cmo")) ->
           (PreCast.enable_ocaml_printer () )
        | (("Printers" | ""),
           ((("pr_dump.cmo" | "p") | "dumpocaml")
            | "camlp4ocamlastdumper.cmo")) ->
           (PreCast.enable_dump_ocaml_ast_printer () )
        | (("Printers" | ""), (("d" | "dumpcamlp4") | "camlp4astdumper.cmo")) ->
           (PreCast.enable_dump_camlp4_ast_printer () )
        | (("Printers" | ""), (("a" | "auto") | "camlp4autoprinter.cmo")) ->
           (
           (load "Camlp4Autoprinter")
           );
           let (module P)  = (Hashtbl.find printers "camlp4autoprinter") in
           (P.apply (module PreCast))
        | _ ->
           let y = ("Camlp4" ^ ( (n ^ ( ("/" ^ ( (x ^ objext) )) )) )) in
           (real_load ( (try (find_in_path y) with
                         Not_found -> x) )))
       );
       ((!rcall_callback) () )

    let print_warning = (eprintf "%a:\n%s@." PreCast.Loc.print)

    let rec parse_file =
     fun dyn_loader ->
      fun name ->
       fun pa ->
        fun getdir ->
         let directive_handler =
          (Some
            (fun ast ->
              (match (getdir ast) with
               | Some (x) ->
                  (match x with
                   | (_, "load", s) -> ( (rewrite_and_load "" s) ); (None)
                   | (_, "directory", s) ->
                      ( (DynLoader.include_dir dyn_loader s) ); (None)
                   | (_, "use", s) ->
                      (Some (parse_file dyn_loader s pa getdir))
                   | (_, "default_quotation", s) ->
                      ( (PreCast.Quotation.default := s) ); (None)
                   | (loc, _, _) ->
                      (PreCast.Loc.raise loc (
                        (Stream.Error
                          ("bad directive camlp4 can not handled ")) )))
               | None -> (None)))) in
         let loc = (PreCast.Loc.mk name) in
         (
         (PreCast.Syntax.current_warning := print_warning)
         );
         let ic = if (name = "-") then stdin else (open_in_bin name) in
         let cs = (Stream.of_channel ic) in
         let clear = fun ()  -> if (name = "-") then ()  else (close_in ic) in
         let phr =
          (try (pa ?directive_handler:directive_handler loc cs) with
           x -> ( (clear () ) ); (raise x)) in
         (
         (clear () )
         );
         phr

    let output_file = (ref None )

    let process =
     fun dyn_loader ->
      fun name ->
       fun pa ->
        fun pr ->
         fun clean ->
          fun fold_filters ->
           fun getdir ->
            ((
              ((
                (( (parse_file dyn_loader name pa getdir) ) |> (
                  (fold_filters ( fun t -> fun filter -> (filter t) )) )) )
                |> clean) ) |> (
              (pr ?input_file:( (Some (name)) ) ?output_file:( !output_file
                )) ))

    let gind =
     function
     | Ast.SgDir (loc, n, Ast.ExStr (_, s)) -> (Some (loc, n, s))
     | _ -> (None)

    let gimd =
     function
     | Ast.StDir (loc, n, Ast.ExStr (_, s)) -> (Some (loc, n, s))
     | _ -> (None)

    let process_intf =
     fun dyn_loader ->
      fun name ->
       (process dyn_loader name PreCast.CurrentParser.parse_interf
         PreCast.CurrentPrinter.print_interf (
         ((new CleanAst.clean_ast)#sig_item) ) AstFilters.fold_interf_filters
         gind)

    let process_impl =
     fun dyn_loader ->
      fun name ->
       (process dyn_loader name PreCast.CurrentParser.parse_implem
         PreCast.CurrentPrinter.print_implem (
         ((new CleanAst.clean_ast)#str_item) ) AstFilters.fold_implem_filters
         gimd)

    let just_print_the_version =
     fun ()  -> ( (printf "%s@." FanConfig.version) ); (exit 0)

    let print_version =
     fun ()
       ->
      ( (eprintf "Camlp4 version %s@." FanConfig.version) ); (exit 0)

    let print_stdlib =
     fun ()
       ->
      ( (printf "%s@." FanConfig.camlp4_standard_library) ); (exit 0)

    let usage =
     fun ini_sl ->
      fun ext_sl ->
       (
       (eprintf
         "Usage: camlp4 [load-options] [--] [other-options]\nOptions:\n<file>.ml        Parse this implementation file\n<file>.mli       Parse this interface file\n<file>.%s Load this module inside the Camlp4 core@."
         ( if DynLoader.is_native then "cmxs     " else "(cmo|cma)" ))
       );
       (
       (Options.print_usage_list ini_sl)
       );
       if (ext_sl <> [] )
       then
        begin
        (
        (eprintf "Options added by loaded object files:@.")
        );
        (Options.print_usage_list ext_sl)
       end else ()

    let warn_noassert =
     fun ()
       ->
      (eprintf
        "camlp4 warning: option -noassert is obsolete\nYou should give the -noassert option to the ocaml compiler instead.@.")

    type file_kind =
       Intf of string
     | Impl of string
     | Str of string
     | ModuleImpl of string
     | IncludeDir of string

    let search_stdlib = (ref true )

    let print_loaded_modules = (ref false )

    let (task, do_task) =
     let t = (ref None ) in
     let task =
      fun f ->
       fun x ->
        let () = (FanConfig.current_input_file := x) in
        (t := (
          (Some
            (if (( !t ) = None ) then ( fun _ -> (f x) )
             else fun usage -> (usage () ))) )) in
     let do_task =
      fun usage -> (match !t with | Some (f) -> (f usage) | None -> ()) in
     (task, do_task)

    let input_file =
     fun x ->
      let dyn_loader = ((!dyn_loader) () ) in
      (
      ((!rcall_callback) () )
      );
      (
      (match x with
       | Intf (file_name) -> (task ( (process_intf dyn_loader) ) file_name)
       | Impl (file_name) -> (task ( (process_impl dyn_loader) ) file_name)
       | Str (s) ->
          let (f, o) = (Filename.open_temp_file "from_string" ".ml") in
          (
          (output_string o s)
          );
          (
          (close_out o)
          );
          (
          (task ( (process_impl dyn_loader) ) f)
          );
          (at_exit ( fun ()  -> (Sys.remove f) ))
       | ModuleImpl (file_name) -> (rewrite_and_load "" file_name)
       | IncludeDir (dir) -> (DynLoader.include_dir dyn_loader dir))
      );
      ((!rcall_callback) () )

    let initial_spec_list =
     [("-I", ( (Arg.String (fun x -> (input_file ( (IncludeDir (x)) )))) ),
       "<directory>  Add directory in search patch for object files.");
      ("-where", ( (Arg.Unit (print_stdlib)) ),
       "Print camlp4 library directory and exit.");
      ("-nolib", ( (Arg.Clear (search_stdlib)) ),
       "No automatic search for object files in library directory.");
      ("-intf", ( (Arg.String (fun x -> (input_file ( (Intf (x)) )))) ),
       "<file>  Parse <file> as an interface, whatever its extension.");
      ("-impl", ( (Arg.String (fun x -> (input_file ( (Impl (x)) )))) ),
       "<file>  Parse <file> as an implementation, whatever its extension.");
      ("-str", ( (Arg.String (fun x -> (input_file ( (Str (x)) )))) ),
       "<string>  Parse <string> as an implementation.");
      ("-unsafe", ( (Arg.Set (FanConfig.unsafe)) ),
       "Generate unsafe accesses to array and strings.");
      ("-noassert", ( (Arg.Unit (warn_noassert)) ),
       "Obsolete, do not use this option.");
      ("-verbose", ( (Arg.Set (FanConfig.verbose)) ),
       "More verbose in parsing errors.");
      ("-loc", ( (Arg.Set_string (Loc.name)) ), (
       ("<name>   Name of the location variable (default: " ^ (
         (( !Loc.name ) ^ ").") )) ));
      ("-QD", (
       (Arg.String (fun x -> (Quotation.dump_file := ( (Some (x)) )))) ),
       "<file> Dump quotation expander result in case of syntax error.");
      ("-o", ( (Arg.String (fun x -> (output_file := ( (Some (x)) )))) ),
       "<file> Output on <file> instead of standard output.");
      ("-v", ( (Arg.Unit (print_version)) ),
       "Print Camlp4 version and exit.");
      ("-version", ( (Arg.Unit (just_print_the_version)) ),
       "Print Camlp4 version number and exit.");
      ("-vnum", ( (Arg.Unit (just_print_the_version)) ),
       "Print Camlp4 version number and exit.");
      ("-no_quot", ( (Arg.Clear (FanConfig.quotations)) ),
       "Don't parse quotations, allowing to use, e.g. \"<:>\" as token.");
      ("-loaded-modules", ( (Arg.Set (print_loaded_modules)) ),
       "Print the list of loaded modules.");
      ("-parser", ( (Arg.String (rewrite_and_load "Parsers")) ),
       "<name>  Load the parser Camlp4Parsers/<name>.cm(o|a|xs)");
      ("-printer", ( (Arg.String (rewrite_and_load "Printers")) ),
       "<name>  Load the printer Camlp4Printers/<name>.cm(o|a|xs)");
      ("-filter", ( (Arg.String (rewrite_and_load "Filters")) ),
       "<name>  Load the filter Camlp4Filters/<name>.cm(o|a|xs)");
      ("-ignore", ( (Arg.String (ignore)) ), "ignore the next argument");
      ("--", ( (Arg.Unit (ignore)) ), "Deprecated, does nothing")]

    let _ = (Options.init initial_spec_list)

    let anon_fun =
     fun name ->
      (input_file (
        if (Filename.check_suffix name ".mli") then ( (Intf (name)) )
        else if (Filename.check_suffix name ".ml") then ( (Impl (name)) )
        else if (Filename.check_suffix name objext) then
              (
              (ModuleImpl (name))
              )
        else if (Filename.check_suffix name libext) then
              (
              (ModuleImpl (name))
              )
        else (raise ( (Arg.Bad ("don't know what to do with " ^ name)) )) ))

    let main =
     fun argv ->
      let usage =
       fun ()
         ->
        (
        (usage initial_spec_list ( (Options.ext_spec_list () ) ))
        );
        (exit 0) in
      (try
        let dynloader =
         (DynLoader.mk ~ocaml_stdlib:( !search_stdlib ) ~camlp4_stdlib:(
           !search_stdlib ) () ) in
        (
        (dyn_loader := ( fun ()  -> dynloader ))
        );
        let call_callback =
         fun ()
           ->
          (PreCast.iter_and_take_callbacks (
            fun (name, module_callback) ->
             let () = (add_to_loaded_modules name) in (module_callback () )
            )) in
        (
        (call_callback () )
        );
        (
        (rcall_callback := call_callback)
        );
        (
        (match (Options.parse anon_fun argv) with
         | [] -> ()
         | (((("-help" | "--help") | "-h") | "-?") :: _) -> (usage () )
         | (s :: _) ->
            (
            (eprintf "%s: unknown or misused option\n" s)
            );
            (
            (eprintf "Use option -help for usage@.")
            );
            (exit 2))
        );
        (
        (do_task usage)
        );
        (
        (call_callback () )
        );
        if !print_loaded_modules then
         (
         (SSet.iter ( (eprintf "%s@.") ) ( !loaded_modules ))
         )
        else ()
       with
       | Arg.Bad (s) ->
          (
          (eprintf "Error: %s\n" s)
          );
          (
          (eprintf "Use option -help for usage@.")
          );
          (exit 2)
       | Arg.Help (_) -> (usage () )
       | exc ->
          (
          (eprintf "@[<v0>%a@]@." FanUtil.ErrorHandler.print exc)
          );
          (exit 2))

    let _ = (main Sys.argv)

   end
