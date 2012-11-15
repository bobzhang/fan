open FanParsers;
open Camlp4Filters;
open Format;
open LibUtil;
module Camlp4Bin
     (PreCast:Sig.PRECAST)
    =struct
      let printers : (Hashtbl.t string (module Sig.PRECAST_PLUGIN)) =
        Hashtbl.create 30;
      (* let dyn_loader = ref (fun () -> failwith "empty in dynloader"); *)
      let rcall_callback = ref (fun () -> ());
      let loaded_modules = ref SSet.empty;
      let add_to_loaded_modules name =
        loaded_modules := SSet.add name !loaded_modules;

     Printexc.register_printer
            (
              fun [ FanLoc.Exc_located loc exn ->
                    Some (sprintf "%s:@\n%s" (FanLoc.to_string loc) (Printexc.to_string exn))
                  | _ -> None ]);
      module DynLoader = DynLoader.Make (struct end);
      (* let plugins = Hashtbl.create 50;      *)
      let (objext,libext) =
        if DynLoader.is_native then (".cmxs",".cmxs")
        else (".cmo",".cma");
      
      let rewrite_and_load n x =
        let dyn_loader = !DynLoader.instance () in 
        let find_in_path = DynLoader.find_in_path dyn_loader in
        let real_load name = do 
          add_to_loaded_modules name;
          DynLoader.load dyn_loader name
        done in
        let load =  begin fun n ->
          if SSet.mem n !loaded_modules
          || List.mem n !PreCast.loaded_modules then ()
          else begin
            add_to_loaded_modules n;
            DynLoader.load dyn_loader (n ^ objext);
          end
        end in begin 
          match (n, String.lowercase x) with
          [ ("Parsers"|"",
             "pa_r.cmo" | "r"|"ocamlr"|"ocamlrevised" | "camlp4ocamlrevisedparser.cmo")
            -> begin
              pa_r (module PreCast) ;
            end
          | ("Parsers"|"",
             "pa_rp.cmo" | "rp" | "rparser" | "camlp4ocamlrevisedparserparser.cmo")
            -> begin
              pa_r (module PreCast);
              pa_rp (module PreCast);
            end 
          | ("Parsers"|"",
             "pa_extend.cmo" | "pa_extend_m.cmo" | "g" | "grammar" | "camlp4grammarparser.cmo")
            -> begin
              pa_g (module PreCast);
            end 
          | ("Parsers"|"",
             "pa_macro.cmo"  | "m"  | "macro" | "camlp4macroparser.cmo") -> begin
               pa_m (module PreCast);
             end 
          | ("Parsers"|"", "q" | "camlp4quotationexpander.cmo") -> begin
              pa_q (module PreCast); (* no pa_qb any more*)
          end
          | ("Parsers"|"", "rf") -> begin
              pa_r (module PreCast);
              pa_rp (module PreCast);
              pa_q (module PreCast);
              pa_g (module PreCast);
              pa_l (module PreCast);
              pa_m (module PreCast);
          end
          | ("Parsers"|"","debug") -> begin
              pa_debug (module PreCast);
          end
          | ("Parsers"|"",
             "comp" | "camlp4listcomprehension.cmo") ->
               begin
                 pa_l (module PreCast);
               end 
          | ("Filters"|"",
             "lift" | "camlp4astlifter.cmo") -> begin
               f_lift (module PreCast);
             end

          | ("Filters"|"",
             "exn" | "camlp4exceptiontracer.cmo") -> begin
               f_exn (module PreCast);
             end 
          | ("Filters"|"",
             "prof" | "camlp4profiler.cmo") -> begin 
               f_prof (module PreCast);
             end 
          (* map is now an alias of fold since fold handles map too *)
          | ("Filters"|"",
             "map" | "camlp4mapgenerator.cmo") -> begin
               f_fold (module PreCast);
             end 
          | ("Filters"|"", 
             "fold" | "camlp4foldgenerator.cmo") -> begin
               f_fold (module PreCast);
             end 
          | ("Filters"|"",
             "meta" | "camlp4metagenerator.cmo") -> begin
               f_meta (module PreCast);
             end 
          | ("Filters"|"",
             "trash" | "camlp4trashremover.cmo") -> begin
               f_trash (module PreCast);
             end 
          | ("Filters"|"",
             "striploc" | "camlp4locationstripper.cmo") -> begin
               f_striploc (module PreCast);
             end
          | ("Printers"|"",
             "pr_o.cmo" | "o" | "ocaml" | "camlp4ocamlprinter.cmo") -> begin
               PreCast.enable_ocaml_printer ();
             end 
          | ("Printers"|"",
             "pr_dump.cmo" | "p" | "dumpocaml" | "camlp4ocamlastdumper.cmo") -> begin 
              PreCast.enable_dump_ocaml_ast_printer ()
             end
          | ("Printers"|"",
             "d" | "dumpcamlp4" | "camlp4astdumper.cmo") ->
              PreCast.enable_dump_camlp4_ast_printer ()
          | ("Printers"|"",
             "a" | "auto" | "camlp4autoprinter.cmo") ->
               (* FIXME introduced dependency on Unix *)
               (* PreCast.enable_auto (fun [ () -> Unix.isatty Unix.stdout]) *)
               begin
                 load "Camlp4Autoprinter";
                 let (module P ) = Hashtbl.find printers "camlp4autoprinter" in
                 P.apply (module PreCast);
               end
          | _ ->
            let y = "Camlp4"^n^"/"^x^objext in
            real_load (try find_in_path y with [ Not_found -> x ])
          ];
          !rcall_callback ();
        end;
      
      
     let print_warning = eprintf "%a:\n%s@." FanLoc.print;  
      (* camlp4 directive handler *)  
      let rec parse_file dyn_loader name pa getdir =
        let directive_handler = Some (fun ast ->
          match getdir ast with
          [ Some x ->
              match x with
              [ (_, "load", s) -> begin  rewrite_and_load "" s; None end
              | (_, "directory", s) -> begin  DynLoader.include_dir dyn_loader s; None end
              | (_, "use", s) -> Some (parse_file dyn_loader s pa getdir)
              | (_, "default_quotation", s) -> begin PreCast.Syntax.Quotation.default := s; None end
              | (loc, _, _) -> FanLoc.raise loc (Stream.Error "bad directive camlp4 can not handled ") ]
          | None -> None ]) in
        let loc = FanLoc.mk name
        in do 
          PreCast.Syntax.current_warning := print_warning;
          let ic = if name = "-" then stdin else open_in_bin name;
          let cs = Stream.of_channel ic;
          let clear () = if name = "-" then () else close_in ic;
          let phr =
            try pa ?directive_handler loc cs
            with x -> begin  clear (); raise x end ;
          clear ();
          phr
        done ;
      
      let output_file = ref None;
      
      let process dyn_loader name pa pr clean fold_filters getdir =
          parse_file dyn_loader name pa getdir
          |> fold_filters (fun t filter -> filter t )
          |> clean
          |> pr ?input_file:(Some name) ?output_file:!output_file ;
      let gind = fun
        [ {:sig_item@loc| # $n $str:s |} -> Some (loc, n, s)
        | _ -> None ];
      
      let gimd = fun
        [ {:str_item@loc| # $n $str:s |} -> Some (loc, n, s)
        | _ -> None ];

      (* [entrance] *)  
      let process_intf dyn_loader name =
        process dyn_loader name PreCast.CurrentParser.parse_interf PreCast.CurrentPrinter.print_interf
                (new Camlp4Ast.clean_ast)#sig_item
                PreCast.Syntax.AstFilters.fold_interf_filters gind;
      let process_impl dyn_loader name =
        process
          dyn_loader
          name
          PreCast.CurrentParser.parse_implem
          PreCast.CurrentPrinter.print_implem
          (new Camlp4Ast.clean_ast)#str_item
          PreCast.Syntax.AstFilters.fold_implem_filters
          gimd;

      (*be careful, since you can register your own [str_item_parser],
        if you do it in-consistently, this may result in an
        in-consistent behavior
       *)  
        
      let just_print_the_version () =
        begin  printf "%s@." FanConfig.version; exit 0 end ;
      
      let print_version () =
        begin eprintf "Camlp4 version %s@." FanConfig.version; exit 0 end;
      
      let print_stdlib () =
        begin  printf "%s@." FanConfig.camlp4_standard_library; exit 0 end;
      
      let warn_noassert () =
        begin
          eprintf "\
      camlp4 warning: option -noassert is obsolete\n\
      You should give the -noassert option to the ocaml compiler instead.@.";
        end;
      
      type file_kind =
        [ Intf of string
        | Impl of string
        | Str of string
        | ModuleImpl of string
        | IncludeDir of string ];
      
      let search_stdlib = ref true;
      let print_loaded_modules = ref false;
      let task f x =
          let () = FanConfig.current_input_file := x in
          f x ;
      let input_file x =
        let dyn_loader = !DynLoader.instance () in 
        begin
          !rcall_callback ();
          match x with
          [ Intf file_name -> task (process_intf dyn_loader) file_name
          | Impl file_name -> task (process_impl dyn_loader) file_name
          | Str s ->
              begin
                let (f, o) = Filename.open_temp_file "from_string" ".ml";
                output_string o s;
                close_out o;
                task (process_impl dyn_loader) f;
                at_exit (fun () -> Sys.remove f);
              end
          | ModuleImpl file_name -> rewrite_and_load "" file_name
          | IncludeDir dir -> DynLoader.include_dir dyn_loader dir ];
          !rcall_callback ();
        end;
      
      let initial_spec_list =
        [("-I", FanArg.String (fun x -> input_file (IncludeDir x)),
          "<directory>  Add directory in search patch for object files.");
         ("-where", FanArg.Unit print_stdlib,
          "Print camlp4 library directory and exit.");
         ("-nolib", FanArg.Clear search_stdlib,
          "No automatic search for object files in library directory.");
         ("-intf", FanArg.String (fun x -> input_file (Intf x)),
          "<file>  Parse <file> as an interface, whatever its extension.");
         ("-impl", FanArg.String (fun x -> input_file (Impl x)),
          "<file>  Parse <file> as an implementation, whatever its extension.");
         ("-str", FanArg.String (fun x -> input_file (Str x)),
          "<string>  Parse <string> as an implementation.");
         ("-unsafe", FanArg.Set FanConfig.unsafe,
          "Generate unsafe accesses to array and strings.");
         ("-noassert", FanArg.Unit warn_noassert,
          "Obsolete, do not use this option.");
         ("-verbose", FanArg.Set FanConfig.verbose,
          "More verbose in parsing errors.");
         ("-loc", FanArg.Set_string FanLoc.name,
          "<name>   Name of the location variable (default: " ^ !FanLoc.name ^ ").");
         ("-QD", FanArg.String (fun x -> PreCast.Syntax.Quotation.dump_file := Some x),
          "<file> Dump quotation expander result in case of syntax error.");
         ("-o", FanArg.String (fun x -> output_file := Some x),
          "<file> Output on <file> instead of standard output.");
         ("-v", FanArg.Unit print_version,
          "Print Camlp4 version and exit.");
         ("-version", FanArg.Unit just_print_the_version,
          "Print Camlp4 version number and exit.");
         ("-vnum", FanArg.Unit just_print_the_version,
          "Print Camlp4 version number and exit.");
         ("-no_quot", FanArg.Clear FanConfig.quotations,
          "Don't parse quotations, allowing to use, e.g. \"<:>\" as token.");
         ("-parsing-strict",FanArg.Set FanConfig.strict_parsing, "");
         (* FIXME the command line parsing sucks, it can not handle prefix problem*)
         ("-loaded-modules", FanArg.Set print_loaded_modules, "Print the list of loaded modules.");
         ("-parser", FanArg.String (rewrite_and_load "Parsers"),
          "<name>  Load the parser FanParsers/<name>.cm(o|a|xs)");
         ("-printer", FanArg.String (rewrite_and_load "Printers"),
          "<name>  Load the printer Camlp4Printers/<name>.cm(o|a|xs)");
         ("-filter", FanArg.String (rewrite_and_load "Filters"),
          "<name>  Load the filter Camlp4Filters/<name>.cm(o|a|xs)");
         ("-ignore", FanArg.String ignore, "ignore the next argument");
         ("--", FanArg.Unit ignore, "Deprecated, does nothing")];
      
      PreCast.Syntax.Options.init initial_spec_list;

      (* handle the file name *)  
      let anon_fun name =
        input_file
        (if Filename.check_suffix name ".mli" then Intf name
          else if Filename.check_suffix name ".ml" then Impl name
          else if Filename.check_suffix name objext then ModuleImpl name
          else if Filename.check_suffix name libext then ModuleImpl name
          else raise (FanArg.Bad ("don't know what to do with " ^ name)));
      
      let main () = try
        begin 
          let dynloader = DynLoader.mk ~ocaml_stdlib:!search_stdlib
                                       ~camlp4_stdlib:!search_stdlib () ;
          DynLoader.instance := fun () -> dynloader ;
          let call_callback () =
            PreCast.iter_and_take_callbacks
              (fun (name, module_callback) ->
                 let () = add_to_loaded_modules name in
                 module_callback ()) ;
          call_callback () ; 
          rcall_callback := call_callback;
          FanArg.parse
              PreCast.Syntax.Options.init_spec_list anon_fun "fan <options> <file>\nOptions are:\n";
          call_callback ();
          if !print_loaded_modules then begin
            SSet.iter (eprintf "%s@.") !loaded_modules;
          end else ()
        end
      with exc -> begin eprintf "@[<v0>%s@]@." (Printexc.to_string exc); exit 2 end;
      main ();
    end ;
    
