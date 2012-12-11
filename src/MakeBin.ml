open Filters;
open Format;
open LibUtil;


(*be careful, since you can register your own [str_item_parser],
  if you do it in-consistently, this may result in an
  in-consistent behavior *)  
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
let just_print_filters () =
  let pp = eprintf (* and f = Format.std_formatter *) in 
  let p_tbl f tbl = Hashtbl.iter (fun k _v -> fprintf f "%s@;" k) tbl in
  begin
    pp  "@[for interface:@[<hv2>%a@]@]@." p_tbl AstFilters.interf_filters ;
    pp  "@[for phrase:@[<hv2>%a@]@]@." p_tbl AstFilters.implem_filters ;
    pp  "@[for top_phrase:@[<hv2>%a@]@]@." p_tbl AstFilters.topphrase_filters 
  end;
let just_print_parsers () =
  let pp = eprintf in
  let p_tbl f tbl = Hashtbl.iter (fun k _v -> fprintf f "%s@;" k) tbl in begin
    pp "@[Loaded Parsers:@;@[<hv2>%a@]@]@." p_tbl AstParsers.registered_parsers
  end;
  
let just_print_applied_parsers () =
  let pp = eprintf in
  pp "@[Applied Parsers:@;@[<hv2>%a@]@]@."
    (fun f q -> Queue.iter (fun (k,_) -> fprintf f "%s@;" k) q  ) AstParsers.applied_parsers;

open ParserListComprehension;
open ParserRevise;
open ParserMacro;
open ParserGrammar;
open ParserDebug;
open ParserStream;
AstParsers.use_parsers ["revise";"stream";"debug";"macro";"ListComprehension"];
  
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

module Camlp4Bin
     (PreCast:Sig.PRECAST) = struct

   let printers : (Hashtbl.t string (module Sig.PRECAST_PLUGIN)) = Hashtbl.create 30;
     (* let dyn_loader = ref (fun () -> failwith "empty in dynloader"); *)
  let rcall_callback = ref (fun () -> ());
    let loaded_modules = ref SSet.empty;
    let add_to_loaded_modules name =
      loaded_modules := SSet.add name !loaded_modules;
        
    Printexc.register_printer
            (
              fun [ FanLoc.Exc_located (loc, exn) ->
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
          [("Printers"|"",
            "pr_o.cmo" | "o" | "ocaml" | "camlp4ocamlprinter.cmo") -> 
              PreCast.enable_ocaml_printer ()
          | ("Printers"|"",
             "pr_dump.cmo" | "p" | "dumpocaml" | "camlp4ocamlastdumper.cmo") -> 
              PreCast.enable_dump_ocaml_ast_printer ()
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
            real_load (try find_in_path y with [ Not_found -> x ]) ];
          !rcall_callback ();
        end;
     let print_warning = eprintf "%a:\n%s@." FanLoc.print;  
     let output_file = ref None;                
     let parse_file  ?directive_handler name pa =
        let loc = FanLoc.mk name in begin
          PreCast.Syntax.current_warning := print_warning;
          let ic = if name = "-" then stdin else open_in_bin name;
          let cs = XStream.of_channel ic;
          let clear () = if name = "-" then () else close_in ic;
          let phr =
            try pa ?directive_handler loc cs
            with [x -> begin  clear (); raise x end ];
          clear ();
          phr
        end;
       let  rec sig_handler  = with "sig_item"
          (fun
            [ {| #load $str:s |} ->
              begin rewrite_and_load "" s; None end
            | {| #directory $str:s |} ->
                begin DynLoader.include_dir (!DynLoader.instance ()) s ; None end
            | {| #use $str:s |} ->
                Some (parse_file  ~directive_handler:sig_handler s PreCast.CurrentParser.parse_interf )
            | {| #default_quotation $str:s |} ->
                begin AstQuotation.default := s; None end
            | {| #filter $str:s |} ->
                begin AstFilters.use_interf_filter s; None ; end 
            | {@loc| # $x $_ |} -> (* FIXME pattern match should give _loc automatically *)
                FanLoc.raise loc (XStream.Error (x ^ " is abad directive camlp4 can not handled "))
            | _ -> assert false
            ] );
      let rec str_handler = with "str_item"
          (fun
            [ {| #load $str:s |} ->
              begin rewrite_and_load "" s; None end
            | {| #directory $str:s |} ->
                begin DynLoader.include_dir (!DynLoader.instance ()) s ; None end
            | {| #use $str:s |} ->
                Some (parse_file  ~directive_handler:str_handler s PreCast.CurrentParser.parse_implem )
            | {| #default_quotation $str:s |} ->
                begin AstQuotation.default := s; None end
            | {| #lang_at $str:tag $str:quot |} ->
                begin AstQuotation.default_at_pos tag quot; None end
            | {| #lang_clear |} -> begin 
                AstQuotation.default:="";
                Hashtbl.clear AstQuotation.default_tbl;
                None
            end
            | {| #filter $str:s |} ->
                begin AstFilters.use_implem_filter s; None ; end 
            | {@loc| # $x $_ |} -> (* FIXME pattern match should give _loc automatically *)
                FanLoc.raise loc (XStream.Error (x ^ "bad directive camlp4 can not handled "))
            | _ -> assert false
            ] );
      let process  ?directive_handler name pa pr clean fold_filters =
          parse_file  ?directive_handler name pa 
          |> fold_filters 
          |> clean
          |> pr ?input_file:(Some name) ?output_file:!output_file ;

      (* [entrance] *)  
      let process_intf  name =
        process ~directive_handler:sig_handler
          name PreCast.CurrentParser.parse_interf PreCast.CurrentPrinter.print_interf
                (new Camlp4Ast.clean_ast)#sig_item
                AstFilters.apply_interf_filters;
      let process_impl  name =
        process ~directive_handler:str_handler
          name
          PreCast.CurrentParser.parse_implem
          PreCast.CurrentPrinter.print_implem
          (new Camlp4Ast.clean_ast)#str_item
          AstFilters.apply_implem_filters
          (* gimd *);

      
      let input_file x =
        let dyn_loader = !DynLoader.instance () in 
        begin
          !rcall_callback ();
          match x with
          [ Intf file_name -> task (process_intf (* dyn_loader *)) file_name
          | Impl file_name -> task (process_impl (* dyn_loader *)) file_name
          | Str s ->
              begin
                let (f, o) = Filename.open_temp_file "from_string" ".ml";
                output_string o s;
                close_out o;
                task (process_impl (* dyn_loader *)) f;
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
         ("-QD", FanArg.String (fun x -> AstQuotation.dump_file := Some x),
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
         ("-loaded-filters", FanArg.Unit just_print_filters, "Print the registered filters.");
         ("-loaded-parsers", FanArg.Unit just_print_parsers, "Print the loaded parsers.");
         ("-used-parsers", FanArg.Unit just_print_applied_parsers, "Print the applied parsers.");
         ("-parser", FanArg.String (rewrite_and_load "Parsers"),
          "<name>  Load the parser FanParsers/<name>.cm(o|a|xs)");
         ("-printer", FanArg.String (rewrite_and_load "Printers"),
          "<name>  Load the printer Camlp4Printers/<name>.cm(o|a|xs)");
         ("-ignore", FanArg.String ignore, "ignore the next argument");
         ("--", FanArg.Unit ignore, "Deprecated, does nothing")];
      
      PreCast.Syntax.Options.adds initial_spec_list;

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
      with [exc -> begin eprintf "@[<v0>%s@]@." (Printexc.to_string exc); exit 2 end];
      main ();
    end ;
    

