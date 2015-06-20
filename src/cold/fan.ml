
(** FIXME the command line parsing  can not handle prefix problem,
    e.g. -p -px will cause some problem *)    
(* let initial_spec_list : (string * Arg.spec * string) list = *)
(*   [ *)
   (* ("-I", String (fun x -> input_file (IncludeDir x)), *)
   (*  "<directory>  Add directory in search patch for object files."); *)

   (* ("-intf", String (fun x -> input_file (Intf x)), *)
   (*  "<file>  Parse <file> as an interface, whatever its extension."); *)

   (* ("-impl", String (fun x -> input_file (Impl x)), *)
   (*  "<file>  Parse <file> as an implementation, whatever its extension."); *)

   (* ("-str", String (fun x -> input_file (Str x)), *)
   (*  "<string>  Parse <string> as an implementation."); *)

   (* ("-o", String (fun x -> output_file := Some x), *)
   (*  "<file> Output on <file> instead of standard output."); *)

   (* ("-list", Unit Ast_quotation.dump_names_tbl, "list all registered DDSLs"); *)

   (* ("-list_directive", Unit Ast_quotation.dump_directives, "list all registered directives"); *)
   
   (* ("-unsafe", Set Configf.unsafe, *)
   (*  "Generate unsafe accesses to array and strings."); *)

   (* ("-where", Unit (fun () -> (print_endline Configf.fan_plugins_library;exit 0)) *)
   (*    , " Print location of standard library and exit"); *)
   (* ("-loc", Set_string Locf.name, *)
   (*  "<name>   Name of the location variable (default: " ^ !Locf.name ^ ")."); *)
   
   (* ("-v", Unit  (fun () -> begin eprintf "Fan version %s@." Configf.version; exit 0 end), *)
   (*  "Print Fan version and exit."); *)

   (* ("-compilation-unit", *)
   (*  Unit (function () ->  *)
   (*    ((match !Configf.compilation_unit with *)
   (*    | Some v -> printf "%s@." v *)
   (*    | None -> printf "null"); *)
   (*     exit 0)),  *)
   (*  "Print the current compilation unit"); *)

   (* ("-plugin", String Control_require.add , "load plugin cma or cmxs files"); *)

   (* ("-loaded-modules", Set print_loaded_modules, "Print the list of loaded modules."); *)
   
   (* ("-loaded-filters", Unit just_print_filters, "Print the registered filters."); *)
   
   (* ("-loaded-parsers", Unit just_print_parsers, "Print the loaded parsers."); *)
   
   (* ("-used-parsers", Unit just_print_applied_parsers, "Print the applied parsers."); *)

   (* ("-dlang", *)
   (*     (String *)
   (*        (fun s  -> *)
   (*          Ast_quotation.default := *)
   (*            (Ast_quotation.resolve_name {domain = `Sub []; name =  s}))), *)
   (*     " Set the default language"); *)
   (* ("-printer", *)
   (*  (String (fun s -> *)

   (*      let x  = *)
   (*        try Hashtbl.find Prelude.backends s with *)
   (*          Not_found -> Util.failwithf "%s backend not found" s in *)
   (*      begin *)
   (*        Prelude.sigi_printer := x.interf ; *)
   (*        Prelude.stru_printer := x.implem *)
   (*      end)) , " Set the backend"); *)
   (* ("-printers", *)
   (*  Unit (fun _ -> *)
   (*    Prelude.backends *)
   (*    |> Hashtbl.iter *)
   (*        (fun k (x:Prelude.backend) -> *)
   (*          Format.eprintf "@[-printer %s, %s@]@\n" k x.descr)), *)
   (*  " List the backends available") *)
 (* ];; *)
      

(** The first argument is file name
    we dispatch different functions based on the filename extension
    handle the file name 
 *)
let anon_fun name =
  let check = Filename.check_suffix name in
  Fan_args.input_file
  (
   (* if name = "-" then *)
   if check ".mli" then Intf name
   else if check ".ml" then Impl name
   else if check Dyn_load.objext then ModuleImpl name
   else if check Dyn_load.libext then ModuleImpl name
   else raise (Arg.Bad ("don't know what to do with " ^ name)));;

let compile  (x : Main_spec.compile_info) : unit = 
  let () = 
    match x.printer with 
    | None -> ()
    | Some s -> 
        let x  =
          try Hashtbl.find Prelude.backends s with
            Not_found -> Util.failwithf "%s backend not found" s in
        begin
          Prelude.sigi_printer := x.interf ;
          Prelude.stru_printer := x.implem
        end in
  let () = List.iter Control_require.add x.plugins in
  anon_fun x.file 



(*
Entry point
*)
open Cmdliner

let _ =
  begin
    (* Options.add *)
    (*   ("-keep", *)
    (*    (Arg.Set State.keep), "Keep the included type definitions") ; *)
    (* Options.add *)
    (*   ("-loaded-plugins", *)
    (*    (Arg.Unit Typehook.show_modules), "Show plugins"); *)

    (* Options.adds initial_spec_list; *)
    Ast_parsers.use_parsers [ "fan"];
    (* try *)
    (*   Arg.parse_dynamic *)
    (*     Options.init_spec_list *)
    (*     anon_fun "fan <options> <file>\nOptions are:" *)
    (* with exc -> begin Format.eprintf "@[<v0>%s@]@." (Printexc.to_string exc); exit 2 end *)
    Term.eval (Term.(app 
                       (pure  compile)
                       Main_spec.compile_info_arg), Main_spec.info)
  end





(* local variables: *)
(* compile-command: "cd .. && omake main_annot/fan.cmo" *)
(* end: *)
