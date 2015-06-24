
(** FIXME the command line parsing  can not handle prefix problem,
    e.g. -p -px will cause some problem *)    
(* let initial_spec_list : (string * Arg.spec * string) list = *)
(*   [ *)

   (* ("-intf", String (fun x -> input_file (Intf x)), *)
   (*  "<file>  Parse <file> as an interface, whatever its extension."); *)

   (* ("-impl", String (fun x -> input_file (Impl x)), *)
   (*  "<file>  Parse <file> as an implementation, whatever its extension."); *)

   (* ("-str", String (fun x -> input_file (Str x)), *)
   (*  "<string>  Parse <string> as an implementation."); *)


   (* ("-list", Unit Ast_quotation.dump_names_tbl, "list all registered DDSLs"); *)

   (* ("-list_directive", Unit Ast_quotation.dump_directives, "list all registered directives"); *)
   
   (* ("-unsafe", Set Configf.unsafe, *)
   (*  "Generate unsafe accesses to array and strings."); *)

   (* ("-loc", Set_string Locf.name, *)
   (*  "<name>   Name of the location variable (default: " ^ !Locf.name ^ ")."); *)
   

   (* ("-compilation-unit", *)
   (*  Unit (function () ->  *)
   (*    ((match !Configf.compilation_unit with *)
   (*    | Some v -> printf "%s@." v *)
   (*    | None -> printf "null"); *)
   (*     exit 0)),  *)
   (*  "Print the current compilation unit"); *)



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
 (* ];; *)
      

(** The first argument is file name
    we dispatch different functions based on the filename extension
    handle the file name 
 *)

let compile  (( { file=name ; _} as x) : Main_spec.compile_info) : unit = 
  begin

    (if x.show_where then 
      (print_endline Configf.fan_plugins_library;exit 0))
      ;

    (match x.output_file with 
    | Some _ -> Fan_args.output_file := x.output_file
    | _ -> ());


    List.iter (fun dir ->Ref.modify Configf.dynload_dirs (Util.cons dir)) x.include_dirs; 
    List.iter Control_require.add x.plugins ;

    if x.show_printers then 
      begin 
        Prelude.backends
        |> Hashtbl.iter (fun k (x:Prelude.backend) ->
            Format.eprintf "@[%s: %s@]@\n" k x.descr);
        exit 0
      end
    else ();
    (match x.printer with 
    | None -> ()
    | Some s -> 
        let x  =
          try Hashtbl.find Prelude.backends s with
            Not_found -> Util.failwithf "%s backend not found" s in
        begin
          Prelude.sigi_printer := x.interf ;
          Prelude.stru_printer := x.implem
        end );

    
    let check = Filename.check_suffix name in
    (** TODO: error check when [name=""] *)
    Fan_args.input_file
      (
       (* if name = "-" then *)
       if check ".mli" then Intf name
       else if check ".ml" then Impl name
       else if check Dyn_load.objext then ModuleImpl name
       else if check Dyn_load.libext then ModuleImpl name
       else raise (Arg.Bad ("don't know what to do with " ^ name)))
  end


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
