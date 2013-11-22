(** *)


let set_paths () =
  begin
    Config.load_path := !Config.load_path @ [Configf.fan_standard_library];
    Config.load_path := "" :: !Config.load_path;
    Dll.add_path !Config.load_path;
  end

(** see {!Compile.initial_env} and {!Toploop.init_toplevel}*)    
let initial_env  () =
  begin
    Ident.reinit();
    Toploop.toplevel_env := Env.open_pers_signature "Pervasives" Env.initial;
  end
let _ =
  begin
    set_paths ();
    initial_env ();
    (* Toploop.set_paths(); *)
    (* Toploop.initialize_toplevel_env (); *)
    Fdir.register
      ("eval",fun loc c ->
          let s  = Gramf.parse_string ~loc Syntaxf.strus c  in
          FEval.eval_ast Format.err_formatter s 
        );
    register_bin_printer (); (** default *)
    Options.adds Fan_args.initial_spec_list;
    Ast_parsers.use_parsers [ "revise"];
    try
      Arg.parse_dynamic
        Options.init_spec_list
        Fan_args.anon_fun "fan <options> <file>\nOptions are:\n"
    with exc -> begin Format.eprintf "@[<v0>%s@]@." (Printexc.to_string exc); exit 2 end
  end

