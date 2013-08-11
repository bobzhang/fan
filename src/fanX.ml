


let set_paths () =
  begin
    Config.load_path := !Config.load_path @ [FConfig.fan_standard_library];
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
          let s  = Fgram.parse_string ~loc Fsyntax.strus c  in
          FEval.eval_ast Format.err_formatter s 
        );
    Printexc.register_printer Mktop.normal_handler;
    PreCast.register_bin_printer (); (** default *)
    Printexc.register_printer
        (function
          |FLoc.Exc_located (loc, exn) ->
              Some (Format.sprintf "%s:@\n%s" (FLoc.to_string loc) (Printexc.to_string exn))
          | _ -> None );

    Foptions.adds MkFan.initial_spec_list;
    AstParsers.use_parsers [ "revise"; "stream"; (* "macro"; *)];
    try
      Arg.parse_dynamic
        Foptions.init_spec_list
        MkFan.anon_fun "fan <options> <file>\nOptions are:\n" (* in *)
    with exc -> begin Format.eprintf "@[<v0>%s@]@." (Printexc.to_string exc); exit 2 end
  end

