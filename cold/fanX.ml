let set_paths () =
  Config.load_path :=
    (Config.load_path.contents @ [FConfig.fan_standard_library]);
  Config.load_path := ("" :: (Config.load_path.contents));
  Dll.add_path Config.load_path.contents
let initial_env () =
  Ident.reinit ();
  Toploop.toplevel_env := (Env.open_pers_signature "Pervasives" Env.initial)
let _ =
  set_paths ();
  initial_env ();
  Fdir.register
    ("eval",
      (fun loc  c  ->
         let s = Fgram.parse_string ~loc Fsyntax.strus c in
         FEval.eval_ast Format.err_formatter s));
  Printexc.register_printer Mktop.normal_handler;
  PreCast.register_bin_printer ();
  Printexc.register_printer
    (function
     | FLoc.Exc_located (loc,exn) ->
         Some
           (Format.sprintf "%s:@\n%s" (FLoc.to_string loc)
              (Printexc.to_string exn))
     | _ -> None);
  Foptions.adds MkFan.initial_spec_list;
  AstParsers.use_parsers ["revise"; "stream"];
  (try
     Arg.parse_dynamic Foptions.init_spec_list MkFan.anon_fun
       "fan <options> <file>\nOptions are:\n"
   with
   | exc -> (Format.eprintf "@[<v0>%s@]@." (Printexc.to_string exc); exit 2))