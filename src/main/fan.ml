


  
let _ =
  begin
    Options.adds Fan_args.initial_spec_list;
    Ast_parsers.use_parsers [ "fan"];
    try
      Arg.parse_dynamic
        Options.init_spec_list
        Fan_args.anon_fun "fan <options> <file>\nOptions are:"
    with exc -> begin Format.eprintf "@[<v0>%s@]@." (Printexc.to_string exc); exit 2 end
  end




