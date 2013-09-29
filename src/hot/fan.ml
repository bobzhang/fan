
let _ =
  begin
    PreCast.register_bin_printer (); (** default to [-printer p] *)
    Foptions.adds MkFan.initial_spec_list;
    Ast_parsers.use_parsers [ "revise"; "stream"];
    try
      Arg.parse_dynamic
        Foptions.init_spec_list
        MkFan.anon_fun "fan <options> <file>\nOptions are:"
    with exc -> begin Format.eprintf "@[<v0>%s@]@." (Printexc.to_string exc); exit 2 end
  end




