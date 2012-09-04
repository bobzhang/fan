open MakeCamlp4Bin

let _ = (Camlp4.Register.enable_auto (
                              fun ()  -> (Unix.isatty Unix.stdout) ))
