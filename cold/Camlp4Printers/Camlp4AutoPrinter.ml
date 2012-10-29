open MakeCamlp4Bin
let _= Register.enable_auto (fun () -> Unix.isatty Unix.stdout)