open MakeBin
let _= Register.enable_auto ( fun () -> Unix.isatty Unix.stdout )