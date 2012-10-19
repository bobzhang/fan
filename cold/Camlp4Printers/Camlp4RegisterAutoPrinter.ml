open MakeCamlp4Bin
module P : PRINTER_PLUGIN =
 struct
  let apply =
   fun ((module
    Register)
     :
     (module MakeRegister.S
    )) ->
    (Register.enable_auto ( fun ()  -> (Unix.isatty Unix.stdout) ))
 end
let _ = (Hashtbl.replace printers "camlp4autoprinter" (module P))