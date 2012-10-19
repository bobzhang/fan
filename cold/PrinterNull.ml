module Id =
 struct let name = "Printers.Null" let version = Sys.ocaml_version  end
module P =
 struct
  let print_interf = fun ?input_file:_ -> fun ?output_file:_ -> fun _ -> ()
 let print_implem = fun ?input_file:_ -> fun ?output_file:_ -> fun _ -> ()
 
 end
