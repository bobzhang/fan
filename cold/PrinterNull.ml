module Id = struct
  let name = "Printers.Null" let version = Sys.ocaml_version
  end
module P = struct
  let print_interf ?input_file  ?output_file  _ = ()
  let print_implem ?input_file  ?output_file  _ = ()
  end