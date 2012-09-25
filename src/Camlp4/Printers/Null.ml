module Id = struct
  value name = "Camlp4.Printers.Null";
  value version = Sys.ocaml_version;
end;

module P = struct
  value print_interf ?input_file:(_) ?output_file:(_) _ = ();
  value print_implem ?input_file:(_) ?output_file:(_) _ = ();
end;
