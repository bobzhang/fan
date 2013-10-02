
(* Format of a .cmo file:
     magic number (Config.cmo_magic_number)
     absolute offset of compilation unit descriptor
     block of relocatable bytecode
     debugging information if any
     compilation unit descriptor *)
let from filename =
  let ic = open_in_bin filename in
  let buffer =  Misc.input_bytes ic (String.length Config.cmo_magic_number) in
  if buffer = Config.cmo_magic_number then begin
    let compunit_pos = input_binary_int ic in
    seek_in ic compunit_pos;
    let cu : compilation_unit = input_value ic in
    cu
  end
  else invalid_arg "Cmo_decomp.from"

      
