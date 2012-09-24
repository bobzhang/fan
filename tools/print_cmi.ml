open Format

let main file =
  let chan = open_in file in 
  let infos = Cmi_format.input_cmi  chan in
  ()



















