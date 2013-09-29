
let readfile chan =
  let rec loop rlst =
    let try line = input_line chan in
    loop [line :: rlst]
    with
      End_of_file -> List.rev rlst in
  loop [];



















