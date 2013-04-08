let _ =
  (try let a = 3 in fun ()  -> 4 with | e -> (fun ()  -> assert false)) ()