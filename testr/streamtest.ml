



let u =
  parser bp [<>] -> bp;


(*Stream.count broken for Stream literals
  [< 1 ; 2 ; 3 ;4 >]
  let _ = Stream.icons 1 (Stream.icons 2 (Stream.icons 3 (Stream.ising 4)))
 *)    


(*
  Stream8
  [<1 ; 2 ; 'from (fun i -> if i < 10 then Some i else None)>] |> iter print_int; 
  1223456789-

  Current Stream
  [<1 ; 2 ; 'from (fun i -> if i < 10 then Some i else None)>] |> iter print_int;
  120123456789
 *)
