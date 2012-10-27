let sfold0 (f) (e) (_entry) (_symbl) (psymb) =
  let rec fold (accu) ((__strm : _ Stream.t )) = begin match begin try
    Some ((psymb __strm))
    with
    | Stream.Failure  ->   None
  end with | Some(a) ->   (fold ( (f a accu) ) __strm)
           | _ ->   accu
    end in
  (fun ((__strm : _ Stream.t )) -> (fold e __strm))
let sfold1 (f) (e) (_entry) (_symbl) (psymb) =
  let rec fold (accu) ((__strm : _ Stream.t )) = begin match begin try
    Some ((psymb __strm))
    with
    | Stream.Failure  ->   None
  end with | Some(a) ->   (fold ( (f a accu) ) __strm)
           | _ ->   accu
    end in
  (fun ((__strm : _ Stream.t )) ->
    let a = (psymb __strm) in begin try
      (fold ( (f a e) ) __strm)
      with
      | Stream.Failure  ->   (raise ( Stream.Error ("") ))
    end)
let sfold0sep (f) (e) (entry) (symbl) (psymb) (psep) =
  let failed =
    (function
    | symb::sep::[]  ->   (Failed.symb_failed_txt entry sep symb)
    | _ ->   "failed") in
  let rec kont (accu) ((__strm : _ Stream.t )) = begin match begin try
    Some ((psep __strm))
    with
    | Stream.Failure  ->   None
  end with
    | Some(() ) ->
        let a = begin try (psymb __strm)
          with
          | Stream.Failure  ->   (raise ( Stream.Error ((failed symbl)) ))
        end in (kont ( (f a accu) ) __strm)
    | _ ->   accu end in
  (fun ((__strm : _ Stream.t )) -> begin match begin try
    Some ((psymb __strm))
    with
    | Stream.Failure  ->   None end with
    | Some(a) ->   (kont ( (f a e) ) __strm)
    | _ ->   e end)