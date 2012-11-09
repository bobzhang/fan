open LibUtil
let slist0 ~f  ps =
  let rec loop al (__strm : _ Stream.t) =
    match try Some (ps __strm) with | Stream.Failure  -> None with
    | Some a -> loop (a :: al) __strm
    | _ -> al in
  fun (__strm : _ Stream.t)  -> let a = loop [] __strm in f a
let slist1 ~f  ps =
  let rec loop al (__strm : _ Stream.t) =
    match try Some (ps __strm) with | Stream.Failure  -> None with
    | Some a -> loop (a :: al) __strm
    | _ -> al in
  fun (__strm : _ Stream.t)  -> let a = ps __strm in f (loop [a] __strm)
let slist0sep ~err  ~f  s sep =
  let rec kont al (__strm : _ Stream.t) =
    match try Some (sep __strm) with | Stream.Failure  -> None with
    | Some v ->
        let a =
          try s __strm with | Stream.Failure  -> raise (Stream.Error (err v)) in
        kont (a :: al) __strm
    | _ -> al in
  fun (__strm : _ Stream.t)  ->
    match try Some (s __strm) with | Stream.Failure  -> None with
    | Some a -> f (kont [a] __strm)
    | _ -> f []
let opt ps ~f  (__strm : _ Stream.t) =
  match try Some (ps __strm) with | Stream.Failure  -> None with
  | Some a -> f (Some a)
  | _ -> f None
let tryp ps strm =
  let strm' = Stream.dup strm in
  let r =
    try ps strm'
    with
    | Stream.Error _|FanLoc.Exc_located (_,Stream.Error _) ->
        raise Stream.Failure
    | exc -> raise exc in
  Stream.njunk (Stream.count strm') strm; r
let orp ?(msg= "")  p1 p2 (__strm : _ Stream.t) =
  try p1 __strm
  with
  | Stream.Failure  ->
      (try p2 __strm with | Stream.Failure  -> raise (Stream.Error msg))