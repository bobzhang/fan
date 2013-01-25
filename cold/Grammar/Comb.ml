open LibUtil
let slist0 ~f  ps =
  let rec loop al (__strm : _ XStream.t) =
    match try Some (ps __strm) with | XStream.Failure  -> None with
    | Some a -> loop (a :: al) __strm
    | _ -> al in
  fun (__strm : _ XStream.t)  -> let a = loop [] __strm in f a
let slist1 ~f  ps =
  let rec loop al (__strm : _ XStream.t) =
    match try Some (ps __strm) with | XStream.Failure  -> None with
    | Some a -> loop (a :: al) __strm
    | _ -> al in
  fun (__strm : _ XStream.t)  -> let a = ps __strm in f (loop [a] __strm)
let slist0sep ~err  ~f  s sep =
  let rec kont al (__strm : _ XStream.t) =
    match try Some (sep __strm) with | XStream.Failure  -> None with
    | Some v ->
        let a =
          try s __strm
          with | XStream.Failure  -> raise (XStream.Error (err v)) in
        kont (a :: al) __strm
    | _ -> al in
  fun (__strm : _ XStream.t)  ->
    match try Some (s __strm) with | XStream.Failure  -> None with
    | Some a -> f (kont [a] __strm)
    | _ -> f []
let slist1sep ~err  ~f  s sep =
  let rec kont al (__strm : _ XStream.t) =
    match try Some (s __strm) with | XStream.Failure  -> None with
    | Some v ->
        let a =
          try sep __strm
          with | XStream.Failure  -> raise (XStream.Error (err v)) in
        kont (a :: al) __strm
    | _ -> al in
  fun (__strm : _ XStream.t)  -> let a = sep __strm in f (kont [a] __strm)
let opt ps ~f  (__strm : _ XStream.t) =
  match try Some (ps __strm) with | XStream.Failure  -> None with
  | Some a -> f (Some a)
  | _ -> f None
let tryp ps strm =
  let strm' = XStream.dup strm in
  let r =
    try ps strm'
    with
    | XStream.Error _|FanLoc.Exc_located (_,XStream.Error _) ->
        raise XStream.Failure
    | exc -> raise exc in
  XStream.njunk (XStream.count strm') strm; r
let peek ps strm =
  let strm' = XStream.dup strm in
  let r =
    try ps strm'
    with
    | XStream.Error _|FanLoc.Exc_located (_,XStream.Error _) ->
        raise XStream.Failure
    | exc -> raise exc in
  r
let orp ?(msg= "")  p1 p2 (__strm : _ XStream.t) =
  try p1 __strm
  with
  | XStream.Failure  ->
      (try p2 __strm with | XStream.Failure  -> raise (XStream.Error msg))