open LibUtil
let sfold0 f e _entry _symbl psymb =
  let rec fold accu (__strm : _ XStream.t) =
    match try Some (psymb __strm) with | XStream.Failure  -> None with
    | Some a -> fold (f a accu) __strm
    | _ -> accu in
  fun (__strm : _ XStream.t)  -> fold e __strm
let sfold1 f e _entry _symbl psymb =
  let rec fold accu (__strm : _ XStream.t) =
    match try Some (psymb __strm) with | XStream.Failure  -> None with
    | Some a -> fold (f a accu) __strm
    | _ -> accu in
  fun (__strm : _ XStream.t)  ->
    let a = psymb __strm in
    try fold (f a e) __strm
    with | XStream.Failure  -> raise (XStream.Error "")
let sfold0sep f e entry symbl psymb psep =
  let failed =
    function
    | symb::sep::[] -> Failed.symb_failed_txt entry sep symb
    | _ -> assert false in
  let rec kont accu (__strm : _ XStream.t) =
    match try Some (psep __strm) with | XStream.Failure  -> None with
    | Some () ->
        let a =
          try psymb __strm
          with | XStream.Failure  -> raise (XStream.Error (failed symbl)) in
        kont (f a accu) __strm
    | _ -> accu in
  fun (__strm : _ XStream.t)  ->
    match try Some (psymb __strm) with | XStream.Failure  -> None with
    | Some a -> kont (f a e) __strm
    | _ -> e