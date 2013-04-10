open LibUtil

let sfold0 f e _entry _symbl psymb =
  let rec fold accu (__strm : _ XStream.t) =
    match try Some (psymb __strm) with | XStream.Failure  -> None with
    | Some a -> let s = __strm in fold (f a accu) s
    | _ -> accu in
  fun (__strm : _ XStream.t)  -> let a = fold e __strm in a

let sfold1 f e _entry _symbl psymb =
  let rec fold accu (__strm : _ XStream.t) =
    match try Some (psymb __strm) with | XStream.Failure  -> None with
    | Some a -> let s = __strm in fold (f a accu) s
    | _ -> accu in
  fun (__strm : _ XStream.t)  ->
    let a = psymb __strm in
    let a =
      try fold (f a e) __strm
      with | XStream.Failure  -> raise (XStream.Error "") in
    a

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
        let s = __strm in kont (f a accu) s
    | _ -> accu in
  fun (__strm : _ XStream.t)  ->
    match try Some (psymb __strm) with | XStream.Failure  -> None with
    | Some a -> let s = __strm in kont (f a e) s
    | _ -> e

let sfold1sep f e entry symbl psymb psep =
  let failed =
    function
    | symb::sep::[] -> Failed.symb_failed_txt entry sep symb
    | _ -> assert false in
  let parse_top =
    function
    | symb::_::[] -> FanParser.parser_of_symbol entry symb 0
    | _ -> raise XStream.Failure in
  let rec kont accu (__strm : _ XStream.t) =
    match try Some (psep __strm) with | XStream.Failure  -> None with
    | Some () ->
        let a =
          try
            match try Some (psymb __strm) with | XStream.Failure  -> None
            with
            | Some a -> a
            | _ ->
                (match try Some (parse_top symbl __strm)
                       with | XStream.Failure  -> None
                 with
                 | Some a -> Obj.magic a
                 | _ -> raise (XStream.Error (failed symbl)))
          with | XStream.Failure  -> raise (XStream.Error "") in
        let s = __strm in kont (f a accu) s
    | _ -> accu in
  fun (__strm : _ XStream.t)  ->
    let a = psymb __strm in let s = __strm in kont (f a e) s