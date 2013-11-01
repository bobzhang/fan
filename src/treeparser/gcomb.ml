
open Streamf



let slist0 ~f ps  =
  let rec loop al (__strm : _ t) =
    match try Some (ps __strm) with  NotConsumed  -> None with
    | Some a -> loop (a :: al) __strm
    | _ -> al in
  fun (__strm : _ t)  -> f @@ loop [] __strm 

let slist1 ~f  ps =
  let rec loop al (s : _ t) =
    match try Some (ps s) with  NotConsumed  -> None with
    | Some a -> loop (a :: al) s
    | _ -> al in
  fun (s : _ t)  ->
    let a = ps s in f (loop [a] s)

let slist0sep ~err  ~f  s sep =
  let rec kont al (__strm : _ t) =
    match try Some (sep __strm) with  NotConsumed  -> None with
    | Some v ->
        let a =
          try s __strm
          with  NotConsumed  -> raise (Error (err v)) in
        kont (a :: al) __strm
    | _ -> al in
  fun (__strm : _ t)  ->
    match try Some (s __strm) with  NotConsumed  -> None with
    | Some a -> f (kont [a] __strm)
    | _ -> f []


let slist1sep ~err  ~f  s sep =
  let rec kont al (__strm : _ t) =
    match try Some (sep __strm) with NotConsumed  -> None with
    | Some v ->
        let a =
          try s __strm
          with  NotConsumed  -> raise (Error (err v)) in
        kont (a :: al) __strm
    | _ -> al in
  fun (__strm : _ t)  -> let a = s __strm in f (kont [a] __strm)
    
        
let tryp ps strm =
  let strm' = dup strm in
  let r =
    try ps strm' with
    | Error _ | Locf.Exc_located (_, Error _) ->
        raise NotConsumed
    | exc -> raise exc  in begin 
        njunk (count strm') strm ;
        r;
    end
    
let peek ps strm =
  let strm' = dup strm in
  let r =
    try ps strm' with
    | Error _ | Locf.Exc_located (_, (Error _)) ->
        raise NotConsumed
    | exc -> raise exc  in 
  r

let orp ?(msg= "")  p1 p2 (__strm : _ t) =
  try p1 __strm with
    NotConsumed  ->
     (try p2 __strm with  NotConsumed  -> raise (Error msg))

