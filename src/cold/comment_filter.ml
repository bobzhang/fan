type t = ((string* Locf.t) Streamf.t* (string* Locf.t) Queue.t) 
let mk () =
  let q = Queue.create () in
  let f _ = try Some (Queue.take q) with | Queue.Empty  -> None in
  ((Streamf.from f), q)
let filter (_,q) =
  let rec self (__strm : _ Streamf.t) =
    match Streamf.peek __strm with
    | Some (`Comment (_,x),loc) ->
        (Streamf.junk __strm;
         (let xs = __strm in Queue.add (x, loc) q; self xs))
    | Some x ->
        (Streamf.junk __strm;
         (let xs = __strm in
          Streamf.icons x (Streamf.slazy (fun _  -> self xs))))
    | _ -> Streamf.sempty in
  self
let take_list (_,q) =
  let rec self accu =
    if Queue.is_empty q then accu else self ((Queue.take q) :: accu) in
  self []
let take_stream = fst
let define token_fiter comments_strm =
  FanTokenFilter.set_filter token_fiter
    (fun previous  strm  -> previous (filter comments_strm strm))