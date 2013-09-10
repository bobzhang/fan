type t = ((string* FLoc.t) XStream.t* (string* FLoc.t) Queue.t) 
let mk () =
  let q = Queue.create () in
  let f _ = try Some (Queue.take q) with | Queue.Empty  -> None in
  ((XStream.from f), q)
let filter (_,q) =
  let rec self (__strm : _ XStream.t) =
    match XStream.peek __strm with
    | Some (`COMMENT x,loc) ->
        (XStream.junk __strm;
         (let xs = __strm in Queue.add (x, loc) q; self xs))
    | Some x ->
        (XStream.junk __strm;
         (let xs = __strm in
          XStream.icons x (XStream.slazy (fun _  -> self xs))))
    | _ -> XStream.sempty in
  self
let take_list (_,q) =
  let rec self accu =
    if Queue.is_empty q then accu else self ((Queue.take q) :: accu) in
  self []
let take_stream = fst
let define token_fiter comments_strm =
  FanTokenFilter.set_filter token_fiter
    (fun previous  strm  -> previous (filter comments_strm strm))