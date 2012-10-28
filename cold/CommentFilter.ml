type t = ((string *FanLoc.t ) Stream.t *(string *FanLoc.t ) Queue.t ) 
let mk (() ) =
  let q = (Queue.create () ) in
  let f _ = begin try Some ((Queue.take q))
    with
    | Queue.Empty  ->   None
  end in (( (Stream.from f) ),q)
let filter (_,q) =
  let rec self (__strm : _ Stream.t ) = begin match (Stream.peek __strm) with
    | Some (`COMMENT x,loc) ->
        begin
        (Stream.junk __strm);
        let xs = __strm in begin
          (Queue.add (x,loc) q);
          (self xs)
          end
        end
    | Some x ->
        begin
        (Stream.junk __strm);
        let xs = __strm in
        (Stream.icons x ( (Stream.slazy ( (fun _ -> (self xs)) )) ))
        end
    | _ ->   Stream.sempty end in
  self
let take_list (_,q) =
  let rec self accu =
    if (Queue.is_empty q) then begin
      accu
    end else begin
      (self ( (Queue.take q)::accu ))
    end in
  (self [] )
let take_stream = fst
let define token_fiter comments_strm =
  (FanToken.Filter.define_filter token_fiter (
    (fun previous ->
      (fun strm -> (previous ( (filter comments_strm strm) )))) ))