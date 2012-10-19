type t = ((string * FanLoc.t ) Stream.t  * (string * FanLoc.t ) Queue.t  ) 
let mk =
 fun ()
   ->
  let q = (Queue.create ()  ) in
  let f = fun _ -> (try (Some (Queue.take q )) with
                    Queue.Empty -> (None)) in
  (( (Stream.from f ) ) , q )
let filter =
 fun (_ , q ) ->
  let rec self =
   fun (__strm :
     _
    Stream.t
    ) ->
    (match (Stream.peek __strm ) with
     | Some ((`COMMENT x) , loc ) ->
        (
        (Stream.junk __strm )
        );
        let xs = __strm in ( (Queue.add (x , loc ) q ) ); (self xs )
     | Some (x) ->
        (
        (Stream.junk __strm )
        );
        let xs = __strm in
        (Stream.icons x ( (Stream.slazy ( fun _ -> (self xs ) ) ) ) )
     | _ -> Stream.sempty) in
  self
let take_list =
 fun (_ , q ) ->
  let rec self =
   fun accu ->
    if (Queue.is_empty q ) then accu
    else (self ( ( ( (Queue.take q ) ) ) :: accu  ) ) in
  (self []  )
let take_stream = fst
let define =
 fun token_fiter ->
  fun comments_strm ->
   (FanToken.Filter.define_filter token_fiter (
     fun previous -> fun strm -> (previous ( (filter comments_strm strm ) ) )
     ) )
