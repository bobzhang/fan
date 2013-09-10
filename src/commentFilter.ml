


type t = ((string * FLoc.t) XStream.t  * (string * FLoc.t) Queue.t )
let mk () =
  let q = Queue.create () in
  let f _ =
    (* debug comments "take...@\n" in *)
  try Some (Queue.take q) with | Queue.Empty -> None 
  in (XStream.from f, q)

let filter (_, q) =
  let rec self = parser
    |  (`COMMENT x, loc); 'xs  -> begin
        Queue.add (x, loc) q;
        (* debug comments "add: %S at %a@\n" x FLoc.dump loc in *)
        self xs
    end
    |  x; 'xs ->
        (* debug comments "Found %a at %a@." Token.print x FLoc.dump loc in *)
        {:stream|x;'self xs|}
    |  -> {:stream||}  in self


let take_list (_, q) =
  let rec self accu =
    if Queue.is_empty q then accu else self (Queue.take q :: accu)
  in self []

let take_stream = fst
  
let define token_fiter comments_strm =
  (* debug comments "Define a comment filter@\n" in *)
   FanTokenFilter.set_filter token_fiter
  (fun previous strm -> previous (filter comments_strm strm))


