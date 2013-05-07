


type t = ((string * FanLoc.t) XStream.t  * (string * FanLoc.t) Queue.t )
let mk () =
  let q = Queue.create () in
  let f _ =
    (* debug comments "take...@\n" in *)
  try Some (Queue.take q) with | Queue.Empty -> None 
  in (XStream.from f, q)

let filter (_, q) =
  let rec self = parser
    [ [< (`COMMENT x, loc); 'xs >] -> begin
      Queue.add (x, loc) q;
      (* debug comments "add: %S at %a@\n" x FanLoc.dump loc in *)
      self xs
    end
    | [< x; 'xs >] ->
        (* debug comments "Found %a at %a@." Token.print x FanLoc.dump loc in *)
        [< x; 'self xs >]
    | [< >] -> [< >] ] in self


let take_list (_, q) =
  let rec self accu =
    if Queue.is_empty q then accu else self [Queue.take q :: accu]
  in self []

let take_stream = fst
  
let define token_fiter comments_strm =
  (* debug comments "Define a comment filter@\n" in *)
   FanTokenFilter.define_filter token_fiter
  (fun previous strm -> previous (filter comments_strm strm))


