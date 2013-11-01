


type t = ((string * Locf.t) Streamf.t  * (string * Locf.t) Queue.t )
let mk () =
  let q = Queue.create () in
  let f _ =
    (* debug comments "take...@\n" in *)
  try Some (Queue.take q) with | Queue.Empty -> None 
  in (Streamf.from f, q)

let filter (_, q) =
  let rec self : Tokenf.stream -> Tokenf.stream = %parser{
    |  `Comment x; 'xs  -> begin
        Queue.add (x.txt, x.loc) q;
        (* debug comments "add: %S at %a@\n" x Locf.dump loc in *)
        self xs
    end
    |  x; 'xs ->
        (* debug comments "Found %a at %a@." Token.print x Locf.dump loc in *)
        %stream{x;'self xs}
    |  -> %stream{}}  in self


let take_list (_, q) =
  let rec self accu =
    if Queue.is_empty q then accu else self (Queue.take q :: accu)
  in self []

let take_stream = fst
  
let define token_fiter comments_strm =
  (* debug comments "Define a comment filter@\n" in *)
   Tokenf.set_filter token_fiter
  (fun previous strm -> previous (filter comments_strm strm))



(* local variables: *)
(* compile-command: "cd .. && pmake main_annot/comment_filter.cmo" *)
(* end: *)
