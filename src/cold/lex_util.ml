open Tag_regexp
let rec nullable (x : regexp) =
  match x with
  | Empty |Tag _ -> true
  | Chars (_,_)|Action _ -> false
  | Seq (r1,r2) -> (nullable r1) && (nullable r2)
  | Alt (r1,r2) -> (nullable r1) || (nullable r2)
  | Star _r -> true
type t_transition =  
  | OnChars of int
  | ToAction of int 
module Tag =
  struct
    type t = tag_info 
    let compare (t1 : tag_info) t2 = Pervasives.compare t1 t2
  end
module Tags = Set.Make(Tag)
type transition = (t_transition* Tags.t) 
module TagMap = Map.Make(Tag)
module Trans =
  struct
    type t = transition 
    let compare (t1,tags1) (t2,tags2) =
      match Pervasives.compare t1 t2 with
      | 0 -> Tags.compare tags1 tags2
      | r -> r
  end
module TransSet = Set.Make(Trans)
let rec emptymatch (x : regexp) =
  (match x with
   | Empty |Chars (_,_)|Action _ -> Tags.empty
   | Tag t -> Tags.add t Tags.empty
   | Seq (r1,r2) -> Tags.union (emptymatch r1) (emptymatch r2)
   | Alt (r1,r2) -> if nullable r1 then emptymatch r1 else emptymatch r2
   | Star r -> if nullable r then emptymatch r else Tags.empty : Tags.t )
let addtags (transs : TransSet.t) (tags : Tags.t) =
  (TransSet.fold
     (fun (t,tags_t)  r  -> TransSet.add (t, (Tags.union tags tags_t)) r)
     transs TransSet.empty : TransSet.t )
let rec firstpos x =
  (match x with
   | Empty |Tag _ -> TransSet.empty
   | Chars (pos,_) -> TransSet.singleton ((OnChars pos), Tags.empty)
   | Action act -> TransSet.singleton ((ToAction act), Tags.empty)
   | Seq (r1,r2) ->
       if nullable r1
       then
         TransSet.union (firstpos r1) (addtags (firstpos r2) (emptymatch r1))
       else firstpos r1
   | Alt (r1,r2) -> TransSet.union (firstpos r1) (firstpos r2)
   | Star r -> firstpos r : TransSet.t )
let followpos size entry_list =
  let v = Array.create size TransSet.empty in
  let rec fill s =
    function
    | Empty |Action _|Tag _ -> ()
    | Chars (n,_) -> v.(n) <- s
    | Alt (r1,r2) -> (fill s r1; fill s r2)
    | Seq (r1,r2) ->
        (fill
           (if nullable r2
            then TransSet.union (firstpos r2) (addtags s (emptymatch r2))
            else firstpos r2) r1;
         fill s r2)
    | Star r -> fill (TransSet.union (firstpos r) s) r in
  List.iter
    (fun (entry,_)  -> fill TransSet.empty entry.Translate_lex.lex_regexp)
    entry_list;
  v
