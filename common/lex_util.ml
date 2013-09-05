

(* To generate directly a NFA from a regular expression.
   Confer Aho-Sethi-Ullman, dragon book, chap. 3
   Extension to tagged automata.
   Confer
   Ville Larikari
   ``NFAs with Tagged Transitions, their Conversion to Deterministic
   Automata and Application to Regular Expressions''.
   Symposium on String Processing and Information Retrieval (SPIRE 2000),
   http://kouli.iki.fi/~vlaurika/spire2000-tnfa.ps
   (See also)
   http://kouli.iki.fi/~vlaurika/regex-submatch.ps.gz
 *)

open Automata_def
(* exception Memory_overflow *)

let rec nullable (x:regexp) =
  match x with 
  | Empty|Tag _ -> true
  | Chars (_,_)
  |Action _ -> false
  | Seq(r1,r2) -> nullable r1 && nullable r2
  | Alt(r1,r2) -> nullable r1 || nullable r2
  | Star _r     -> true


let tag_compare t1 t2 = Pervasives.compare t1 t2
type t_transition =
  | OnChars of int
  | ToAction of int

module Tags = Set.Make(struct type t = tag_info let compare = tag_compare end)
module TagMap = Map.Make (struct type t = tag_info let compare = tag_compare end)
type transition = (t_transition * Tags.t)

let trans_compare (t1,tags1) (t2,tags2) =
  match Pervasives.compare  t1 t2 with
  | 0 -> Tags.compare tags1 tags2
  | r -> r


module TransSet =
  Set.Make(struct type t = transition let compare = trans_compare end)


let rec emptymatch (x:regexp) : Tags.t =
  match x with 
  | Empty | Chars (_,_) | Action _ -> Tags.empty
  | Tag t       -> Tags.add t Tags.empty
  | Seq (r1,r2) -> Tags.union (emptymatch r1) (emptymatch r2)
  | Alt(r1,r2)  ->
      if nullable r1 then
        emptymatch r1
      else
        emptymatch r2
  | Star r ->
      if nullable r then
        emptymatch r
      else
        Tags.empty

let addtags (transs:TransSet.t) (tags:Tags.t) : TransSet.t =
  TransSet.fold
    (fun (t,tags_t) r -> TransSet.add (t, Tags.union tags tags_t) r)
    transs TransSet.empty


let rec firstpos x : TransSet.t =
  match x with
  | Empty|Tag _ -> TransSet.empty
  | Chars (pos,_) -> TransSet.add (OnChars pos,Tags.empty) TransSet.empty
  | Action act -> TransSet.add (ToAction act,Tags.empty) TransSet.empty
  | Seq(r1,r2) ->
      if nullable r1 then
        TransSet.union (firstpos r1) (addtags (firstpos r2) (emptymatch r1))
      else
        firstpos r1
  | Alt(r1,r2) -> TransSet.union (firstpos r1) (firstpos r2)
  | Star r     -> firstpos r

(* Berry-sethi followpos *)
let followpos size entry_list =
  let v = Array.create size TransSet.empty in
  let rec fill s = function
    | Empty|Action _|Tag _ -> ()
    | Chars (n,_) -> v.(n) <- s
    | Alt (r1,r2) ->
        (fill s r1 ; fill s r2)
    | Seq (r1,r2) ->
        (fill
          (if nullable r2 then
            TransSet.union (firstpos r2) (addtags s (emptymatch r2))
          else
            (firstpos r2))
          r1 ;
        fill s r2)
    | Star r ->
        fill (TransSet.union (firstpos r) s) r in
  (List.iter (fun (entry,_) -> fill TransSet.empty entry.lex_regexp) entry_list ;
  v)
