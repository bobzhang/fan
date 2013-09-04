(* Compiling a lexer definition *)
open LibUtil
open LexSyntax

open Automata_def
(* Deep abstract syntax for regular expressions *)

type tag_info = {id : string ; start : bool ; action : int}
type regexp =
  | Empty
  | Chars of int * bool
  | Action of int
  | Tag of tag_info
  | Seq of regexp * regexp
  | Alt of regexp * regexp
  | Star of regexp


(* A lot of sets and map structures *)
module Id = struct
  type t = ident
  let compare (x:t) (y:t) =
    match x,y with `Lid(_,id1),`Lid(_,id2) -> String.compare id1 id2
end
module SSet = (* Set.Make(Id)  *)SSet
module IdMap = Map.Make (Id)

(*********************)
(* Variable cleaning *)
(*********************)

(* Silently eliminate nested variables *)

let rec do_remove_nested to_remove = function
  | Bind (e, (`Lid(_,x) as name)) ->
      if SSet.mem x to_remove then
        do_remove_nested to_remove e
      else
        Bind (do_remove_nested (SSet.add x to_remove) e, name)
  | Epsilon|Eof|Characters _ as e -> e
  | Sequence (e1, e2) ->
      Sequence
        (do_remove_nested to_remove  e1, do_remove_nested to_remove  e2)
  | Alternative (e1, e2) ->
      Alternative
        (do_remove_nested to_remove  e1, do_remove_nested to_remove  e2)
  | Repetition e ->
      Repetition (do_remove_nested to_remove  e)

let remove_nested_as e = do_remove_nested SSet.empty e

(*********************)
(* Variable analysis *)
(*********************)

(*
  Optional variables.
   A variable is optional when matching of regexp does not
   implies it binds.
     The typical case is:
       ("" | 'a' as x) -> optional
       ("" as x | 'a' as x) -> non-optional
*)

let stringset_delta s1 s2 =
  SSet.union
    (SSet.diff s1 s2)
    (SSet.diff s2 s1)

let rec find_all_vars = function
  | Characters _|Epsilon|Eof ->
      SSet.empty
  | Bind (e,`Lid(_,x)) ->
      SSet.add x (find_all_vars e)
  | Sequence (e1,e2)|Alternative (e1,e2) ->
      SSet.union (find_all_vars e1) (find_all_vars e2)
  | Repetition e -> find_all_vars e


let rec do_find_opt = function
  | Characters _|Epsilon|Eof -> (SSet.empty, SSet.empty)
  | Bind (e,`Lid(_,x)) ->
      let (opt,all) = do_find_opt e in
      (opt, SSet.add x all)
  | Sequence (e1,e2) ->
      let (opt1,all1) = do_find_opt e1
      and (opt2,all2) = do_find_opt e2 in
      (SSet.union opt1 opt2, SSet.union all1 all2)
  | Alternative (e1,e2) ->
      let (opt1,all1) = do_find_opt e1
      and (opt2,all2) = do_find_opt e2 in
      (SSet.union
         (SSet.union opt1 opt2)
         (stringset_delta all1 all2),
       SSet.union all1 all2)
  | Repetition e  ->
      let r = find_all_vars e in
      (r,r)

let find_optional e =
  let (r,_) = do_find_opt e in r

(*
   Double variables
   A variable is double when it can be bound more than once
   in a single matching
     The typical case is:
       (e1 as x) (e2 as x)

*)

let rec do_find_double = function
  | Characters _|Epsilon|Eof -> (SSet.empty, SSet.empty)
  | Bind (e,`Lid(_,x)) ->
      let (dbl,all) = do_find_double e in
      ((if SSet.mem x all then
        SSet.add x dbl
      else
        dbl),
      SSet.add x all)
  | Sequence (e1,e2) ->
      let (dbl1, all1) = do_find_double e1
      and (dbl2, all2) = do_find_double e2 in
      (SSet.union
        (SSet.inter all1 all2)
        (SSet.union dbl1 dbl2),
      SSet.union all1 all2)
  | Alternative (e1,e2) ->
      let (dbl1, all1) = do_find_double e1
      and (dbl2, all2) = do_find_double e2 in
      (SSet.union dbl1 dbl2,
      SSet.union all1 all2)
  | Repetition e ->
      let r = find_all_vars e in
      (r,r)

let find_double e = do_find_double e

(*
   Type of variables:
    A variable is bound to a char when all its occurences
    bind a pattern of length 1.
     The typical case is:
       (_ as x) -> char
*)

let add_some x = function
  | Some i -> Some (x+i)
  | None   -> None

let add_some_some x y =
  match (x,y) with
  | (Some i, Some j) -> Some (i+j)
  | (_,_)            -> None

let rec do_find_chars sz = function
  | Epsilon|Eof    -> (SSet.empty, SSet.empty, sz)
  | Characters _ -> (SSet.empty, SSet.empty, add_some 1 sz)
  | Bind (e,`Lid(_,x))   ->
      let (c,s,e_sz) = do_find_chars (Some 0) e in
      begin match e_sz  with
      | Some 1 ->
          (SSet.add x c,s,add_some 1 sz)
      | _ ->
          (c, SSet.add x s, add_some_some sz e_sz)
      end
  | Sequence (e1,e2) ->
      let (c1,s1,sz1) = do_find_chars sz e1 in
      let (c2,s2,sz2) = do_find_chars sz1 e2 in
      (SSet.union c1 c2,
       SSet.union s1 s2,
       sz2)
  | Alternative (e1,e2) ->
      let (c1,s1,sz1) = do_find_chars sz e1
      and (c2,s2,sz2) = do_find_chars sz e2 in
      (SSet.union c1 c2,
      SSet.union s1 s2,
      (if sz1 = sz2 then sz1 else None))
  | Repetition e -> do_find_chars None e



let find_chars e =
  let (c,s,_) = do_find_chars (Some 0) e in
  SSet.diff c s

(*******************************)
(* From shallow to deep syntax *)
(*******************************)

let chars = ref ([] : Fcset.t list)
let chars_count = ref 0


let rec encode_regexp (char_vars:SSet.t) (act:int) (x:LexSyntax.regular_expression) =
  match x with
  | Epsilon -> Empty
  | Characters cl ->
      let n = !chars_count in begin
        chars := cl :: !chars;
        incr chars_count;
        Chars(n,false)
      end
  | Eof ->
      let n = !chars_count in begin
        chars := Fcset.eof :: !chars;
        incr chars_count;
        Chars(n,true)
      end
  | Sequence(r1,r2) ->
      let r1 = encode_regexp char_vars act r1 in
      let r2 = encode_regexp char_vars act r2 in
      Seq (r1, r2)
  | Alternative(r1,r2) ->
      let r1 = encode_regexp char_vars act r1 in
      let r2 = encode_regexp char_vars act r2 in
      Alt(r1, r2)
  | Repetition r ->
      let r = encode_regexp char_vars act r in
      Star r
  | Bind (r,(`Lid(_,name) as x)) ->
      let r = encode_regexp char_vars act r in
      if SSet.mem (* x *) name char_vars then
        Seq (Tag {id=name ; start=true ; action=act},r)
      else
        Seq (Tag {id=name ; start=true ; action=act},
          Seq (r, Tag {id=name ; start=false ; action=act}))



(* Optimisation,
    Static optimization :
      Replace tags by offsets relative to the beginning
      or end of matched string.
    Dynamic optimization:
      Replace some non-optional, non-double tags by offsets w.r.t
      a previous similar tag.
*)

let incr_pos = function
  | None   -> None
  | Some i -> Some (i+1)

let decr_pos = function
  | None -> None
  | Some i -> Some (i-1)


let opt = true

let mk_seq r1 r2 =
  match (r1,r2)  with
  | (Empty,_) -> r2
  | (_,Empty) -> r1
  | (_,_)     -> Seq (r1,r2)

let add_pos p i =
  match p with
  | Some (Sum (a,n)) -> Some (Sum (a,n+i))
  | None -> None
          
let mem_name name id_set =
  SSet.exists (function | id_name (* (`Lid(_,id_name)) *) -> name = id_name) id_set (* FIXME*)

let opt_regexp all_vars char_vars optional_vars double_vars r =

(* From removed tags to their addresses *)
  let env = Hashtbl.create 17 in

(* First static optimizations, from start position *)
  let rec size_forward pos = function
    | Empty|Chars (_,true)|Tag _ -> Some pos
    | Chars (_,false) -> Some (pos+1)
    | Seq (r1,r2) ->
        begin match size_forward pos r1 with
        | None -> None
        | Some pos  -> size_forward pos r2
        end
    | Alt (r1,r2) ->
        let pos1 = size_forward pos r1
        and pos2 = size_forward pos r2 in
        if pos1=pos2 then pos1 else None
    | Star _ -> None
    | Action _ -> assert false in

  let rec simple_forward pos r = match r with
    | Tag n ->
        if mem_name n.id double_vars then
          (r,Some pos)
        else begin
          Hashtbl.add env (n.id,n.start) (Sum (Start, pos)) ;
          (Empty,Some pos)
        end
    | Empty -> (r, Some pos)
    | Chars (_,is_eof) ->
        (r,Some (if is_eof then  pos else pos+1))
    | Seq (r1,r2) ->
        let (r1,pos) = simple_forward pos r1 in
        begin match pos with
        | None -> (mk_seq r1 r2,None)
        | Some pos ->
            let (r2,pos) = simple_forward pos r2 in
            (mk_seq r1 r2,pos)
        end
    | Alt (r1,r2) ->
        let pos1 = size_forward pos r1
        and pos2 = size_forward pos r2 in
        (r,(if pos1=pos2 then pos1 else None))
    | Star _ -> (r,None)
    | Action _ -> assert false in

(* Then static optimizations, from end position *)
  let rec size_backward pos = function
    | Empty|Chars (_,true)|Tag _ -> Some pos
    | Chars (_,false) -> Some (pos-1)
    | Seq (r1,r2) ->
        begin match size_backward pos r2 with
        | None -> None
        | Some pos  -> size_backward pos r1
        end
    | Alt (r1,r2) ->
        let pos1 = size_backward pos r1
        and pos2 = size_backward pos r2 in
        if pos1=pos2 then pos1 else None
    | Star _ -> None
    | Action _ -> assert false in


  let rec simple_backward pos r = match r with
    | Tag n ->
        if mem_name n.id double_vars then
          (r,Some pos)
        else begin
          Hashtbl.add env (n.id,n.start) (Sum (End, pos)) ;
          (Empty,Some pos)
        end
    | Empty -> (r,Some pos)
    | Chars (_,is_eof) ->
        (r,Some (if is_eof then pos else pos-1))
    | Seq (r1,r2) ->
        let (r2,pos) = simple_backward pos r2 in
        begin match pos with
        | None -> (mk_seq r1 r2,None)
        | Some pos ->
            let (r1,pos) = simple_backward pos r1 in
            (mk_seq r1 r2,pos)
        end
    | Alt (r1,r2) ->
        let pos1 = size_backward pos r1
        and pos2 = size_backward pos r2 in
        (r,(if pos1=pos2 then pos1 else None))
    | Star _ -> (r,None)
    | Action _ -> assert false in

  let r =
    if opt then
      let (r,_) = simple_forward 0 r in
      let (r,_) = simple_backward 0 r in
      r
    else
      r in

  let loc_count = ref 0 in
  let get_tag_addr t =
    try
     Hashtbl.find env t
    with
    | Not_found ->
        let n = !loc_count in begin
          incr loc_count ;
          Hashtbl.add env t (Sum (Mem n,0)) ;
          Sum (Mem n,0)
        end in

  let rec alloc_exp pos r = match r with
    | Tag n ->
        if mem_name n.id double_vars then
          (r,pos)
        else begin match pos with
        | Some a -> begin
            Hashtbl.add env (n.id,n.start) a ;
            (Empty,pos)
        end
        | None ->
            let a = get_tag_addr (n.id,n.start) in
            (r,Some a)
        end

    | Empty -> (r,pos)
    | Chars (_,is_eof) -> (r,(if is_eof then pos else add_pos pos 1))
    | Seq (r1,r2) ->
        let (r1,pos) = alloc_exp pos r1 in
        let (r2,pos) = alloc_exp pos r2 in
        (mk_seq r1 r2,pos)
    | Alt (_,_) ->
        let off = size_forward 0 r in
        begin match off with
        | Some i -> (r,add_pos pos i)
        | None -> (r,None)
        end
    | Star _ -> (r,None)
    | Action _ -> assert false in

  let (r,_) = alloc_exp None r in
  let m =
    SSet.fold
      (fun x r ->
        match x with
        | (* `Lid(_,name) *) name -> 

        let v =
          if SSet.mem x char_vars then
            Ident_char
              (SSet.mem x optional_vars, get_tag_addr (name,true))
          else
            Ident_string
              (SSet.mem x optional_vars,
               get_tag_addr (name,true),
               get_tag_addr (name,false)) in
        (x,v)::r)
      all_vars [] in
  (m,r, !loc_count)
          
let encode_casedef casedef =
  List.fold_left
    (fun (reg,actions,count,ntags) (expr, act) ->
      let expr = remove_nested_as expr in
      let char_vars = find_chars expr in
      let r = encode_regexp char_vars count expr
      and opt_vars = find_optional expr
      and (double_vars,all_vars) = find_double expr in
      let (m,r,loc_ntags) =
        opt_regexp all_vars char_vars opt_vars double_vars r in
      (Alt(reg, Seq(r, Action count)),
       (count, m ,act) :: actions,
       (succ count),
       max loc_ntags ntags))
    (Empty, [], 0, 0)
    casedef 


let encode_lexdef def =
  (chars := [];
   chars_count := 0;
   let (entry_list : (lexer_entry *bool) list ) =
     List.map
       (fun {shortest=shortest ; clauses= casedef} ->
         let (re,actions,_,ntags) = encode_casedef casedef in
         ({lex_regexp = re;
            lex_mem_tags = ntags ;
            lex_actions = List.rev actions },
          shortest))
       def in
   let chr = Array.of_list (List.rev !chars) in
   (chars := [];
    (chr, entry_list)))
    
let encode_single_lexdef def =
  (chars := [];
   chars_count := 0;
   let result : (lexer_entry * bool)=
     match def with
       {shortest=shortest ; clauses= casedef} ->
         let (re,actions,_,ntags) = encode_casedef casedef in
         ({lex_regexp = re;
           lex_mem_tags = ntags ;
           lex_actions = List.rev actions },
          shortest) in
   let chr = Array.of_list (List.rev !chars) in
   (chars := [];
    (chr, result)))



type transition = (t_transition * Tags.t)
type t_transition =
  | OnChars of int
  | ToAction of int

        
let trans_compare (t1,tags1) (t2,tags2) =
  match Pervasives.compare  t1 t2 with
  | 0 -> Tags.compare tags1 tags2
  | r -> r


module TransSet =
  Set.Make(struct type t = transition let compare = trans_compare end)

let rec nullable = function
  | Empty|Tag _ -> true
  | Chars (_,_)|Action _ -> false
  | Seq(r1,r2) -> nullable r1 && nullable r2
  | Alt(r1,r2) -> nullable r1 || nullable r2
  | Star _r     -> true

let rec emptymatch = function
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

let addtags transs tags =
  TransSet.fold
    (fun (t,tags_t) r -> TransSet.add (t, Tags.union tags tags_t) r)
    transs TransSet.empty

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




let rec firstpos = function
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

