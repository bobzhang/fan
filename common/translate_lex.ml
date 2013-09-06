

open Automata_def

type concrete_regexp =
  | Epsilon
  | Characters of Fcset.t
  | Eof
  | Sequence of concrete_regexp * concrete_regexp
  | Alternative of concrete_regexp * concrete_regexp
  | Repetition of concrete_regexp
  | Bind of concrete_regexp * (FLoc.t * string)

type 'a entry =
  {shortest : bool ;
   clauses : (concrete_regexp * 'a) list}
  
module Id =   struct
  type t = (FLoc.t * string)
  let compare (_,x) (_,y) = String.compare x  y
end
  
module IdSet = Set.Make (Id)


(*********************)
(* Variable cleaning *)
(*********************)

(* Silently eliminate nested variables *)

let rec do_remove_nested (to_remove:IdSet.t) x : concrete_regexp =
  match x with
  | Bind (e,x) ->
      if IdSet.mem x to_remove then
        do_remove_nested to_remove e
      else
        Bind (do_remove_nested (IdSet.add x to_remove) e, x)
  | Epsilon|Eof|Characters _ as e -> e
  | Sequence (e1, e2) ->
      Sequence
        (do_remove_nested to_remove  e1, do_remove_nested to_remove  e2)
  | Alternative (e1, e2) ->
      Alternative
        (do_remove_nested to_remove  e1, do_remove_nested to_remove  e2)
  | Repetition e ->
      Repetition (do_remove_nested to_remove  e)



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
  let open IdSet in
  union (diff s1 s2) (diff s2 s1)

let rec find_all_vars (x:concrete_regexp) : IdSet.t=
  let open IdSet in
  match x with 
  | Characters _ | Epsilon |Eof ->
      empty
  | Bind (e,x) ->
      add x (find_all_vars e)
  | Sequence (e1,e2)|Alternative (e1,e2) ->
      union (find_all_vars e1) (find_all_vars e2)
  | Repetition e -> find_all_vars e

let remove_nested_as e = do_remove_nested IdSet.empty e

let rec do_find_opt x : IdSet.t * IdSet.t =
  let open IdSet in
  match x with 
  | Characters _|Epsilon|Eof -> (empty, empty)
  | Bind (e,x) ->
      let (opt,all) = do_find_opt e in
      (opt, add x all)
  | Sequence (e1,e2) ->
      let (opt1,all1) = do_find_opt e1
      and (opt2,all2) = do_find_opt e2 in
      (union opt1 opt2, union all1 all2)
  | Alternative (e1,e2) ->
      let (opt1,all1) = do_find_opt e1
      and (opt2,all2) = do_find_opt e2 in
      (union (union opt1 opt2) (stringset_delta all1 all2),
       union all1 all2)
  | Repetition e  ->
      let r = find_all_vars e in
      (r,r)

let find_optional e = fst @@ do_find_opt e 


(*
   Double variables
   A variable is double when it can be bound more than once
   in a single matching
     The typical case is:
       (e1 as x) (e2 as x)

*)

let rec do_find_double x : IdSet.t * IdSet.t =
  let open IdSet in
  match x with 
  | Characters _|Epsilon|Eof -> (empty, empty)
  | Bind (e,x) ->
      let (dbl,all) = do_find_double e in
      ((if mem x all then
        add x dbl
      else
        dbl),
      add x all)
  | Sequence (e1,e2) ->
      let (dbl1, all1) = do_find_double e1
      and (dbl2, all2) = do_find_double e2 in
      (union (inter all1 all2) (union dbl1 dbl2), union all1 all2)
  | Alternative (e1,e2) ->
      let (dbl1, all1) = do_find_double e1
      and (dbl2, all2) = do_find_double e2 in
      (union dbl1 dbl2, union all1 all2)
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
  match x,y with
  | (Some i, Some j) -> Some (i+j)
  | (_,_)            -> None


let rec do_find_chars sz x : IdSet.t * IdSet.t * int option =
  let open IdSet in
  match x with 
  | Epsilon|Eof    -> (empty, empty, sz)
  | Characters _ -> (empty, empty, add_some 1 sz)
  | Bind (e,x)   ->
      let (c,s,e_sz) = do_find_chars (Some 0) e in
      begin match e_sz  with
      | Some 1 ->
          (add x c,s,add_some 1 sz)
      | _ ->
          (c, add x s, add_some_some sz e_sz)
      end
  | Sequence (e1,e2) ->
      let (c1,s1,sz1) = do_find_chars sz e1 in
      let (c2,s2,sz2) = do_find_chars sz1 e2 in
      (union c1 c2,
       union s1 s2,
       sz2)
  | Alternative (e1,e2) ->
      let (c1,s1,sz1) = do_find_chars sz e1
      and (c2,s2,sz2) = do_find_chars sz e2 in
      (union c1 c2,
       union s1 s2,
       (if sz1 = sz2 then sz1 else None))
  | Repetition e -> do_find_chars None e



let find_chars e =
  let (c,s,_) = do_find_chars (Some 0) e in
  IdSet.diff c s

(*******************************)
(* From shallow to deep syntax *)
(*******************************)

let chars = ref ([] : Fcset.t list)
let chars_count = ref 0


let rec encode_regexp (char_vars:IdSet.t) (act:int) x : Automata_def.regexp =
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
  | Sequence(r1,r2)  ->
      let r1 = encode_regexp char_vars act r1 in
      let r2 = encode_regexp char_vars act r2 in
      Seq (r1, r2)
  | Alternative(r1,r2) -> 
      let r1 = encode_regexp char_vars act r1 in
      let r2 = encode_regexp char_vars act r2 in
      Alt (r1, r2)
  | Repetition r ->
      Star ( encode_regexp char_vars act r )
  | Bind (r,((_,name) as x)) ->
      let r = encode_regexp char_vars act r in
      if IdSet.mem x char_vars then
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



let mk_seq (r1: Automata_def.regexp) (r2 : Automata_def.regexp) : Automata_def.regexp=
  match r1,r2  with
  | Empty,_ -> r2
  | _,Empty -> r1
  | _     -> Seq (r1,r2)

let add_pos p i =
  match (p:Automata_def.tag_addr option) with
  | Some (Sum (a,n)) -> Some (Sum (a,n+i))
  | None -> None

let mem_name name (id_set:IdSet.t) : bool =
  IdSet.exists (function (_,id_name) -> name = id_name) id_set (* FIXME*)


(* First static optimizations, from start position *)
let rec size_forward pos (x:regexp) : int option =
  match x with 
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
  | Action _ -> assert false 

(* Then static optimizations, from end position *)
let rec size_backward pos (x:regexp) =
  match x with
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
  | Action _ -> assert false

(* type dir = Backward | Forward          *)
let opt_regexp all_vars char_vars optional_vars double_vars (r:regexp):
    (IdSet.elt * ident_info) list * regexp * int=
(* From removed tags to their addresses *)
  let env = Hashtbl.create 17 in
  let rec simple_forward pos r double_vars=
    match r with
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
        begin match simple_forward pos r1 double_vars with
        | r1,None -> (mk_seq r1 r2,None)
        | r1,Some pos ->
            let (r2,pos) = simple_forward pos r2 double_vars in
            (mk_seq r1 r2,pos)
        end
    | Alt (r1,r2) ->
        let pos1 = size_forward pos r1
        and pos2 = size_forward pos r2 in
        (r,if pos1=pos2 then pos1 else None)
    | Star _ -> (r,None)
    | Action _ -> assert false in
  let rec simple_backward pos r double_vars =
    match r with
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
        begin match simple_backward pos r2 double_vars with
        | r2,None -> (mk_seq r1 r2,None)
        | r2,Some pos ->
            let (r1,pos) = simple_backward pos r1 double_vars in
            (mk_seq r1 r2,pos)
        end
    | Alt (r1,r2) ->
        let pos1 = size_backward pos r1
        and pos2 = size_backward pos r2 in
        (r,if pos1=pos2 then pos1 else None)
    | Star _ -> (r,None)
    | Action _ -> assert false in
  let r = (* optimization *)
    let r,_ = simple_forward 0 r double_vars in
    let r,_ = simple_backward 0 r double_vars in
    r in
  let loc_count = ref 0 in
  let get_tag_addr t =
    try
      Hashtbl.find env t
    with Not_found ->
      let n = !loc_count in begin
        incr loc_count ;
        Hashtbl.add env t (Sum (Mem n,0)) ;
        Sum (Mem n,0)
      end in
  let rec alloc_exp pos r =
    match r with
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
    IdSet.fold
      (fun ((_,name) as x) r ->
        let v =
          if IdSet.mem x char_vars then
            Ident_char
              (IdSet.mem x optional_vars, get_tag_addr (name,true))
          else
            Ident_string
              (IdSet.mem x optional_vars,
               get_tag_addr (name,true),
               get_tag_addr (name,false)) in
        (x,v)::r)
      all_vars [] in
  (m,r, !loc_count)




let encode_casedef (casedef:(concrete_regexp * 'a) list) =
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
       count + 1, max loc_ntags ntags))
    (Empty, [], 0, 0)
    casedef 

let encode_lexdef (def:' a entry list) :
    Fcset.t array * ('a Automata_def.lexer_entry * bool) list
    =
  (chars := [];
   chars_count := 0;
   let entry_list =
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
    
let encode_single_lexdef (def:'a entry) :
    Fcset.t array * ('a Automata_def.lexer_entry * bool) = 
  (chars := [];
   chars_count := 0;
   let result =
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

