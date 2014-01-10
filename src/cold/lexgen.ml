exception Memory_overflow
open Automata_def
open Lex_util
open Translate_lex
open Tag_regexp
type tag_action =  
  | SetTag of int* int
  | EraseTag of int 
type memory_action =  
  | Copy of int* int
  | Set of int 
type automata_move =  
  | Backtrack
  | Goto of int 
type automata_trans =  
  | No_remember
  | Remember of int* tag_action list 
type automata =  
  | Perform of int* tag_action list
  | Shift of automata_trans* (automata_move* memory_action list) array 
let no_action = max_int
module StateSet =
  Set.Make(struct type t = t_transition 
                  let compare = Pervasives.compare end)
module MemMap =
  Map.Make(struct type t = int 
                  let compare = Pervasives.compare end)
type 'a dfa_state = 
  {
  final: (int* ('a* int TagMap.t));
  others: ('a* int TagMap.t) MemMap.t} 
let dfa_state_empty =
  { final = (no_action, (max_int, TagMap.empty)); others = MemMap.empty }
and dfa_state_is_empty { final = (act,_); others = o } =
  (act = no_action) && (o = MemMap.empty)
module StateSetSet =
  Set.Make(struct type t = StateSet.t 
                  let compare = StateSet.compare end)
type t_equiv =  {
  tag: tag_info;
  equiv: StateSetSet.t} 
module MemKey =
  Set.Make(struct
             type t = t_equiv 
             let compare e1 e2 =
               match Pervasives.compare e1.tag e2.tag with
               | 0 -> StateSetSet.compare e1.equiv e2.equiv
               | r -> r
           end)
type dfa_key =  {
  kstate: StateSet.t;
  kmem: MemKey.t} 
let env_to_class m =
  let env1 =
    MemMap.fold
      (fun _  (tag,s)  r  ->
         try
           let ss = TagMap.find tag r in
           let r = TagMap.remove tag r in
           TagMap.add tag (StateSetSet.add s ss) r
         with
         | Not_found  ->
             TagMap.add tag (StateSetSet.add s StateSetSet.empty) r) m
      TagMap.empty in
  TagMap.fold (fun tag  equiv  r  -> MemKey.add { tag; equiv } r) env1
    MemKey.empty
let inverse_mem_map trans m r =
  TagMap.fold
    (fun tag  addr  r  ->
       try
         let (otag,s) = MemMap.find addr r in
         assert (tag = otag);
         (let r = MemMap.remove addr r in
          MemMap.add addr (tag, (StateSet.add trans s)) r)
       with
       | Not_found  ->
           MemMap.add addr (tag, (StateSet.add trans StateSet.empty)) r) m r
let inverse_mem_map_other n (_,m) r = inverse_mem_map (OnChars n) m r
let get_key { final = (act,(_,m_act)); others = o } =
  let env =
    MemMap.fold inverse_mem_map_other o
      (if act = no_action
       then MemMap.empty
       else inverse_mem_map (ToAction act) m_act MemMap.empty) in
  let state_key =
    MemMap.fold (fun n  _  r  -> StateSet.add (OnChars n) r) o
      (if act = no_action
       then StateSet.empty
       else StateSet.add (ToAction act) StateSet.empty) in
  let mem_key = env_to_class env in { kstate = state_key; kmem = mem_key }
let key_compare k1 k2 =
  match StateSet.compare k1.kstate k2.kstate with
  | 0 -> MemKey.compare k1.kmem k2.kmem
  | r -> r
module StateMap =
  Map.Make(struct type t = dfa_key 
                  let compare = key_compare end)
let state_map = ref (StateMap.empty : int StateMap.t )
let todo = Stack.create ()
let next_state_num = ref 0
let next_mem_cell = ref 0
let temp_pending = ref false
let tag_cells = Hashtbl.create 17
let state_table = Dyn_array.create dfa_state_empty
let reset_state () =
  Stack.clear todo;
  next_state_num := 0;
  (let _ = Dyn_array.to_array' state_table in ())
let reset_state_partial ntags =
  next_mem_cell := ntags;
  Hashtbl.clear tag_cells;
  temp_pending := false;
  state_map := StateMap.empty
let do_alloc_temp () = temp_pending := true; (let n = !next_mem_cell in n)
let do_alloc_cell used t =
  let available =
    try Hashtbl.find tag_cells t with | Not_found  -> Setf.Int.empty in
  try Setf.Int.choose (Setf.Int.diff available used)
  with
  | Not_found  ->
      (temp_pending := false;
       (let n = !next_mem_cell in
        if n >= 255 then raise Memory_overflow;
        Hashtbl.replace tag_cells t (Setf.Int.add n available);
        incr next_mem_cell;
        n))
let is_old_addr a = a >= 0 and is_new_addr a = a < 0
let old_in_map m r =
  TagMap.fold
    (fun _  addr  r  -> if is_old_addr addr then Setf.Int.add addr r else r)
    m r
let alloc_map used m mvs =
  TagMap.fold
    (fun tag  a  (r,mvs)  ->
       let (a,mvs) =
         if is_new_addr a
         then let a = do_alloc_cell used tag in (a, (Setf.Int.add a mvs))
         else (a, mvs) in
       ((TagMap.add tag a r), mvs)) m (TagMap.empty, mvs)
let create_new_state { final = (act,(_,m_act)); others = o } =
  let used =
    MemMap.fold (fun _  (_,m)  r  -> old_in_map m r) o
      (old_in_map m_act Setf.Int.empty) in
  let (new_m_act,mvs) = alloc_map used m_act Setf.Int.empty in
  let (new_o,mvs) =
    MemMap.fold
      (fun k  (x,m)  (r,mvs)  ->
         let (m,mvs) = alloc_map used m mvs in ((MemMap.add k (x, m) r), mvs))
      o (MemMap.empty, mvs) in
  ({ final = (act, (0, new_m_act)); others = new_o },
    (Setf.Int.fold (fun x  r  -> (Set x) :: r) mvs []))
type new_addr_gen =  {
  mutable count: int;
  mutable env: int TagMap.t} 
let create_new_addr_gen () = { count = (-1); env = TagMap.empty }
let alloc_new_addr tag r =
  try TagMap.find tag r.env
  with
  | Not_found  ->
      let a = r.count in
      (r.count <- a - 1; r.env <- TagMap.add tag a r.env; a)
let create_mem_map tags gen =
  Tags.fold (fun tag  r  -> TagMap.add tag (alloc_new_addr tag gen) r) tags
    TagMap.empty
let create_init_state pos =
  let gen = create_new_addr_gen () in
  TransSet.fold
    (fun (t,tags)  st  ->
       match t with
       | ToAction n ->
           let (on,_otags) = st.final in
           if n < on
           then { st with final = (n, (0, (create_mem_map tags gen))) }
           else st
       | OnChars n ->
           (try let _ = MemMap.find n st.others in assert false
            with
            | Not_found  ->
                {
                  st with
                  others =
                    (MemMap.add n (0, (create_mem_map tags gen)) st.others)
                })) pos dfa_state_empty
let get_map t st =
  match t with
  | ToAction _ -> let (_,(_,m)) = st.final in m
  | OnChars n -> let (_,m) = MemMap.find n st.others in m
let dest (Copy (d,_)|Set d) = d
and orig = function | Copy (_,o) -> o | Set _ -> (-1)
let sort_mvs mvs =
  let rec do_rec r mvs =
    match mvs with
    | [] -> r
    | _ ->
        let dests =
          List.fold_left (fun r  mv  -> Setf.Int.add (dest mv) r)
            Setf.Int.empty mvs in
        let (rem,here) =
          List.partition (fun mv  -> Setf.Int.mem (orig mv) dests) mvs in
        (match here with
         | [] ->
             (match rem with
              | (Copy (d,_))::_ ->
                  let d' = do_alloc_temp () in (Copy (d', d)) ::
                    (do_rec r
                       (List.map
                          (fun mv  ->
                             if (orig mv) = d
                             then Copy ((dest mv), d')
                             else mv) rem))
              | _ -> assert false)
         | _ -> do_rec (here @ r) rem) in
  do_rec [] mvs
let move_to mem_key src tgt =
  let mvs =
    MemKey.fold
      (fun { tag; equiv = m }  r  ->
         StateSetSet.fold
           (fun s  r  ->
              try
                let t = StateSet.choose s in
                let src = TagMap.find tag (get_map t src)
                and tgt = TagMap.find tag (get_map t tgt) in
                if src <> tgt
                then
                  (if is_new_addr src
                   then (Set tgt) :: r
                   else (Copy (tgt, src)) :: r)
                else r
              with | Not_found  -> assert false) m r) mem_key [] in
  sort_mvs mvs
let get_state st =
  let key = get_key st in
  try
    let num = StateMap.find key (!state_map) in
    (num, (move_to key.kmem st (Dyn_array.get state_table num)))
  with
  | Not_found  ->
      let num = !next_state_num in
      (incr next_state_num;
       (let (st,mvs) = create_new_state st in
        Dyn_array.add state_table st;
        state_map := (StateMap.add key num (!state_map));
        Stack.push (st, num) todo;
        (num, mvs)))
let map_on_all_states f old_res =
  let res = ref old_res in
  (try
     while true do
       let (st,i) = Stack.pop todo in
       let r = f st in res := ((r, i) :: (!res)) done
   with | Stack.Empty  -> ());
  !res
let goto_state st =
  if dfa_state_is_empty st
  then (Backtrack, [])
  else (let (n,moves) = get_state st in ((Goto n), moves))
let add_tags_to_map gen tags m =
  Tags.fold
    (fun tag  m  ->
       let m = TagMap.remove tag m in
       TagMap.add tag (alloc_new_addr tag gen) m) tags m
let apply_transition gen r pri m =
  function
  | (ToAction n,tags) ->
      let (on,(opri,_)) = r.final in
      if (n < on) || ((on = n) && (pri < opri))
      then
        let m = add_tags_to_map gen tags m in
        { r with final = (n, (pri, m)) }
      else r
  | (OnChars n,tags) ->
      (try
         let (opri,_) = MemMap.find n r.others in
         if pri < opri
         then
           let m = add_tags_to_map gen tags m in
           {
             r with
             others = (MemMap.add n (pri, m) (MemMap.remove n r.others))
           }
         else r
       with
       | Not_found  ->
           let m = add_tags_to_map gen tags m in
           { r with others = (MemMap.add n (pri, m) r.others) })
let apply_transitions gen r pri m ts =
  TransSet.fold (fun t  r  -> apply_transition gen r pri m t) ts r
let rec split_env gen follow pos m s =
  function
  | [] -> []
  | ((s1,st1) as p)::rem ->
      let here = Cset.inter s s1 in
      if Cset.is_empty here
      then p :: (split_env gen follow pos m s rem)
      else
        (let rest = Cset.diff s here in
         let rem =
           if Cset.is_empty rest
           then rem
           else split_env gen follow pos m rest rem
         and new_st = apply_transitions gen st1 pos m follow in
         let stay = Cset.diff s1 here in
         if Cset.is_empty stay
         then (here, new_st) :: rem
         else (stay, st1) :: (here, new_st) :: rem)
let comp_shift gen chars follow st =
  MemMap.fold
    (fun pos  (_,m)  env  ->
       split_env gen (follow.(pos)) pos m (chars.(pos)) env) st
    [(Cset.all_chars_eof, dfa_state_empty)]
let reachs chars follow st =
  let gen = create_new_addr_gen () in
  let env = comp_shift gen chars follow st in
  let env = List.map (fun (s,dfa_state)  -> (s, (goto_state dfa_state))) env in
  let shift = Cset.env_to_array env in shift
let get_tag_mem n env t =
  try TagMap.find t (env.(n)) with | Not_found  -> assert false
let do_tag_actions n env m =
  let (used,r) =
    TagMap.fold
      (fun t  m  (used,r)  ->
         let a = get_tag_mem n env t in
         ((Setf.Int.add a used), ((SetTag (a, m)) :: r))) m
      (Setf.Int.empty, []) in
  let (_,r) =
    TagMap.fold
      (fun tag  m  (used,r)  ->
         if (not (Setf.Int.mem m used)) && tag.start
         then ((Setf.Int.add m used), ((EraseTag m) :: r))
         else (used, r)) (env.(n)) (used, r) in
  r
let translate_state shortest_match tags chars follow st =
  let (n,(_,m)) = st.final in
  if MemMap.empty = st.others
  then Perform (n, (do_tag_actions n tags m))
  else
    if shortest_match
    then
      (if n = no_action
       then Shift (No_remember, (reachs chars follow st.others))
       else Perform (n, (do_tag_actions n tags m)))
    else
      Shift
        ((if n = no_action
          then No_remember
          else Remember (n, (do_tag_actions n tags m))),
          (reachs chars follow st.others))
let make_tag_entry id start act a r =
  match a with
  | (Mem m,0) -> TagMap.add { id; start; action = act } m r
  | _ -> r
let extract_tags (l : (int* ((Locf.t* string)* ident_info) list* 'b) list) =
  (let envs = Array.create (List.length l) TagMap.empty in
   List.iter
     (fun (act,m,_)  ->
        envs.(act) <-
        List.fold_right
          (fun (x,v)  r  ->
             let name = snd x in
             match v with
             | Ident_char (_,t) -> make_tag_entry name true act t r
             | Ident_string (_,t1,t2) ->
                 make_tag_entry name true act t1
                   (make_tag_entry name false act t2 r)) m TagMap.empty) l;
   envs : int TagMap.t array )
type 'a automata_entry = 
  {
  auto_mem_size: int;
  auto_initial_state: (int* memory_action list);
  auto_actions: (int* t_env* 'a) list} 
let make_single_dfa (lexdef : 'a entry) =
  (let (chars,entry) = encode_single_lexdef lexdef in
   let follow = followpos (Array.length chars) [entry] in
   let _ = reset_state () in
   let r_states = ref [] in
   let initial_states =
     match entry with
     | (le,shortest) ->
         let tags = extract_tags le.lex_actions in
         (reset_state_partial le.lex_mem_tags;
          (let init_num =
             get_state (create_init_state (firstpos le.lex_regexp)) in
           r_states :=
             (map_on_all_states (translate_state shortest tags chars follow)
                (!r_states));
           {
             auto_mem_size =
               (if !temp_pending
                then (!next_mem_cell) + 1
                else !next_mem_cell);
             auto_initial_state = init_num;
             auto_actions = (le.lex_actions)
           })) in
   let states = !r_states in
   let actions = Array.create (!next_state_num) (Perform (0, [])) in
   List.iter (fun (act,i)  -> actions.(i) <- act) states;
   reset_state ();
   reset_state_partial 0;
   (initial_states, actions) : ('a automata_entry* automata array) )
let make_dfa (lexdef : 'a entry list) =
  (let (chars,entry_list) = encode_lexdef lexdef in
   let follow = followpos (Array.length chars) entry_list in
   let _ = reset_state () in
   let r_states = ref [] in
   let initial_states =
     List.map
       (fun (le,shortest)  ->
          let tags = extract_tags le.lex_actions in
          reset_state_partial le.lex_mem_tags;
          (let pos_set = firstpos le.lex_regexp in
           let init_state = create_init_state pos_set in
           let init_num = get_state init_state in
           r_states :=
             (map_on_all_states (translate_state shortest tags chars follow)
                (!r_states));
           {
             auto_mem_size =
               (if !temp_pending
                then (!next_mem_cell) + 1
                else !next_mem_cell);
             auto_initial_state = init_num;
             auto_actions = (le.lex_actions)
           })) entry_list in
   let states = !r_states in
   let actions = Array.create (!next_state_num) (Perform (0, [])) in
   List.iter (fun (act,i)  -> actions.(i) <- act) states;
   reset_state ();
   reset_state_partial 0;
   (initial_states, actions) : ('a automata_entry list* automata array) )
