type t = (int* int) list 
let meta_t _loc eta__002_ =
  (let mklist loc =
     let rec loop top =
       function
       | [] -> `Uid (loc, "[]")
       | e1::el ->
           let _loc = if top then loc else Locf.merge (Ast_gen.loc_of e1) loc in
           `App
             (_loc, (`Uid (_loc, "::")),
               (`Par (_loc, (`Com (_loc, e1, (loop false el)))))) in
     loop true in
   let meta_list mf_a _loc ls =
     mklist _loc (List.map (fun x  -> mf_a _loc x) ls) in
   meta_list)
    (fun _loc  (_a0,_a1)  ->
       `Par
         (_loc,
           (`Com
              (_loc,
                ((fun _loc  (i : int)  ->
                    (`Int (_loc, (string_of_int i)) :>Astf.ep)) _loc _a0),
                ((fun _loc  (i : int)  ->
                    (`Int (_loc, (string_of_int i)) :>Astf.ep)) _loc _a1)))))
    _loc eta__002_
let empty = []
let is_empty = function | [] -> true | _ -> false
let singleton c = [(c, c)]
let interval c1 c2 = if c1 <= c2 then [(c1, c2)] else [(c2, c1)]
let rec union s1 s2 =
  match (s1, s2) with
  | ([],_) -> s2
  | (_,[]) -> s1
  | (((c1,d1) as p1)::r1,(c2,d2)::r2) ->
      if c1 > c2
      then union s2 s1
      else
        if (d1 + 1) < c2
        then p1 :: (union r1 s2)
        else if d1 < d2 then union ((c1, d2) :: r2) r1 else union s1 r2
let rec inter l l' =
  match (l, l') with
  | (_,[]) -> []
  | ([],_) -> []
  | ((c1,c2)::r,(c1',c2')::r') ->
      if c2 < c1'
      then inter r l'
      else
        if c2' < c1
        then inter l r'
        else
          if c2 < c2'
          then ((max c1 c1'), c2) :: (inter r l')
          else ((max c1 c1'), c2') :: (inter l r')
let rec diff l l' =
  match (l, l') with
  | (_,[]) -> l
  | ([],_) -> []
  | ((c1,c2)::r,(c1',c2')::r') ->
      if c2 < c1'
      then (c1, c2) :: (diff r l')
      else
        if c2' < c1
        then diff l r'
        else
          (let r'' = if c2' < c2 then ((c2' + 1), c2) :: r else r in
           if c1 < c1' then (c1, (c1' - 1)) :: (diff r'' r') else diff r'' r')
let eof = singleton 256
let all_chars = interval 0 255
let all_chars_eof = interval 0 256
let complement s = diff all_chars s
let env_to_array env =
  match env with
  | [] -> assert false
  | (_,x)::rem ->
      let res = Array.create 257 x in
      (List.iter
         (fun (c,y)  ->
            List.iter (fun (i,j)  -> for k = i to j do res.(k) <- y done) c)
         rem;
       res)
let to_string (set : t) =
  let x =
    (String.concat ";") @@
      (List.map (fun (x,y)  -> Printf.sprintf "%d,%d" x y) set) in
  "[ " ^ (x ^ " ]")
