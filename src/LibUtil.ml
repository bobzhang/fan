include Format;

module Map_Make (S : Map.OrderedType) =
  struct
    include (Map.Make S);
    let of_list lst =
      List.fold_left (fun acc (k, v) -> add k v acc) empty lst;
    let of_hashtbl tbl =
      Hashtbl.fold (fun k v acc -> add k v acc) tbl empty;
    let elements map = fold (fun k v acc -> [ (k, v) :: acc ]) map [];
  end;
module StringMap = Map_Make String;
module IntMap =
  Map_Make (struct type t = int; let compare = Pervasives.compare; end);
module StringSet = struct include (Set.Make String); end;
module IntSet =
  Set.Make (struct type t = int; let compare = Pervasives.compare; end);
module Hashset =
  struct
    type t 'a = Hashtbl.t 'a unit;
    let create = Hashtbl.create;
    let add set x = Hashtbl.replace set x ();
    let remove = Hashtbl.remove;
    let mem = Hashtbl.mem;
    let iter f = Hashtbl.iter (fun v () -> f v);
    let fold f = Hashtbl.fold (fun v () st -> f v st);
    let elements = Hashtbl.length;
    let clear = Hashtbl.clear;
    let of_list size vs =
      let set = create size in (List.iter (add set) vs; set);
    let to_list set = fold (fun x y -> [ x :: y ]) set [];
  end;
(*
  let (module M) = mk_set (compare:int->int->int) in
  M.iter print_int M.empty;     *)
let mk_set (type s) ~cmp =
  let module M = struct type t = s; let compare = cmp; end
  in (module Set.Make M : Set.S with type elt = s);
let mk_map (type s) ~cmp =
  let module M = struct type t = s; let compare = cmp; end
  in (module Map.Make M : Map.S with type key = s);
let mk_hashtbl (type s) ~eq ~hash =
  let module M = struct type t = s; let equal = eq; let hash = hash; end
  in (module Hashtbl.Make M : Hashtbl.S with type key = s);
let ( |> ) x f = f x;
let ( /> ) x f = f x;
let ( & ) f x = f x;
let ( |- ) f g x = g (f x);
let ( <| ) f x = f x;
let ( -| ) f g x = f (g x);
let ( *** ) f g (x, y) = ((f x), (g y));
let ( &&& ) f g x = ((f x), (g x));
let flip f x y = f y x;
let curry f x y = f (x, y);
let uncurry f (x, y) = f x y;
let failwithf fmt = ksprintf failwith fmt;
let prerr_endlinef fmt = ksprintf prerr_endline fmt;
let const x _ = x;
let tap f x = (f x; x);
let is_even x = (x mod 2) == 0;
(*
  {[
  to_string_of_printer pp_print_int 32;
  "32"]}
 *)
let to_string_of_printer printer v =
  let buf = Buffer.create 30 in
  let () = Format.bprintf buf "@[%a@]" printer v in Buffer.contents buf;
(*
  closed interval 
  {[
  nfold_left ~until:3 ~acc:0 (fun acc i -> acc + i);
  int = 6
  ]}
 *)
let nfold_left ?(start = 0) ~until ~acc f =
  let v = ref acc
  in (for x = start to until do v := f !v x done; !v);
(* a module to abstract exeption mechanism  *)
(* we put the return let exn, so we don't need to work around type system later *)
type cont 'a = 'a -> exn;
let callcc (type u) (f : cont u -> u) =
  let module M = struct exception Return of u; end
  in try f (fun x -> raise (M.Return x)) with [ M.Return u -> u ];
(*

 *)
module List =
  struct
    include List;
    (* a mutable list whose runtime is the same as function list  *)
    type mut_list 'a = { hd : 'a; tl : mutable list 'a };
    external inj : mut_list 'a -> list 'a = "%identity";
    let dummy_node () = { hd = Obj.magic (); tl = []; };
    let cons h t = [ h :: t ];
    let is_empty = fun [ [] -> True | _ -> False ];
    (*$T is_empty
  is_empty []
  not (is_empty [1])
 *)
    (* tail recursive list *)
    let nth l index =
      (if index < 0 then invalid_arg "Negative index not allowed" else ();
       let rec loop n =
         fun
         [ [] -> invalid_arg "Index past end of list"
         | [ h :: t ] -> if n = 0 then h else loop (n - 1) t ];
       loop index l);
    let at = nth;
    (*$T at
  try ignore (at [] 0); false with Invalid_argument _ -> true
  try ignore (at [1;2;3] (-1)); false with Invalid_argument _ -> true
  at [1;2;3] 2 = 3
 *)
    (* save one pass compared with Stdlib's append *)
    let append l1 l2 =
      match l1 with
      [ [] -> l2
      | [ h :: t ] ->
          let rec loop dst =
            fun
            [ [] -> dst.tl <- l2
            | [ h :: t ] ->
                let cell = { hd = h; tl = []; }
                in (dst.tl <- inj cell; loop cell t) ] in
          let r = { hd = h; tl = []; } in (loop r t; inj r) ];
    let  flatten l =
      let rec inner dst =
        fun
        [ [] -> dst
        | [ h :: t ] ->
            let r = { hd = h; tl = []; } in (dst.tl <- inj r; inner r t) ] in
      let rec outer dst =
        fun [ [] -> () | [ h :: t ] -> outer (inner dst h) t ] in
      let r = dummy_node () in (outer r l; r.tl);
    let concat = flatten;
    (*$T flatten
  flatten [[1;2];[3];[];[4;5;6]] = [1;2;3;4;5;6]
  flatten [[]] = []
 *)
    let map f =
      fun
      [ [] -> []
      | [ h :: t ] ->
          let rec loop dst =
            fun
            [ [] -> ()
            | [ h :: t ] ->
                let r = { hd = f h; tl = []; } in (dst.tl <- inj r; loop r t) ] in
          let r = { hd = f h; tl = []; } in (loop r t; inj r) ];
    let take n l =
      let rec loop n dst =
        fun
        [ [ h :: t ] when n > 0 ->
            let r = { hd = h; tl = []; }
            in (dst.tl <- inj r; loop (n - 1) r t)
        | _ -> () ] in
      let dummy = dummy_node () in (loop n dummy l; dummy.tl);
    (*$= take & ~printer:(IO.to_string (List.print Int.print))
  (take 0 [1;2;3]) []
  (take 3 [1;2;3]) [1;2;3]
  (take 4 [1;2;3]) [1;2;3]
  (take 1 [1;2;3]) [1]
 *)
    let take_while p li =
      let rec loop dst =
        fun
        [ [] -> ()
        | [ x :: xs ] ->
            if p x
            then let r = { hd = x; tl = []; } in (dst.tl <- inj r; loop r xs)
            else () ] in
      let dummy = dummy_node () in (loop dummy li; dummy.tl);
    (*$= take_while & ~printer:(IO.to_string (List.print Int.print))
  (take_while ((=) 3) [3;3;4;3;3]) [3;3]
  (take_while ((=) 3) [3]) [3]
  (take_while ((=) 3) [4]) []
  (take_while ((=) 3) []) []
  (take_while ((=) 2) [2; 2]) [2; 2]
 *)
    let rec drop_while f =
      fun [ [] -> [] | [ x :: xs ] when f x -> drop_while f xs | xs -> xs ];
    (*$= drop_while & ~printer:(IO.to_string (List.print Int.print))
  (drop_while ((=) 3) [3;3;4;3;3]) [4;3;3]
  (drop_while ((=) 3) [3]) []
 *)
    let takewhile = take_while;
    let dropwhile = drop_while;
    let interleave ?first ?last (sep : 'a) (l : list 'a) =
      let rec aux acc =
        fun [ [] -> acc | [ h :: t ] -> aux [ h; sep :: acc ] t ]
      in
        match (l, first, last) with
        [ ([], None, None) -> []
        | ([], None, Some x) -> [ x ]
        | ([], Some x, None) -> [ x ]
        | ([], Some x, Some y) -> [ x; y ]
        | ([ h ], None, None) -> [ h ]
        | ([ h ], None, Some x) -> [ h; x ]
        | ([ h ], Some x, None) -> [ x; h ]
        | ([ h ], Some x, Some y) -> [ x; h; y ]
        | ([ h :: t ], None, None) -> rev (aux [ h ] t)
        | ([ h :: t ], Some x, None) -> [ x :: rev (aux [ h ] t) ]
        | ([ h :: t ], None, Some y) -> rev_append (aux [ h ] t) [ y ]
        | ([ h :: t ], Some x, Some y) ->
            [ x :: rev_append (aux [ h ] t) [ y ] ] ];
    (*$= interleave & ~printer:(IO.to_string (List.print Int.print))
  (interleave 0 [1;2;3]) [1;0;2;0;3]
  (interleave 0 [1]) [1]
  (interleave 0 []) []
  (interleave ~first:(-1) 0 [1;2;3]) [-1;1;0;2;0;3]
  (interleave ~first:(-1) 0 [1]) [-1;1]
  (interleave ~first:(-1) 0 []) [-1]
  (interleave ~last:(-2) 0 [1;2;3]) [1;0;2;0;3;-2]
  (interleave ~last:(-2) 0 [1]) [1;-2]
  (interleave ~last:(-2) 0 []) [-2]
  (interleave ~first:(-1) ~last:(-2) 0 [1;2;3]) [-1;1;0;2;0;3;-2]
  (interleave ~first:(-1) ~last:(-2) 0 [1]) [-1;1;-2]
  (interleave ~first:(-1) ~last:(-2) 0 []) [-1;-2]
 *)
    (*  unique [1;3;3;3;2];
                                             [1; 3; 2] *)
    let unique (type et) ?(hash = Hashtbl.hash) ?(eq = ( = )) (l : list et)
                 =
      let module HT =
        Hashtbl.Make
          (struct type t = et; let equal = eq; let hash = hash; end)  in
        let ht = HT.create (List.length l) in
        let rec loop dst =
          fun
          [ [ h :: t ] when not (HT.mem ht h) ->
              (HT.add ht h ();
               let (* put h in hash table *) r = { hd = h; tl = []; };
               (* and to output list *)
               dst.tl <- inj r;
               loop r t)
          | [ _ :: t ] ->
              (* if already in hashtable then don't add to output list *)
              loop dst t
          | [] -> () ] in
        let dummy = dummy_node () in (loop dummy l; dummy.tl);
    (*$= unique & ~printer:(IO.to_string (List.print Int.print))
  [1;2;3;4;5;6] (unique_hash [1;1;2;2;3;3;4;5;6;4;5;6])
  [1] (unique_hash [1;1;1;1;1;1;1;1;1;1])
  [2;3] (unique_hash ~hash:(fun x -> Hashtbl.hash (x land 1)) ~eq:(fun x y -> x land 1 = y land 1) [2;2;2;4;6;8;3;1;2])
 *)
    let filter_map f l =
      let rec loop dst =
        fun
        [ [] -> ()
        | [ h :: t ] ->
            match f h with
            [ None -> loop dst t
            | Some x ->
                let r = { hd = x; tl = []; } in (dst.tl <- inj r; loop r t) ] ] in
      let dummy = dummy_node () in (loop dummy l; dummy.tl);
    let rec find_map f =
      fun
      [ [] -> None
      | [ x :: xs ] ->
          match f x with [ (Some _ as y) -> y | None -> find_map f xs ] ];
    let fold_right_max = 1000;
    (* a constant for switching tail-recursion or not *)
    let fold_right f l init =
      let rec tail_loop acc =
        fun [ [] -> acc | [ h :: t ] -> tail_loop (f h acc) t ] in
      let rec loop n =
        fun
        [ [] -> init
        | [ h :: t ] ->
            if n < fold_right_max
            then f h (loop (n + 1) t)
            else f h (tail_loop init (rev t)) ]
      in loop 0 l;
    let map2 f l1 l2 =
      let rec loop dst src1 src2 =
        match (src1, src2) with
        [ ([], []) -> ()
        | ([ h1 :: t1 ], [ h2 :: t2 ]) ->
            let r = { hd = f h1 h2; tl = []; }
            in (dst.tl <- inj r; loop r t1 t2)
        | _ -> invalid_arg "map2: Different_list_size" ] in
      let dummy = dummy_node () in (loop dummy l1 l2; dummy.tl);
    let rec iter2 f l1 l2 =
      match (l1, l2) with
      [ ([], []) -> ()
      | ([ h1 :: t1 ], [ h2 :: t2 ]) -> (f h1 h2; iter2 f t1 t2)
      | _ -> invalid_arg "iter2: Different_list_size" ];
    let rec fold_left2 f accum l1 l2 =
      match (l1, l2) with
      [ ([], []) -> accum
      | ([ h1 :: t1 ], [ h2 :: t2 ]) -> fold_left2 f (f accum h1 h2) t1 t2
      | _ -> invalid_arg "fold_left2: Different_list_size" ];
    let fold_right2 f l1 l2 init =
      let rec tail_loop acc l1 l2 =
        match (l1, l2) with
        [ ([], []) -> acc
        | ([ h1 :: t1 ], [ h2 :: t2 ]) -> tail_loop (f h1 h2 acc) t1 t2
        | _ -> invalid_arg "fold_left2: Different_list_size" ] in
      let rec loop n l1 l2 =
        match (l1, l2) with
        [ ([], []) -> init
        | ([ h1 :: t1 ], [ h2 :: t2 ]) ->
            if n < fold_right_max
            then f h1 h2 (loop (n + 1) t1 t2)
            else f h1 h2 (tail_loop init (rev t1) (rev t2))
        | _ -> invalid_arg "fold_right2: Different_list_size" ]
      in loop 0 l1 l2;
    let for_all2 p l1 l2 =
      let rec loop l1 l2 =
        match (l1, l2) with
        [ ([], []) -> True
        | ([ h1 :: t1 ], [ h2 :: t2 ]) ->
            if p h1 h2 then loop t1 t2 else False
        | _ -> invalid_arg "for_all2: Different_list_size" ]
      in loop l1 l2;
    let exists2 p l1 l2 =
      let rec loop l1 l2 =
        match (l1, l2) with
        [ ([], []) -> False
        | ([ h1 :: t1 ], [ h2 :: t2 ]) ->
            if p h1 h2 then True else loop t1 t2
        | _ -> invalid_arg "exists2: Different_list_size" ]
      in loop l1 l2;
    (* equality using [=] *)
    let remove_assoc x lst =
      let rec loop dst =
        fun
        [ [] -> ()
        | [ ((a, _) as pair) :: t ] ->
            if a = x
            then dst.tl <- t
            else
              let r = { hd = pair; tl = []; } in (dst.tl <- inj r; loop r t) ] in
      let dummy = dummy_node () in (loop dummy lst; dummy.tl);
    (* equality using [==] *)
    let remove_assq x lst =
      let rec loop dst =
        fun
        [ [] -> ()
        | [ ((a, _) as pair) :: t ] ->
            if a == x
            then dst.tl <- t
            else
              let r = { hd = pair; tl = []; } in (dst.tl <- inj r; loop r t) ] in
      let dummy = dummy_node () in (loop dummy lst; dummy.tl);
    (* rfind (fun x -> x>3) [1;2;3;9;3;4;2;3];
 * - : int = 4 *)
    let rfind p l = find p (rev l);
    let find_all p l =
      let rec findnext dst =
        fun
        [ [] -> ()
        | [ h :: t ] ->
            if p h
            then
              let r = { hd = h; tl = []; } in (dst.tl <- inj r; findnext r t)
            else findnext dst t ] in
      let dummy = dummy_node () in (findnext dummy l; dummy.tl);
    let findi p l =
      let rec loop n =
        fun
        [ [] -> raise Not_found
        | [ h :: t ] -> if p n h then (n, h) else loop (n + 1) t ]
      in loop 0 l;
    let index_of e l =
      let rec loop n =
        fun
        [ [] -> None
        | [ h :: _ ] when h = e -> Some n
        | [ _ :: t ] -> loop (n + 1) t ]
      in loop 0 l;
    (* using [==]*)
    let index_ofq e l =
      let rec loop n =
        fun
        [ [] -> None
        | [ h :: _ ] when h == e -> Some n
        | [ _ :: t ] -> loop (n + 1) t ]
      in loop 0 l;
    let rindex_of e l =
      let rec loop n acc =
        fun
        [ [] -> acc
        | [ h :: t ] when h = e -> loop (n + 1) (Some n) t
        | [ _ :: t ] -> loop (n + 1) acc t ]
      in loop 0 None l;
    let rindex_ofq e l =
      let rec loop n acc =
        fun
        [ [] -> acc
        | [ h :: t ] when h == e -> loop (n + 1) (Some n) t
        | [ _ :: t ] -> loop (n + 1) acc t ]
      in loop 0 None l;
    let filter = find_all;
    let partition p lst =
      let rec loop yesdst nodst =
        fun
        [ [] -> ()
        | [ h :: t ] ->
            let r = { hd = h; tl = []; }
            in
              if p h
              then (yesdst.tl <- inj r; loop r nodst t)
              else (nodst.tl <- inj r; loop yesdst r t) ] in
      let yesdummy = dummy_node ()
      and nodummy = dummy_node ()
      in (loop yesdummy nodummy lst; ((yesdummy.tl), (nodummy.tl)));
    let split lst =
      let rec loop adst bdst =
        fun
        [ [] -> ()
        | [ (a, b) :: t ] ->
            let x = { hd = a; tl = []; }
            and y = { hd = b; tl = []; }
            in (adst.tl <- inj x; bdst.tl <- inj y; loop x y t) ] in
      let adummy = dummy_node ()
      and bdummy = dummy_node ()
      in (loop adummy bdummy lst; ((adummy.tl), (bdummy.tl)));
    let combine l1 l2 =
      let rec loop dst l1 l2 =
        match (l1, l2) with
        [ ([], []) -> ()
        | ([ h1 :: t1 ], [ h2 :: t2 ]) ->
            let r = { hd = (h1, h2); tl = []; }
            in (dst.tl <- inj r; loop r t1 t2)
        | (_, _) -> invalid_arg "combine: Different_list_size" ] in
      let dummy = dummy_node () in (loop dummy l1 l2; dummy.tl);
    let make i x =
      (if i < 0 then invalid_arg "List.make" else ();
       let rec loop x acc =
         fun [ 0 -> acc | i -> loop x [ x :: acc ] (i - 1) ];
       loop x [] i);
    let mapi f =
      fun
      [ [] -> []
      | [ h :: t ] ->
          let rec loop dst n =
            fun
            [ [] -> ()
            | [ h :: t ] ->
                let r = { hd = f n h; tl = []; }
                in (dst.tl <- inj r; loop r (n + 1) t) ] in
          let r = { hd = f 0 h; tl = []; } in (loop r 1 t; inj r) ];
    let iteri f l =
      let rec loop n =
        fun [ [] -> () | [ h :: t ] -> (f n h; loop (n + 1) t) ]
      in loop 0 l;
    let first = hd;
    let rec last =
      fun
      [ [] -> invalid_arg "Empty List"
      | [ h ] -> h
      | [ _ :: t ] -> last t ];
    let split_nth index =
      fun
      [ [] ->
          if index = 0
          then ([], [])
          else invalid_arg "Index past end of list"
      | ([ h :: t ] as l) ->
          if index = 0
          then ([], l)
          else
            if index < 0
            then invalid_arg "Negative index not allowed"
            else
              let rec loop n dst l =
                if n = 0
                then l
                else
                  match l with
                  [ [] -> invalid_arg "Index past end of list"
                  | [ h :: t ] ->
                      let r = { hd = h; tl = []; }
                      in (dst.tl <- inj r; loop (n - 1) r t) ] in
              let r = { hd = h; tl = []; }
              in ((inj r), (loop (index - 1) r t)) ];
    let split_at = split_nth;
    let find_exn f e l = try find f l with [ Not_found -> raise e ];
    (* using [=]*)
    let remove l x =
      let rec loop dst =
        fun
        [ [] -> ()
        | [ h :: t ] ->
            if x = h
            then dst.tl <- t
            else let r = { hd = h; tl = []; } in (dst.tl <- inj r; loop r t) ] in
      let dummy = dummy_node () in (loop dummy l; dummy.tl);
    let remove_if f lst =
      let rec loop dst =
        fun
        [ [] -> ()
        | [ x :: l ] ->
            if f x
            then dst.tl <- l
            else let r = { hd = x; tl = []; } in (dst.tl <- inj r; loop r l) ] in
      let dummy = dummy_node () in (loop dummy lst; dummy.tl);
    let remove_all l x =
      let rec loop dst =
        fun
        [ [] -> ()
        | [ h :: t ] ->
            if x = h
            then loop dst t
            else let r = { hd = h; tl = []; } in (dst.tl <- inj r; loop r t) ] in
      let dummy = dummy_node () in (loop dummy l; dummy.tl);
    (* will raise an exception if it's not an rectangle *)
    let transpose =
      fun
      [ [] -> []
      | [ x ] -> List.map (fun x -> [ x ]) x
      | [ x :: xs ] ->
          let heads = List.map (fun x -> { hd = x; tl = []; }) x in
          let _list =
            List.fold_left
              (fun acc x ->
                 List.map2
                   (fun x xs ->
                      let r = { hd = x; tl = []; } in (xs.tl <- inj r; r))
                   x acc)
              heads xs
          in Obj.magic heads ];
    (* equivalent to List.map inj heads, but without creating a new list *)
    (*$T transpose
  transpose [ [1; 2; 3;]; [4; 5; 6;]; [7; 8; 9;] ] = [[1;4;7];[2;5;8];[3;6;9]]
  transpose [] = []
  transpose [ [1] ] = [ [1] ]
 *)
    (* assoc via let using [=]*)
    let assoc_inv e l =
      let rec aux =
        fun
        [ [] -> raise Not_found
        | [ (a, b) :: _ ] when b = e -> a
        | [ _ :: t ] -> aux t ]
      in aux l;
    (* assoc via let using [==]*)
    let assq_inv e l =
      let rec aux =
        fun
        [ [] -> raise Not_found
        | [ (a, b) :: _ ] when b == e -> a
        | [ _ :: t ] -> aux t ]
      in aux l;
    (* sort_unique (compare) [1;2;3;2;3;2;1;2;3;434;2;23;2;4234;23];
 * list int = [1; 2; 3; 23; 434; 4234] *)
    let sort_unique (type s) (cmp : s -> s -> int) lst =
      let (module M) = mk_set ~cmp
      in let open M in elements & (fold_left (flip add) empty lst);
    (* group compare [1;2;2;3;2;32;32;23;2];
 * - : list (list int) = [[1]; [2; 2; 2; 2]; [3]; [23]; [32; 32]]       *)
    let group cmp lst =
      let sorted = List.sort cmp lst in
      let fold first rest =
        List.fold_left
          (fun (acc, agr, last) elem ->
             if (cmp last elem) = 0
             then (acc, [ elem :: agr ], elem)
             else ([ agr :: acc ], [ elem ], elem))
          ([], [ first ], first) rest
      in
        match sorted with
        [ [] -> []
        | [ hd :: tl ] ->
            let (groups, lastgr, _) = fold hd tl
            in List.rev_map List.rev [ lastgr :: groups ] ];
    let cross_product l1 l2 =
      List.concat (List.map (fun i -> List.map (fun j -> (i, j)) l2) l1);
    (*$T cartesian_product as cp
  cp [1;2;3] ['x';'y'] = [1,'x';1,'y';2,'x';2,'y';3,'x';3,'y']
 *)
    let rec ncross_product =
      fun
      [ [] -> assert False
      | [ l ] -> List.map (fun i -> [ i ]) l
      | [ h :: t ] ->
          let rest = ncross_product t
          in
            List.concat
              (List.map (fun i -> List.map (fun r -> [ i :: r ]) rest) h) ];
    (* f will be incremented for each item ndexed from 0
   {[
   fold_lefti
   (fun i a b -> (print_int i; print_newline (); a+b)) 2 [1;2;3;4];
   0
   1
   2
   3
   (4, 12)
   ]}
 *)
    let fold_lefti f init ls =
      fold_left (fun (i, acc) x -> ((i + 1), (f i acc x))) (0, init) ls;
    let reduce_left_with ~compose ~map lst =
      match lst with
      [ [] -> invalid_arg "reduce_left length zero"
      | [ x :: xs ] ->
          let rec loop x xs =
            match xs with
            [ [] -> x
            | [ y :: ys ] -> loop (compose x (map y)) ys ]
          in loop (map x) xs ];
    let reduce_left compose = reduce_left_with ~compose ~map: (fun x -> x);
    let reduce_right_with ~compose ~map lst =
      match lst with
      [ [] -> invalid_arg "reduce_right length zero"
      | xs ->
          let rec loop xs =
            match xs with
            [ [] -> assert False
            | [ y ] -> map y
            | [ y :: ys ] -> compose (map y) (loop ys) ]
          in loop xs ];
    let reduce_right compose =
      reduce_right_with ~compose ~map: (fun x -> x);
    (* do sequence does not support try with
         use do only for simple construct
       *)
    let find_first f lst = let module M = struct exception First; end
      in
        let res = ref None
        in
          try
            (List.iter
               (fun x ->
                  match f x with
                  [ Some v -> (res:= Some v; raise M.First)
                  | None -> () ])
               lst;
             None)
          with [ M.First -> !res ];
    let rec filter_map f =
      fun
      [ [] -> []
      | [ x :: xs ] ->
          match f x with
          [ Some v -> [ v :: filter_map f xs ]
          | None -> filter_map f xs ] ];
    (* find the first function which return some let *)
    let find_first_result preds v =
      match find_first (fun pred -> pred v) preds with
      [ None -> failwith "find_first_result"
      | Some r -> r ];
    let init n f = let open Array
      in if n < 0 then invalid_arg "List.init <0" else (init n f) |> to_list;
    (*
        {[
        drop 3 [1;2;3;4];

        list int = [4]
        ]}
       *)
    let rec drop n =
      fun [ [ _ :: l ] when n > 0 -> drop (n - 1) l | l -> l ];
    (*
            {[
            [1;2;3;4;5]
            ([4;3;2;1], 5 )
            ]}
           *)
    let lastbut1 ls =
      match ls with
      [ [] -> failwith "lastbut1 empty"
      | _ -> let l = List.rev ls in ((List.tl l), (List.hd l)) ];
    (* range_up 20 ~step:3 ;;
 * - : int list = [0; 3; 6; 9; 12; 15; 18] *)
    let range_up ?(from = 0) ?(step = 1) until =
      let rec loop dst i =
        if i > until
        then ()
        else
          let cell = { hd = i; tl = []; }
          in (dst.tl <- inj cell; loop cell (i + step))
      in
        if from > until
        then []
        else
          let res = { hd = from; tl = []; }
          in (loop res (from + step); inj res);
    (* range_down 20 ~step:3 ;;
 * - : int list = [20; 17; 14; 11; 8; 5; 2] *)
    let range_down ?(until = 0) ?(step = 1) from =
      let rec loop dst i =
        if i < until
        then ()
        else
          let cell = { hd = i; tl = []; }
          in (dst.tl <- inj cell; loop cell (i - step))
      in
        if from < until
        then []
        else
          let res = { hd = from; tl = []; }
          in (loop res (from - step); inj res);
    (* range_up 10 // is_even /@ (fun x -> x + 1);;
 * - : int list = [1; 3; 5; 7; 9; 11] 
 * range_up 10 // is_even /@ (fun x -> x + 1) /> take 10  /@ (fun x -> x + 1);; *)
    let ( @ ) = append;
    let ( /@ ) lst f = map f lst;
    let ( @/ ) = map;
    let ( // ) lst f = filter f lst;
    let ( //@ ) lst f = filter_map f lst;
    let ( @// ) = filter_map;
  end;
include List;
module ErrorMonad =
  struct
    type log = string;
    type result 'a = [ Left of 'a | Right of log ];
    let return x = Left x;
    let fail x = Right x;
    let ( >>= ) ma f =
      match ma with [ Left v -> f v | Right x -> Right x ];
    (* write this way to overcome type system*)
    let bind = ( >>= );
    let map f = fun [ Left v -> Left (f v) | Right s -> Right s ];
    let ( >>| ) ma (str, f) =
      match ma with [ Left v -> f v | Right x -> Right (x ^ str) ];
    (*  append error message later *)
    let ( >>? ) ma str =
      match ma with [ Left _ -> ma | Right x -> Right (x ^ str) ];
    let ( <|> ) fa fb a =
      match fa a with [ (Left _ as x) -> x | Right str -> (fb a) >>? str ];
    (* raise an exception to make type system simple  *)
    let unwrap f a =
      match f a with [ Left res -> res | Right msg -> failwith msg ];
    let mapi_m f xs =
      let rec aux acc xs =
        match xs with
        [ [] -> return []
        | [ x :: xs ] ->
            (f x acc) >>=
              (fun x -> (aux (acc + 1) xs) >>= (fun xs -> return [ x :: xs ])) ]
      in aux 0 xs;
  end;
module Option =
  struct
    let bind o f = match o with [ Some x -> f x | None -> None ];
    let map f = fun [ Some x -> Some (f x) | None -> None ];
    let adapt f a = Some (f a);
  end;
module Log =
  struct
    let verbose = ref 1;
    let dprintf ?(level = 1) =
      if !verbose > level then eprintf else ifprintf err_formatter;
    (* ifprintf to overcome type system *)
    (* infoprintf will not *)
    let info_printf a = dprintf ~level: 2 a;
    let warn_printf a = dprintf ~level: 1 a;
    let error_printf a = dprintf ~level: 0 a;
  end;
module Char =
  struct
    include Char;
    let is_whitespace =
      fun [ ' ' | '\n' | '\r' | '\t' | '\026' | '\012' -> True | _ -> False ];
    let is_newline = fun [ '\n' | '\r' -> True | _ -> False ];
    let is_digit = fun [ '0' .. '9' -> True | _ -> False ];
    let is_uppercase c = ('A' <= c) && (c <= 'Z');
    let is_lowercase c = ('a' <= c) && (c <= 'z');
    let is_uppercase_latin1 c =
      (is_uppercase c) ||
        ((('\192' <= (*À*) c) && (c <= '\214')) || (*Ö*)
           (('\216' <= (*Ø*) c) && (c <= '\221')));
    (*Ý*)
    let is_lowercase_latin1 c =
      (is_lowercase c) ||
        ((('\222' <= (*Þ*) c) && (c <= '\246')) || (*ö*)
           (('\248' <= (*ø*) c) && (c <= '\255')));
    (*'ÿ'*)
    let is_latin1 c = (is_uppercase_latin1 c) || (is_lowercase_latin1 c);
    let is_symbol =
      fun
      [ '!' | '%' | '&' | '$' | '#' | '+' | '-' | '/' | ':' | '<' | '=' | '>'
          | '?' | '@' | '\\' | '~' | '^' | '|' | '*' -> True
      | _ -> False ];
    let is_letter c = (is_uppercase c) || (is_lowercase c);
    external unsafe_int : char -> int = "%identity";
    external unsafe_chr : int -> char = "%identity";
    let is_capital c =
      let c = code c in (c >= (code 'A')) && (c <= (code 'Z'));
    let of_digit i =
      if (i >= 0) && (i < 10)
      then unsafe_chr (i + (code '0'))
      else invalid_arg "Char.of_digit";
  end;
(*$T of_digit
  of_digit 6 = '6'
  try ignore (of_digit (-2)); false with Invalid_argument _ -> true
  try ignore (of_digit (46)); false with Invalid_argument _ -> true
 *)
module String =
  struct
    include String;
    let init len f =
      let s = create len
      in (for i = 0 to len - 1 do unsafe_set s i (f i) done; s);
    (*$T init
  init 5 (fun i -> Char.chr (i + int_of_char '0')) = "01234";
 *)
    (*
 *)
    let ends_with s e =
      let ne = String.length e
      and ns = String.length s
      in
        (ns >= ne) &&
          (callcc
             (fun k ->
                let diff = ns - ne
                in
                  (for i = 0 to ne - 1 do
                     if (unsafe_get s (diff + i)) <> (unsafe_get e i)
                     then raise & (k False)
                     else ()
                   done;
                   True)));
    (*
  {[
  starts_with "foobarbaz" "foob"
  true 
  starts_with "foobarbaz" ""
  true
  starts_with "" ""
  true
  not (starts_with "bar" "foobar")
  true
  not (starts_with "" "foo")
  true;;
  starts_with "Jon \"Maddog\" Orwant" "Jon"
  true
  not (starts_with "Jon \"Maddog\" Orwant" "Jon \"Maddog\" Orwants")
  true
  not (starts_with "Jon \"Maddog\" Orwant" "Orwants")
  true
  ]}
 *)
    let starts_with s e =
      let ne = length e in
      let ns = length s
      in
        (ns >= ne) &&
          (callcc
             (fun k ->
                (for i = 0 to ne - 1 do
                   if (unsafe_get s i) <> (unsafe_get e i)
                   then raise & (k False)
                   else ()
                 done;
                 True)));
    (*
  {[
  neg "ab"
  "-ab"
  neg_string ""
  "-"
  neg "-3"
  "3"
  ]} *)
    let neg n =
      let len = String.length n
      in
        if (len > 0) && (n.[0] = '-')
        then String.sub n 1 (len - 1)
        else "-" ^ n;
    (*
  {[
  drop_while (fun x -> x = '_') "__a";
  string = "a"
  drop_while (fun x -> x = '_') "a";
  string = "a"
  drop_while (fun x -> x = '_') "";
  string = ""
  ]}
 *)
    let drop_while f s =
      let len = String.length s in
      let found = ref False in
      let i = ref 0
      in
        (while (!i < len) && (not !found) do
           if not (f s.[!i]) then found:= True else incr i done;
         String.sub s !i (len - !i));
    let find_from str pos sub =
      let len = length str in
      let sublen = length sub
      in
        (if (pos < 0) || (pos > len)
         then raise (Invalid_argument "String.find_from")
         else ();
         if sublen = 0
         then pos
         else
           callcc
             (fun k ->
                (for i = pos to len - sublen do
                   let j = ref 0;
                   while
                     (unsafe_get str (i + j.contents)) = (unsafe_get sub !j) do
                     incr j; if !j = sublen then raise & (k i) else ()
                     done
                 done;
                 raise Not_found)));
    (*$T find_from
  find_from "foobarbaz" 4 "ba" = 6
  find_from "foobarbaz" 7 "" = 7
  try ignore (find_from "" 0 "a"); false with Not_found -> true
  try ignore (find_from "foo" 2 "foo"); false with Not_found -> true
  try ignore (find_from "foo" 3 "foo"); false with Not_found -> true
  try ignore (find_from "foo" 4 "foo"); false with Invalid_argument _ -> true
  try ignore (find_from "foo" (-1) "foo"); false with Invalid_argument _ -> true
 *)
    let find str sub = find_from str 0 sub;
    (*$T find
  find "foobarbaz" "bar" = 3
  try ignore (find "foo" "bar"); false with Not_found -> true
 *)
    let rfind_from str pos sub =
      let sublen = length sub
      and len = length str
      in
        (if ((pos + 1) < 0) || ((pos + 1) > len)
         then raise (Invalid_argument "String.rfind_from")
         else ();
         if sublen = 0
         then pos + 1
         else
           callcc
             (fun k ->
                (for i = (pos - sublen) + 1 downto 0 do
                   let j = ref 0;
                   while
                     (unsafe_get str (i + !j)) = (unsafe_get sub !j) do
                     incr j; if !j = sublen then raise & (k i) else ()
                     done
                 done;
                 raise Not_found)));
    (*$T rfind_from
  rfind_from "foobarbaz" 5 "ba" = 3
  rfind_from "foobarbaz" 7 "ba" = 6
  rfind_from "foobarbaz" 6 "ba" = 3
  rfind_from "foobarbaz" 7 "" = 8
  try ignore (rfind_from "" 3 ""); false with Invalid_argument _ -> true
  try ignore (rfind_from "" (-1) "a"); false with Not_found -> true
  try ignore (rfind_from "foobarbaz" 2 "ba"); false with Not_found -> true
  try ignore (rfind_from "foo" 3 "foo"); false with Invalid_argument _ -> true
  try ignore (rfind_from "foo" (-2) "foo"); false with Invalid_argument _ -> true
 *)
    let rfind str sub = rfind_from str ((String.length str) - 1) sub;
    (*$T rfind
  rfind "foobarbaz" "ba" = 6
  try ignore (rfind "foo" "barr"); false with Not_found -> true
 *)
    let strip ?(chars = " \t\r\n") s =
      let p = ref 0 in
      let l = length s
      in
        (while (!p < l) && (contains chars (unsafe_get s !p)) do 
           incr p done;
         let p = !p;
         let l = ref (l - 1);
         while (!l >= p) && (contains chars (unsafe_get s !l)) do
           decr l done;
         sub s p ((!l - p) + 1));
    (*$T strip
  strip ~chars:" ,()" " boo() bar()" = "boo() bar"
  strip ~chars:"abc" "abbcbab"
  ""
 *)
    let exists str sub =
      try (ignore (find str sub); True) with [ Not_found -> False ];
    (*$T exists
  exists "foobarbaz" "obar"
  exists "obar" "obar"
  exists "foobarbaz" ""
  exists "" ""
  not (exists "foobar" "obb")
  not (exists "" "foo")
  exists "a" ""
  not (exists "" "a")
  exists "ab" "a"
  exists "ab" "b"
  not (exists "ab" "c")
 *)
    let left s len = if len >= (length s) then s else sub s 0 len;
    (*$T left
  left "abc" 1 = "a"
  left "ab" 3 = "ab"
  left "abc" 3 = "abc"
  left "abc" 10 = "abc"
  left "abc" 0 = ""
 *)
    let right s len =
      let slen = length s
      in if len >= slen then s else sub s (slen - len) len;
    (*$T right
  right "abc" 1 = "c"
  right "ab" 3 = "ab"
  right "abc" 3 = "abc"
  right "abc" 0 = ""
  right "abc" 10 = "abc" *)
    let head s pos = left s pos;
    (*$T head
  head "abc" 0 = ""
  head "abc" 10 = "abc"
  head "abc" 3 = "abc"
 *)
    let tail s pos =
      let slen = length s
      in if pos >= slen then "" else sub s pos (slen - pos);
    (*$T tail
  tail "abc" 1
  "bc"
  tail "ab" 3
  ""
  tail "abc" 3
  ""
  tail "abc" 10
  ""
  tail "abc" 0
  "abc"
 *)
    let split str ~by: (sep) =
      let p = find str sep in
      let len = length sep in
      let slen = length str
      in ((sub str 0 p), (sub str (p + len) ((slen - p) - len)));
    (*$T split
  split "abcGxyzG123" ~by:"G" = ("abc","xyzG123")
  split "abcGHIzyxGHI123" ~by:"GHI"
  ("abc", "zyxGHI123")
  split "abcGHIzyxGHI123" ~by:""
  ("", "abcGHIzyxGHI123")
  try split "abcxyz" ~by:"G" |> ignore; false with Not_found -> true
  split "abcabc" ~by:"abc"
  ("", "abc")
  split "abcabcd" ~by:"abcd"
  ("abc", "")
 *)
    let rsplit str ~by: (sep) =
      let p = rfind str sep in
      let len = length sep in
      let slen = length str
      in ((sub str 0 p), (sub str (p + len) ((slen - p) - len)));
    (*$T rsplit
  rsplit "abcGxyzG123" ~by:"G" = ("abcGxyz","123")
  rsplit "abcGHIzyxGHI123" ~by:"GHI" = ("abcGHIzyx", "123")
  rsplit "abcGHIzyxGHI123" ~by:"" = ("abcGHIzyxGHI123", "")
  try rsplit "abcxyz" ~by:"G" |> ignore; false with Not_found -> true
 *)
    (*
  An implementation of [nsplit] in one pass.
  This implementation traverses the string backwards, hence building the list
  of substrings from the end to the beginning, so as to avoid a call to [List.rev].
 *)
    let nsplit str ~by: (sep) =
      if str = ""
      then []
      else
        if sep = ""
        then invalid_arg "nsplit: empty sep not allowed"
        else (* str is non empty *)
          let seplen = String.length sep in
          let rec aux acc ofs =
            if ofs >= 0
            then
              match try Some (rfind_from str ofs sep)
                    with [ Not_found -> None ]
              with
              [ Some idx -> (* sep found *)
                  let end_of_sep = (idx + seplen) - 1
                  in
                    if end_of_sep = ofs
                    then (* sep at end of str *) aux [ "" :: acc ] (idx - 1)
                    else
                      let token = sub str (end_of_sep + 1) (ofs - end_of_sep)
                      in aux [ token :: acc ] (idx - 1)
              | None -> (* sep NOT found *) [ sub str 0 (ofs + 1) :: acc ] ]
            else
              (* Negative ofs: the last sep started at the beginning of str *)
              [ "" :: acc ]
          in aux [] ((length str) - 1);
    (*$T nsplit
  nsplit "a;b;c" ~by:";" = ["a"; "b"; "c"]
  nsplit "" ~by:"x" = []
  try nsplit "abc" ~by:"" = ["a"; "b"; "c"] with Invalid_argument _ -> true
  nsplit "a/b/c" ~by:"/" = ["a"; "b"; "c"]
  nsplit "/a/b/c//" ~by:"/" = [""; "a"; "b"; "c"; ""; ""]
  nsplit "FOOaFOObFOOcFOOFOO" ~by:"FOO" = [""; "a"; "b"; "c"; ""; ""]
 *)
    let join = concat;
    let unsafe_slice i j s =
      if (i >= j) || (i = (length s)) then create 0 else sub s i (j - i);
    let clip ~lo ~hi (x : int) =
      if x < lo then lo else if x > hi then hi else x;
    let wrap (x : int) ~hi = if x < 0 then hi + x else x;
    let slice ?(first = 0) ?(last = Sys.max_string_length) s =
      let lo = 0 and hi = length s in
      let i = clip ~lo ~hi (wrap first ~hi) in
      let j = clip ~lo ~hi (wrap last ~hi) in unsafe_slice i j s;
    (*$T slice
  slice ~first:1 ~last:(-3) " foo bar baz" = "foo bar "
  slice "foo" = "foo"
  slice ~first:0 ~last:10 "foo" = "foo"
  slice ~first:(-2) "foo" = "oo"
  slice ~first:(-3) ~last:(-1) "foob" = "oo"
  slice ~first:5 ~last:4 "foobarbaz" = ""
 *)
    let lchop ?(n = 1) s =
      if n < 0
      then invalid_arg "lchop: number of characters to chop is negative"
      else
        let slen = length s in if slen <= n then "" else sub s n (slen - n);
    (*$T lchop
  lchop "Weeble" = "eeble"
  lchop "" = ""
  lchop ~n:3 "Weeble" = "ble"
  lchop ~n:1000 "Weeble" = ""
  lchop ~n:0 "Weeble" = "Weeble"
  try ignore (lchop ~n:(-1) "Weeble"); false with Invalid_argument _ -> true
 *)
    let rchop ?(n = 1) s =
      if n < 0
      then invalid_arg "rchop: number of characters to chop is negative"
      else
        let slen = length s in if slen <= n then "" else sub s 0 (slen - n);
    (*$T rchop
  rchop "Weeble" = "Weebl"
  rchop "" = ""
  rchop ~n:3 "Weeble" = "Wee"
  rchop ~n:1000 "Weeble" = ""
  try ignore (rchop ~n:(-1) "Weeble"); false with Invalid_argument _ -> true
 *)
    let of_int = string_of_int;
    (*$T of_int
  of_int 56 = "56"
  of_int (-1) = "-1"
 *)
    let of_float = string_of_float;
    let of_char = make 1;
    (*$T of_char
  of_char 's' = "s"
  of_char '\000' = "\000"
 *)
    let to_int s = int_of_string s;
    (*$T to_int
  to_int "8_480" = to_int "0x21_20"
  try ignore (to_int ""); false with Failure "int_of_string" -> true
  try ignore (to_int "2,3"); false with Failure "int_of_string" -> true
 *)
    let to_float s = float_of_string s;
  end;
(*$T to_float
  to_float "12.34e-1" = to_float "1.234"
  to_float "1" = 1.
  try ignore (to_float ""); false with Failure _ -> true
 *)
(*    
let is_antiquot_data_ctor s = String.ends_with s "Ant"
    
(**
   {[
   mk_anti ~c:"binding" "list" "code" ;
   
   string = "\\$listbinding:code"
   ]}
 *)
let mk_anti ?(c = "") n s = "\\$" ^ (n ^ (c ^ (":" ^ s)))
                                      
(** \\$expr;:code *)
let is_antiquot s =
  let len = String.length s in (len > 2) && ((s.[0] = '\\') && (s.[1] = '$'))
    
let handle_antiquot_in_string ~s ~term ~parse ~loc ~decorate =
  if is_antiquot s
  then
    (let pos = String.index s ':' in
    let name = String.sub s 2 (pos - 2)
    and code = String.sub s (pos + 1) (((String.length s) - pos) - 1)
    in decorate name (parse loc code))
  else term
      
      
(**
   {[
   destruct_poly "`a";
   Some "a"
   ]}
 *)
let destruct_poly s =
  let n = String.length s  in
  if n = 0  then invalid_arg "destruct_poly length=0"
  else if s.[0] = '`' then Some (String.sub s 1 (n - 1)) else None
*)
exception Not_implemented;
module Ref =
  struct
    type t 'a = ref 'a;
    let post r f = let old = !r in (r := f old; old);
    let pre r f = (r := f !r; !r);
    let modify r f = r := f !r;
    let swap a b = let buf = !a in (a := !b; b := buf);
    (*$T swap
  let a = ref 1 and b = ref 2 in swap a b; !a = 2 && !b = 1
 *)
    let pre_incr r = pre r (( + ) 1);
    let pre_decr r = pre r (( + ) (-1));
    let post_incr r = post r (( + ) 1);
    let post_decr r = post r (( + ) (-1));
    (*$T pre_incr
  let r = ref 0 in pre_incr r = 1 && !r = 1
 *)
    (*$T post_incr
  let r = ref 0 in post_incr r = 0 && !r = 1
 *)
    let copy r = ref !r;
    (*$T copy
  let r = ref 0 in let s = copy r in r := 1; !s == 0 && !r == 1
 *)
    let protect r v body =
      let old = !r
      in
        try (r := v; let res = body (); r := old; res)
        with [ x -> (r := old; raise x) ];
    (*$T protect
  let r = ref 0 in let b () = incr r; !r in protect r 2 b = 3 && !r = 0
  let r = ref 0 in let b () = incr r; if !r=3 then raise Not_found in (try protect r 2 b; false with Not_found -> true) && !r = 0
 *)
    external ref : 'a -> ref 'a = "%makemutable";
    (** Return a fresh reference containing the given let. *)
    external ( ! ) : ref 'a -> 'a = "%field0";
    (** [!r] returns the current contents of reference [r].
    Equivalent to [fun r -> r.contents]. *)
    external ( := ) : ref 'a -> 'a -> unit = "%setfield0";
    (** [r := a] stores the let of [a] in reference [r].
    Equivalent to [fun r v -> r.contents <- v]. *)
    external set : ref 'a -> 'a -> unit = "%setfield0";
    (** As [ := ] *)
    external get : ref 'a -> 'a = "%field0";
    (** As [ ! ]*)
    let print print_a out r = print_a out !r;
    let toggle r = r := not !r;
    (*$T toggle
  let r = ref true in toggle r; !r = false;
  let r = ref false in toggle r; !r = true;
 *)
    let oset r x = r := Some x;
    let oget_exn r =
      match !r with [ None -> raise Not_found | Some x -> x ];
    (*  FAIL $T oset, oget_exn
          let r = ref None in oset r 3; oget_exn r = 3
       *)
    let ord o x y = o !x !y;
    let eq e x y = e !x !y;
  end;
module Buffer =
  struct
    include Buffer;
    module Ops =
      struct
        let ( +> ) buf chr = (Buffer.add_char buf chr; buf);
        let ( +>> ) buf str = (Buffer.add_string buf str; buf);
      end;
  end;
module Hashtbl =
  struct
    include Hashtbl;
    let keys tbl = Hashtbl.fold (fun k _v acc -> [ k :: acc ]) tbl [];
    let lets tbl = Hashtbl.fold (fun _k v acc -> [ v :: acc ]) tbl [];
  end;


module SMap = Map.Make(String);
module SSet = Set.Make(String);
let (|>) x f = f x;
let (&) f x = f x;

type log = string;

type result 'a =
  [ Left of 'a
  | Right of log];

(* let left x  =  Left x ; *)
  
let ret x = Left x ;  
let right x = Right x ;

(** maybe you need more log here *)  
let (>>=) ma f = match ma with
  [ Left v -> f v
  | Right x -> Right x (** rewrite to overcome the type system*)
  ];

let (>>|) ma (str,f) = match ma with
  [ Left v -> f v
  | Right x -> Right (x ^ str) ]
;
  
let (|-) f g  x =
  g (f x );

(**
   append error message later
 *)  
let (>>?) ma str = match ma with
  [ Left _ -> ma
  | Right x -> Right (x^str)]
;
  
let (<|>) fa fb = fun a->
  match fa a with
  [ Left _ as x -> x
  | Right str ->
      fb a >>? str
  ]
;

let unwrap f a = match f a with
  [ Left res -> res
  | Right msg -> invalid_arg msg]
;
  
let failwithf fmt = ksprintf failwith fmt  ;

let opt_bind o f = match o with
  [Some x -> f x
  | None -> None]
;

let opt_map o f = match o with
  [ Some x -> Some f x
  | None -> None]
;
  
let prerr_endlinef fmt = ksprintf prerr_endline fmt;

let verbose = ref 1;  
let dprintf ?(log_level=1)   =
  if !verbose >= log_level then
    (* prerr_endlinef fmt str *)
    eprintf
  else ifprintf err_formatter;


let ends_with s e = 
   let ne = String.length e
   and ns = String.length s in
   ns >= ne && String.sub s (ns-ne) ne = e 
;

let starts_with s e =
  let ne = String.length e and ns = String.length s in
  ns >= ne && String.sub s 0 ne = e
;    
let is_antiquot_data_ctor s =
       ends_with s "Ant"
;

(**
   [neg_string "ab" ] = ["-ab"]
   [neg_string ""] = ["-"]
 *)
let neg_string n =
  let len = String.length n in
  if len > 0 && n.[0] = '-' then String.sub n 1 (len - 1)
  else "-" ^ n
;
  
(** f will be incremented for each item
    indexed from 0 *)
let fold_lefti
     (f : 'a -> int -> 'b -> 'b )
     (ty: list 'a)  (init:'b) : 'b =
    let (_,res) = List.fold_left begin fun (i,acc) ty ->
      (i+1, f ty i acc)
    end (0, init) ty
    in res
;


let  mapi_m f xs =
    let rec aux acc xs = 
      match xs with
    [ [] ->  ret []
    | [x::xs] ->
        f x acc >>= (fun x ->
        aux (acc+1) xs >>= (fun xs ->
        (ret [x::xs] )))]
    in aux 0 xs
;
    
let mapi f xs =
  let rec aux acc xs = match xs with
    [ [] -> []
    | [x::xs] ->
        [f x acc :: aux (acc+1) xs ]
    ] in
  aux 0 xs
;
  
(**
   [start,until]
 *)
let fold_nat_left ?(start=0) ~until ~acc f = do{
  let v = ref acc ;
  for x = start to until do
    v := f !v x 
  done;
  !v  
}
;  


let iteri f lst =
  let i = ref 0 in 
  List.iter (fun x -> 
    let () = f !i x in
    incr i) lst
;    

type dir = [= `Left | `Right];

let reduce_left f lst =
  match lst with
    [ [] -> invalid_arg "reduce_left length zero"
    | [x::xs] ->
        let rec loop x xs = match xs with
          [ [] -> x
          | [y::ys] -> loop (f x y) ys] in loop x xs];
    
let reduce_right_with ~compose ~f  lst =
  match lst with
    [ [] -> invalid_arg "reduce_right length zero"
    | xs ->
        let rec loop xs = match xs with
          [ [] -> assert False
          | [y] -> f y
          | [y::ys] -> compose (f y) (loop ys) ] in loop xs];
let reduce_right compose = reduce_right_with ~compose ~f:(fun x -> x);


(*
  {[
  string_drop_while (fun x -> x = '_') "__a";
  string = "a"
  string_drop_while (fun x -> x = '_') "a";
  string = "a"
  string_drop_while (fun x -> x = '_') "";
  string = ""
  ]}
 *)  
let string_drop_while f s =
  let len = String.length s in
  let found = ref False in
  let i = ref 0 in begin 
  while !i < len && not !found do
    if not (f s.[!i]) then 
      found:=True
    else incr i
  done ;
  String.sub s !i (len - !i)
  end 
;      

  
exception First;

(** do sequence does not support try with
    use do only for simple construct
 *)
let find_first f lst =
  let res = ref None in
  try
    List.iter (fun x ->
    match f x with
    [ Some v -> do{ res := Some v; raise First}
    | None -> ()]) lst;
    None;
  with [First ->  !res]
;
  

let mkmke preds (cons,tyargs)=
  match find_first (fun pred -> pred(cons,tyargs)) preds with
  [ None -> invalid_arg "mkmke"
  | Some r -> r ]
;

let adapt f a =
  Some (f a )
;

(* #default_quotation "expr"; *)
(* Camlp4.PreCast.Syntax.Quotation.default := "expr"; *)


let rec intersperse y xs = match xs with
  [ [] -> []
  | [_x] -> xs
  | [x::xs] ->
      [x ; y :: intersperse y xs]
  ]
;  

let init n f =
  Array.( init n f |> to_list)
;    



let to_string_of_printer printer v =
  let buf = Buffer.create 30 in 
  let () = Format.bprintf buf "@[%a@]" printer v in 
  Buffer.contents buf 
;

(**
   {[
   mk_anti ~c:"binding" "list" "code" ;
   
   string = "\\$listbinding:code"
   ]}
 *)    
let mk_anti ?(c = "") n s = "\\$"^n^c^":"^s;

(** \\$expr;:code *)
let is_antiquot s =
  let len = String.length s in
  len > 2 && s.[0] = '\\' && s.[1] = '$';

  
let handle_antiquot_in_string ~s ~term ~parse ~loc ~decorate =
  if is_antiquot s then
    let pos = String.index s ':' in
    let name = String.sub s 2 (pos - 2)
    and code = String.sub s (pos + 1) (String.length s - pos - 1) in
    decorate name (parse loc code)
  else term;

let is_capital c =
  let c = Char.code c  in
  (c >= Char.code 'A' &&
   c <= Char.code 'Z')
;
  
let is_digit c =
  let c = Char.code c in
  (c >= Char.code '0' && c <= Char.code '9')
;    
(**
   {[
   destruct_poly "`a";
   Some "a"
   ]}
 *)
let destruct_poly s =
  let n = String.length s in
  if n = 0 then
    invalid_arg "destruct_poly length=0"
  else
    if s.[0] = '`' then
      Some (String.sub s 1 (n-1))
    else None
;

let uncurry f (x,y)  =f x y;
(**
   {[
   drop 3 [1;2;3;4];

   list int = [4]
   ]}
 *)  
let rec drop n = fun
  [ [_ :: l] when n > 0 -> drop (n-1) l
  | l -> l]
;

(*
  {[
  [1;2;3;4;5]
  ([4;3;2;1], 5 )
  ]}
 *)
let lastbut1 ls = match ls with
  [ [ ] -> failwith "lastbut1 empty"
  |  _ -> let l = List.rev ls in
     (List.tl l, List.hd l ) ];
  

