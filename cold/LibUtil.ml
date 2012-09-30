include Format

module MapMake =
                 functor (S : Map.OrderedType) ->
                  struct
                   include (Map.Make)(S)

                   let of_list =
                    fun lst ->
                     (List.fold_left ( fun acc -> fun (k, v) -> (add k v acc)
                       ) empty lst)

                   let of_hashtbl =
                    fun tbl ->
                     (Hashtbl.fold (
                       fun k -> fun v -> fun acc -> (add k v acc) ) tbl
                       empty)

                   let elements =
                    fun map ->
                     (fold ( fun k -> fun v -> fun acc -> ( (k, v) ) :: acc 
                       ) map [] )

                  end

module StringMap = (MapMake)(String)

module IntMap =
                                                              (MapMake)
                                                               (struct
                                                                 type t = int

                                                                 let compare =
                                                                  Pervasives.compare

                                                                end)


module StringSet =
 struct include (Set.Make)(String)
 end

module IntSet =
                                          (Set.Make)
                                           (struct
                                             type t = int

                                             let compare = Pervasives.compare

                                            end)

module Hashset =
                                                   struct
                                                    type 'a t =
                                                     ('a, unit) Hashtbl.t

                                                    let create =
                                                     Hashtbl.create

                                                    let add =
                                                     fun set ->
                                                      fun x ->
                                                       (Hashtbl.replace set x
                                                         () )

                                                    let remove =
                                                     Hashtbl.remove

                                                    let mem = Hashtbl.mem

                                                    let iter =
                                                     fun f ->
                                                      (Hashtbl.iter (
                                                        fun v ->
                                                         fun ()  -> (f v) ))

                                                    let fold =
                                                     fun f ->
                                                      (Hashtbl.fold (
                                                        fun v ->
                                                         fun ()
                                                           ->
                                                          fun st -> (f v st)
                                                        ))

                                                    let elements =
                                                     Hashtbl.length

                                                    let clear = Hashtbl.clear

                                                    let of_list =
                                                     fun size ->
                                                      fun vs ->
                                                       let set =
                                                        (create size) in
                                                       (
                                                       (List.iter ( (add set)
                                                         ) vs)
                                                       );
                                                       set

                                                    let to_list =
                                                     fun set ->
                                                      (fold (
                                                        fun x ->
                                                         fun y -> ( x ) :: y 
                                                        ) set [] )

                                                   end

let mk_set =
                                                         fun (type s)
                                                         ->fun ~cmp ->
                                                            let module M =
                                                             struct
                                                              type t = s

                                                              let compare =
                                                               cmp

                                                             end in
                                                            ((module
                                                              (Set.Make)(M)) :
                                                              (module Set.S
                                                               with type elt
                                                               = s))


let mk_map =
 fun (type s)
 ->fun ~cmp ->
    let module M = struct type t = s
 let compare = cmp
 end in
    ((module (Map.Make)(M)) : (module Map.S  with type key = s))

let mk_hashtbl =
                                                                   fun (type s)
                                                                   ->
                                                                   fun ~eq ->
                                                                    fun ~hash ->
                                                                    let module M =
                                                                    struct
                                                                    type t =
                                                                    s

                                                                    let equal =
                                                                    eq

                                                                    let hash =
                                                                    hash

                                                                    end in
                                                                    ((module
                                                                    (Hashtbl.Make)
                                                                    (M)) :
                                                                    (module Hashtbl.S
                                                                     with
                                                                    type key
                                                                    = 
                                                                    s))


let ( |> ) = fun x -> fun f -> (f x)

let ( /> ) = fun x -> fun f -> (f x)


let ( & ) = fun f -> fun x -> (f x)

let ( |- ) =
                                      fun f ->
                                       fun g -> fun x -> (g ( (f x) ))


let ( <| ) = fun f -> fun x -> (f x)

let ( -| ) =
                                       fun f ->
                                        fun g -> fun x -> (f ( (g x) ))


let ( *** ) = fun f -> fun g -> fun (x, y) -> (( (f x) ), ( (g y) ))


let ( &&& ) = fun f -> fun g -> fun x -> (( (f x) ), ( (g x) ))

let flip =
                                                                  fun f ->
                                                                   fun x ->
                                                                    fun y ->
                                                                    (f y x)


let curry = fun f -> fun x -> fun y -> (f (x, y))

let uncurry =
                                                    fun f ->
                                                     fun (x, y) -> (f x y)


let failwithf = fun fmt -> (ksprintf failwith fmt)

let prerr_endlinef =
                                                     fun fmt ->
                                                      (ksprintf prerr_endline
                                                        fmt)

let const =
                                                               fun x ->
                                                                fun _ -> x


let tap = fun f -> fun x -> ( (f x) ); x

let is_even =
                                           fun x -> (( (x mod 2) ) == 0)


let to_string_of_printer =
 fun printer ->
  fun v ->
   let buf = (Buffer.create 30) in
   let () = (Format.bprintf buf "@[%a@]" printer v) in (Buffer.contents buf)


let nfold_left =
 fun ?(start = 0) ->
  fun ~until ->
   fun ~acc ->
    fun f ->
     let v = (ref acc) in
     for x = start to until do (v := ( (f ( !v ) x) )) done;
     !v

type 'a cont = ('a -> exn)

let callcc =
                                      fun (type u)
                                      ->fun (f :
                                          (u cont -> u)) ->
                                         let module M =
                                          struct exception Return of u
 end in
                                         (try
                                           (f (
                                             fun x ->
                                              (raise ( (M.Return (x)) )) ))
                                          with
                                          M.Return (u) -> u)

module List =
                                                               struct
                                                                include List

                                                                type 'a mut_list = 
                                                                {
                                                                  hd:'a;
                                                                  mutable tl:
                                                                   'a list}

                                                                external
                                                                 inj :
                                                                 ('a mut_list
                                                                  -> 
                                                                  'a list) =
                                                                  "%identity"

                                                                let dummy_node =
                                                                 fun ()
                                                                   ->
                                                                  {hd = (
                                                                    (Obj.magic
                                                                    () ) );
                                                                   tl = [] }

                                                                let cons =
                                                                 fun h ->
                                                                  fun t ->
                                                                   ( h ) :: t 

                                                                let is_empty =
                                                                 function
                                                                 | [] ->
                                                                    (true)
                                                                 | _ ->
                                                                    (false)

                                                                let nth =
                                                                 fun l ->
                                                                  fun index ->
                                                                   (
                                                                   if 
                                                                    (index <
                                                                    0) then
                                                                    (
                                                                    (invalid_arg
                                                                    "Negative index not allowed")
                                                                    )
                                                                   else ()
                                                                   );
                                                                   let rec loop =
                                                                    fun n ->
                                                                    function
                                                                    | [] ->
                                                                    (invalid_arg
                                                                    "Index past end of list")
                                                                    | 
                                                                    (h :: t) ->
                                                                    if 
                                                                    (n = 0) then
                                                                    h
                                                                    else
                                                                    (loop (
                                                                    (n - 1) )
                                                                    t) in
                                                                   (loop
                                                                    index l)

                                                                let at = nth

                                                                let append =
                                                                 fun l1 ->
                                                                  fun l2 ->
                                                                   (match
                                                                    l1 with
                                                                    | [] ->
                                                                    l2
                                                                    | 
                                                                    (h :: t) ->
                                                                    let rec loop =
                                                                    fun dst ->
                                                                    function
                                                                    | [] ->
                                                                    dst.tl <-
                                                                    l2
                                                                    | 
                                                                    (h :: t) ->
                                                                    let cell =
                                                                    {hd = h;
                                                                    tl = [] } in
                                                                    (
                                                                    dst.tl <-
                                                                    (inj
                                                                    cell)
                                                                    );
                                                                    (loop
                                                                    cell t) in
                                                                    let r =
                                                                    {hd = h;
                                                                    tl = [] } in
                                                                    (
                                                                    (loop r
                                                                    t)
                                                                    );
                                                                    (inj r))

                                                                let flatten =
                                                                 fun l ->
                                                                  let rec inner =
                                                                   fun dst ->
                                                                    function
                                                                    | [] ->
                                                                    dst
                                                                    | 
                                                                    (h :: t) ->
                                                                    let r =
                                                                    {hd = h;
                                                                    tl = [] } in
                                                                    (
                                                                    dst.tl <-
                                                                    (inj r)
                                                                    );
                                                                    (inner r
                                                                    t) in
                                                                  let rec outer =
                                                                   fun dst ->
                                                                    function
                                                                    | [] ->
                                                                    ()
                                                                    | 
                                                                    (h :: t) ->
                                                                    (outer (
                                                                    (inner
                                                                    dst h) )
                                                                    t) in
                                                                  let r =
                                                                   (dummy_node
                                                                    () ) in
                                                                  (
                                                                  (outer r l)
                                                                  );
                                                                  r.tl

                                                                let concat =
                                                                 flatten

                                                                let map =
                                                                 fun f ->
                                                                  function
                                                                  | [] ->
                                                                    ([])
                                                                  | (h :: t) ->
                                                                    let rec loop =
                                                                    fun dst ->
                                                                    function
                                                                    | [] ->
                                                                    ()
                                                                    | 
                                                                    (h :: t) ->
                                                                    let r =
                                                                    {hd = (
                                                                    (f h) );
                                                                    tl = [] } in
                                                                    (
                                                                    dst.tl <-
                                                                    (inj r)
                                                                    );
                                                                    (loop r
                                                                    t) in
                                                                    let r =
                                                                    {hd = (
                                                                    (f h) );
                                                                    tl = [] } in
                                                                    (
                                                                    (loop r
                                                                    t)
                                                                    );
                                                                    (inj r)

                                                                let take =
                                                                 fun n ->
                                                                  fun l ->
                                                                   let rec loop =
                                                                    fun n ->
                                                                    fun dst ->
                                                                    function
                                                                    | (h ::
                                                                    t) when
                                                                    (n > 0) ->
                                                                    let r =
                                                                    {hd = h;
                                                                    tl = [] } in
                                                                    (
                                                                    dst.tl <-
                                                                    (inj r)
                                                                    );
                                                                    (loop (
                                                                    (n - 1) )
                                                                    r t)
                                                                    | 
                                                                    _ -> () in
                                                                   let dummy =
                                                                    (dummy_node
                                                                    () ) in
                                                                   (
                                                                   (loop n
                                                                    dummy l)
                                                                   );
                                                                   dummy.tl

                                                                let take_while =
                                                                 fun p ->
                                                                  fun li ->
                                                                   let rec loop =
                                                                    fun dst ->
                                                                    function
                                                                    | [] ->
                                                                    ()
                                                                    | 
                                                                    (x :: xs) ->
                                                                    if 
                                                                    (p x) then
                                                                    (
                                                                    let r =
                                                                    {hd = x;
                                                                    tl = [] } in
                                                                    (
                                                                    dst.tl <-
                                                                    (inj r)
                                                                    );
                                                                    (loop r
                                                                    xs)
                                                                    )
                                                                    else () in
                                                                   let dummy =
                                                                    (dummy_node
                                                                    () ) in
                                                                   (
                                                                   (loop
                                                                    dummy li)
                                                                   );
                                                                   dummy.tl

                                                                let rec drop_while =
                                                                 fun f ->
                                                                  function
                                                                  | [] ->
                                                                    ([])
                                                                  | (x :: xs)
                                                                    when
                                                                    (f x) ->
                                                                    (drop_while
                                                                    f xs)
                                                                  | xs -> xs

                                                                let takewhile =
                                                                 take_while

                                                                let dropwhile =
                                                                 drop_while

                                                                let interleave =
                                                                 fun ?first ->
                                                                  fun ?last ->
                                                                   fun (sep :
                                                                    'a) ->
                                                                    fun (l :
                                                                    'a list) ->
                                                                    let rec aux =
                                                                    fun acc ->
                                                                    function
                                                                    | [] ->
                                                                    acc
                                                                    | 
                                                                    (h :: t) ->
                                                                    (aux (
                                                                    ( h ) ::
                                                                    sep ::
                                                                    acc  ) t) in
                                                                    (
                                                                    match
                                                                    (l,
                                                                    first,
                                                                    last) with
                                                                    | ([],
                                                                    None,
                                                                    None) ->
                                                                    ([])
                                                                    | ([],
                                                                    None,
                                                                    Some (x)) ->
                                                                    [x]
                                                                    | ([],
                                                                    Some (x),
                                                                    None) ->
                                                                    [x]
                                                                    | ([],
                                                                    Some (x),
                                                                    Some (y)) ->
                                                                    [x; y]
                                                                    | ((h ::
                                                                    []),
                                                                    None,
                                                                    None) ->
                                                                    [h]
                                                                    | ((h ::
                                                                    []),
                                                                    None,
                                                                    Some (x)) ->
                                                                    [h; x]
                                                                    | ((h ::
                                                                    []),
                                                                    Some (x),
                                                                    None) ->
                                                                    [x; h]
                                                                    | ((h ::
                                                                    []),
                                                                    Some (x),
                                                                    Some (y)) ->
                                                                    [x; h; y]
                                                                    | ((h ::
                                                                    t), 
                                                                    None,
                                                                    None) ->
                                                                    (rev (
                                                                    (aux (
                                                                    [h] ) t)
                                                                    ))
                                                                    | ((h ::
                                                                    t),
                                                                    Some (x),
                                                                    None) ->
                                                                    ( x ) ::
                                                                    (rev (
                                                                    (aux (
                                                                    [h] ) t)
                                                                    )) 
                                                                    | ((h ::
                                                                    t), 
                                                                    None,
                                                                    Some (y)) ->
                                                                    (rev_append
                                                                    (
                                                                    (aux (
                                                                    [h] ) t)
                                                                    ) ( 
                                                                    [y] ))
                                                                    | 
                                                                    ((h :: t),
                                                                    Some (x),
                                                                    Some (y)) ->
                                                                    ( x ) ::
                                                                    (rev_append
                                                                    (
                                                                    (aux (
                                                                    [h] ) t)
                                                                    ) ( 
                                                                    [y] )) )

                                                                let unique =
                                                                 fun (type et)
                                                                 ->fun ?
                                                                    (hash =
                                                                    Hashtbl.hash) ->
                                                                    fun ?
                                                                    (eq =
                                                                    (=)) ->
                                                                    fun (l :
                                                                    et list) ->
                                                                    let module HT =
                                                                    (Hashtbl.Make)
                                                                    (struct
                                                                    type t =
                                                                    et

                                                                    let equal =
                                                                    eq

                                                                    let hash =
                                                                    hash

                                                                    end) in
                                                                    let ht =
                                                                    (HT.create
                                                                    (
                                                                    (List.length
                                                                    l) )) in
                                                                    let rec loop =
                                                                    fun dst ->
                                                                    function
                                                                    | (h ::
                                                                    t) when
                                                                    (not (
                                                                    (HT.mem
                                                                    ht h) )) ->
                                                                    (
                                                                    (HT.add
                                                                    ht h () )
                                                                    );
                                                                    let r =
                                                                    {hd = h;
                                                                    tl = [] } in
                                                                    (
                                                                    dst.tl <-
                                                                    (inj r)
                                                                    );
                                                                    (loop r
                                                                    t)
                                                                    | (_ ::
                                                                    t) ->
                                                                    (loop dst
                                                                    t)
                                                                    | 
                                                                    [] ->
                                                                    () in
                                                                    let dummy =
                                                                    (dummy_node
                                                                    () ) in
                                                                    (
                                                                    (loop
                                                                    dummy l)
                                                                    );
                                                                    dummy.tl

                                                                let filter_map =
                                                                 fun f ->
                                                                  fun l ->
                                                                   let rec loop =
                                                                    fun dst ->
                                                                    function
                                                                    | [] ->
                                                                    ()
                                                                    | 
                                                                    (h :: t) ->
                                                                    (
                                                                    match
                                                                    (f h) with
                                                                    | None ->
                                                                    (loop dst
                                                                    t)
                                                                    | 
                                                                    Some (x) ->
                                                                    let r =
                                                                    {hd = x;
                                                                    tl = [] } in
                                                                    (
                                                                    dst.tl <-
                                                                    (inj r)
                                                                    );
                                                                    (loop r
                                                                    t)) in
                                                                   let dummy =
                                                                    (dummy_node
                                                                    () ) in
                                                                   (
                                                                   (loop
                                                                    dummy l)
                                                                   );
                                                                   dummy.tl

                                                                let rec find_map =
                                                                 fun f ->
                                                                  function
                                                                  | [] ->
                                                                    (None)
                                                                  | (x :: xs) ->
                                                                    (
                                                                    match
                                                                    (f x) with
                                                                    | (Some
                                                                    (_) as y) ->
                                                                    y
                                                                    | 
                                                                    None ->
                                                                    (find_map
                                                                    f xs))

                                                                let fold_right_max =
                                                                 1000

                                                                let fold_right =
                                                                 fun f ->
                                                                  fun l ->
                                                                   fun init ->
                                                                    let rec tail_loop =
                                                                    fun acc ->
                                                                    function
                                                                    | [] ->
                                                                    acc
                                                                    | 
                                                                    (h :: t) ->
                                                                    (tail_loop
                                                                    (
                                                                    (f h acc)
                                                                    ) t) in
                                                                    let rec loop =
                                                                    fun n ->
                                                                    function
                                                                    | [] ->
                                                                    init
                                                                    | 
                                                                    (h :: t) ->
                                                                    if 
                                                                    (n <
                                                                    fold_right_max) then
                                                                    (
                                                                    (f h (
                                                                    (loop (
                                                                    (n + 1) )
                                                                    t) ))
                                                                    )
                                                                    else
                                                                    (f h (
                                                                    (tail_loop
                                                                    init (
                                                                    (rev t)
                                                                    )) )) in
                                                                    (loop 0
                                                                    l)

                                                                let map2 =
                                                                 fun f ->
                                                                  fun l1 ->
                                                                   fun l2 ->
                                                                    let rec loop =
                                                                    fun dst ->
                                                                    fun src1 ->
                                                                    fun src2 ->
                                                                    (
                                                                    match
                                                                    (src1,
                                                                    src2) with
                                                                    | ([], []) ->
                                                                    ()
                                                                    | ((h1 ::
                                                                    t1),
                                                                    (h2 ::
                                                                    t2)) ->
                                                                    let r =
                                                                    {hd = (
                                                                    (f h1 h2)
                                                                    );
                                                                    tl = [] } in
                                                                    (
                                                                    dst.tl <-
                                                                    (inj r)
                                                                    );
                                                                    (loop r
                                                                    t1 t2)
                                                                    | 
                                                                    _ ->
                                                                    (invalid_arg
                                                                    "map2: Different_list_size")) in
                                                                    let dummy =
                                                                    (dummy_node
                                                                    () ) in
                                                                    (
                                                                    (loop
                                                                    dummy l1
                                                                    l2)
                                                                    );
                                                                    dummy.tl

                                                                let rec iter2 =
                                                                 fun f ->
                                                                  fun l1 ->
                                                                   fun l2 ->
                                                                    (
                                                                    match
                                                                    (l1, l2) with
                                                                    | ([], []) ->
                                                                    ()
                                                                    | ((h1 ::
                                                                    t1),
                                                                    (h2 ::
                                                                    t2)) ->
                                                                    (
                                                                    (f h1 h2)
                                                                    );
                                                                    (iter2 f
                                                                    t1 t2)
                                                                    | 
                                                                    _ ->
                                                                    (invalid_arg
                                                                    "iter2: Different_list_size"))

                                                                let rec fold_left2 =
                                                                 fun f ->
                                                                  fun accum ->
                                                                   fun l1 ->
                                                                    fun l2 ->
                                                                    (
                                                                    match
                                                                    (l1, l2) with
                                                                    | ([], []) ->
                                                                    accum
                                                                    | ((h1 ::
                                                                    t1),
                                                                    (h2 ::
                                                                    t2)) ->
                                                                    (fold_left2
                                                                    f (
                                                                    (f accum
                                                                    h1 h2) )
                                                                    t1 t2)
                                                                    | 
                                                                    _ ->
                                                                    (invalid_arg
                                                                    "fold_left2: Different_list_size"))

                                                                let fold_right2 =
                                                                 fun f ->
                                                                  fun l1 ->
                                                                   fun l2 ->
                                                                    fun init ->
                                                                    let rec tail_loop =
                                                                    fun acc ->
                                                                    fun l1 ->
                                                                    fun l2 ->
                                                                    (
                                                                    match
                                                                    (l1, l2) with
                                                                    | ([], []) ->
                                                                    acc
                                                                    | ((h1 ::
                                                                    t1),
                                                                    (h2 ::
                                                                    t2)) ->
                                                                    (tail_loop
                                                                    (
                                                                    (f h1 h2
                                                                    acc) ) t1
                                                                    t2)
                                                                    | 
                                                                    _ ->
                                                                    (invalid_arg
                                                                    "fold_left2: Different_list_size")) in
                                                                    let rec loop =
                                                                    fun n ->
                                                                    fun l1 ->
                                                                    fun l2 ->
                                                                    (
                                                                    match
                                                                    (l1, l2) with
                                                                    | ([], []) ->
                                                                    init
                                                                    | ((h1 ::
                                                                    t1),
                                                                    (h2 ::
                                                                    t2)) ->
                                                                    if 
                                                                    (n <
                                                                    fold_right_max) then
                                                                    (
                                                                    (f h1 h2
                                                                    (
                                                                    (loop (
                                                                    (n + 1) )
                                                                    t1 t2) ))
                                                                    )
                                                                    else
                                                                    (f h1 h2
                                                                    (
                                                                    (tail_loop
                                                                    init (
                                                                    (rev t1)
                                                                    ) (
                                                                    (rev t2)
                                                                    )) ))
                                                                    | 
                                                                    _ ->
                                                                    (invalid_arg
                                                                    "fold_right2: Different_list_size")) in
                                                                    (loop 0
                                                                    l1 l2)

                                                                let for_all2 =
                                                                 fun p ->
                                                                  fun l1 ->
                                                                   fun l2 ->
                                                                    let rec loop =
                                                                    fun l1 ->
                                                                    fun l2 ->
                                                                    (
                                                                    match
                                                                    (l1, l2) with
                                                                    | ([], []) ->
                                                                    (true)
                                                                    | ((h1 ::
                                                                    t1),
                                                                    (h2 ::
                                                                    t2)) ->
                                                                    if 
                                                                    (p h1 h2) then
                                                                    (
                                                                    (loop t1
                                                                    t2)
                                                                    )
                                                                    else
                                                                    (false)
                                                                    | 
                                                                    _ ->
                                                                    (invalid_arg
                                                                    "for_all2: Different_list_size")) in
                                                                    (loop l1
                                                                    l2)

                                                                let exists2 =
                                                                 fun p ->
                                                                  fun l1 ->
                                                                   fun l2 ->
                                                                    let rec loop =
                                                                    fun l1 ->
                                                                    fun l2 ->
                                                                    (
                                                                    match
                                                                    (l1, l2) with
                                                                    | ([], []) ->
                                                                    (false)
                                                                    | ((h1 ::
                                                                    t1),
                                                                    (h2 ::
                                                                    t2)) ->
                                                                    if 
                                                                    (p h1 h2) then
                                                                    true
                                                                    
                                                                    else
                                                                    (loop t1
                                                                    t2)
                                                                    | 
                                                                    _ ->
                                                                    (invalid_arg
                                                                    "exists2: Different_list_size")) in
                                                                    (loop l1
                                                                    l2)

                                                                let remove_assoc =
                                                                 fun x ->
                                                                  fun lst ->
                                                                   let rec loop =
                                                                    fun dst ->
                                                                    function
                                                                    | [] ->
                                                                    ()
                                                                    | 
                                                                    (((a, _) as
                                                                    pair) ::
                                                                    t) ->
                                                                    if 
                                                                    (a = x) then
                                                                    (
                                                                    dst.tl <-
                                                                    t
                                                                    )
                                                                    else
                                                                    let r =
                                                                    {hd =
                                                                    pair;
                                                                    tl = [] } in
                                                                    (
                                                                    dst.tl <-
                                                                    (inj r)
                                                                    );
                                                                    (loop r
                                                                    t) in
                                                                   let dummy =
                                                                    (dummy_node
                                                                    () ) in
                                                                   (
                                                                   (loop
                                                                    dummy
                                                                    lst)
                                                                   );
                                                                   dummy.tl

                                                                let remove_assq =
                                                                 fun x ->
                                                                  fun lst ->
                                                                   let rec loop =
                                                                    fun dst ->
                                                                    function
                                                                    | [] ->
                                                                    ()
                                                                    | 
                                                                    (((a, _) as
                                                                    pair) ::
                                                                    t) ->
                                                                    if 
                                                                    (a == x) then
                                                                    (
                                                                    dst.tl <-
                                                                    t
                                                                    )
                                                                    else
                                                                    let r =
                                                                    {hd =
                                                                    pair;
                                                                    tl = [] } in
                                                                    (
                                                                    dst.tl <-
                                                                    (inj r)
                                                                    );
                                                                    (loop r
                                                                    t) in
                                                                   let dummy =
                                                                    (dummy_node
                                                                    () ) in
                                                                   (
                                                                   (loop
                                                                    dummy
                                                                    lst)
                                                                   );
                                                                   dummy.tl

                                                                let rfind =
                                                                 fun p ->
                                                                  fun l ->
                                                                   (find p (
                                                                    (rev l)
                                                                    ))

                                                                let find_all =
                                                                 fun p ->
                                                                  fun l ->
                                                                   let rec findnext =
                                                                    fun dst ->
                                                                    function
                                                                    | [] ->
                                                                    ()
                                                                    | 
                                                                    (h :: t) ->
                                                                    if 
                                                                    (p h) then
                                                                    (
                                                                    let r =
                                                                    {hd = h;
                                                                    tl = [] } in
                                                                    (
                                                                    dst.tl <-
                                                                    (inj r)
                                                                    );
                                                                    (findnext
                                                                    r t)
                                                                    )
                                                                    else
                                                                    (findnext
                                                                    dst t) in
                                                                   let dummy =
                                                                    (dummy_node
                                                                    () ) in
                                                                   (
                                                                   (findnext
                                                                    dummy l)
                                                                   );
                                                                   dummy.tl

                                                                let findi =
                                                                 fun p ->
                                                                  fun l ->
                                                                   let rec loop =
                                                                    fun n ->
                                                                    function
                                                                    | [] ->
                                                                    (raise
                                                                    Not_found
                                                                    )
                                                                    | 
                                                                    (h :: t) ->
                                                                    if 
                                                                    (p n h) then
                                                                    (n, h)
                                                                    else
                                                                    (loop (
                                                                    (n + 1) )
                                                                    t) in
                                                                   (loop 0 l)

                                                                let index_of =
                                                                 fun e ->
                                                                  fun l ->
                                                                   let rec loop =
                                                                    fun n ->
                                                                    function
                                                                    | [] ->
                                                                    (None)
                                                                    | (h ::
                                                                    _) when
                                                                    (h = e) ->
                                                                    (
                                                                    Some (n))
                                                                    | 
                                                                    (_ :: t) ->
                                                                    (loop (
                                                                    (n + 1) )
                                                                    t) in
                                                                   (loop 0 l)

                                                                let index_ofq =
                                                                 fun e ->
                                                                  fun l ->
                                                                   let rec loop =
                                                                    fun n ->
                                                                    function
                                                                    | [] ->
                                                                    (None)
                                                                    | (h ::
                                                                    _) when
                                                                    (h == e) ->
                                                                    (
                                                                    Some (n))
                                                                    | 
                                                                    (_ :: t) ->
                                                                    (loop (
                                                                    (n + 1) )
                                                                    t) in
                                                                   (loop 0 l)

                                                                let rindex_of =
                                                                 fun e ->
                                                                  fun l ->
                                                                   let rec loop =
                                                                    fun n ->
                                                                    fun acc ->
                                                                    function
                                                                    | [] ->
                                                                    acc
                                                                    | (h ::
                                                                    t) when
                                                                    (h = e) ->
                                                                    (loop (
                                                                    (n + 1) )
                                                                    (
                                                                    (Some (n))
                                                                    ) t)
                                                                    | 
                                                                    (_ :: t) ->
                                                                    (loop (
                                                                    (n + 1) )
                                                                    acc t) in
                                                                   (loop 0
                                                                    None  l)

                                                                let rindex_ofq =
                                                                 fun e ->
                                                                  fun l ->
                                                                   let rec loop =
                                                                    fun n ->
                                                                    fun acc ->
                                                                    function
                                                                    | [] ->
                                                                    acc
                                                                    | (h ::
                                                                    t) when
                                                                    (h == e) ->
                                                                    (loop (
                                                                    (n + 1) )
                                                                    (
                                                                    (Some (n))
                                                                    ) t)
                                                                    | 
                                                                    (_ :: t) ->
                                                                    (loop (
                                                                    (n + 1) )
                                                                    acc t) in
                                                                   (loop 0
                                                                    None  l)

                                                                let filter =
                                                                 find_all

                                                                let partition =
                                                                 fun p ->
                                                                  fun lst ->
                                                                   let rec loop =
                                                                    fun yesdst ->
                                                                    fun nodst ->
                                                                    function
                                                                    | [] ->
                                                                    ()
                                                                    | 
                                                                    (h :: t) ->
                                                                    let r =
                                                                    {hd = h;
                                                                    tl = [] } in
                                                                    if 
                                                                    (p h)
                                                                    then
                                                                     begin
                                                                    (
                                                                    yesdst.tl
                                                                    <-
                                                                    (inj r)
                                                                    );
                                                                    (loop r
                                                                    nodst t)
                                                                    end else begin
                                                                    (
                                                                    nodst.tl
                                                                    <-
                                                                    (inj r)
                                                                    );
                                                                    (loop
                                                                    yesdst r
                                                                    t)
                                                                    end in
                                                                   let yesdummy =
                                                                    (dummy_node
                                                                    () )
                                                                   and nodummy =
                                                                    (dummy_node
                                                                    () ) in
                                                                   (
                                                                   (loop
                                                                    yesdummy
                                                                    nodummy
                                                                    lst)
                                                                   );
                                                                   ((
                                                                    yesdummy.tl
                                                                    ), (
                                                                    nodummy.tl
                                                                    ))

                                                                let split =
                                                                 fun lst ->
                                                                  let rec loop =
                                                                   fun adst ->
                                                                    fun bdst ->
                                                                    function
                                                                    | [] ->
                                                                    ()
                                                                    | 
                                                                    ((a, b)
                                                                    :: t) ->
                                                                    let x =
                                                                    {hd = a;
                                                                    tl = [] }
                                                                    and y =
                                                                    {hd = b;
                                                                    tl = [] } in
                                                                    (
                                                                    adst.tl
                                                                    <-
                                                                    (inj x)
                                                                    );
                                                                    (
                                                                    bdst.tl
                                                                    <-
                                                                    (inj y)
                                                                    );
                                                                    (loop x y
                                                                    t) in
                                                                  let adummy =
                                                                   (dummy_node
                                                                    () )
                                                                  and bdummy =
                                                                   (dummy_node
                                                                    () ) in
                                                                  (
                                                                  (loop
                                                                    adummy
                                                                    bdummy
                                                                    lst)
                                                                  );
                                                                  ((
                                                                   adummy.tl
                                                                   ), (
                                                                   bdummy.tl
                                                                   ))

                                                                let combine =
                                                                 fun l1 ->
                                                                  fun l2 ->
                                                                   let rec loop =
                                                                    fun dst ->
                                                                    fun l1 ->
                                                                    fun l2 ->
                                                                    (
                                                                    match
                                                                    (l1, l2) with
                                                                    | ([], []) ->
                                                                    ()
                                                                    | ((h1 ::
                                                                    t1),
                                                                    (h2 ::
                                                                    t2)) ->
                                                                    let r =
                                                                    {hd =
                                                                    (h1, h2);
                                                                    tl = [] } in
                                                                    (
                                                                    dst.tl <-
                                                                    (inj r)
                                                                    );
                                                                    (loop r
                                                                    t1 t2)
                                                                    | 
                                                                    (_, _) ->
                                                                    (invalid_arg
                                                                    "combine: Different_list_size")) in
                                                                   let dummy =
                                                                    (dummy_node
                                                                    () ) in
                                                                   (
                                                                   (loop
                                                                    dummy l1
                                                                    l2)
                                                                   );
                                                                   dummy.tl

                                                                let make =
                                                                 fun i ->
                                                                  fun x ->
                                                                   (
                                                                   if 
                                                                    (i < 0) then
                                                                    (
                                                                    (invalid_arg
                                                                    "List.make")
                                                                    )
                                                                   else ()
                                                                   );
                                                                   let rec loop =
                                                                    fun x ->
                                                                    fun acc ->
                                                                    function
                                                                    | 0 ->
                                                                    acc
                                                                    | 
                                                                    i ->
                                                                    (loop x (
                                                                    ( x ) ::
                                                                    acc  ) (
                                                                    (i - 1)
                                                                    )) in
                                                                   (loop x []
                                                                     i)

                                                                let mapi =
                                                                 fun f ->
                                                                  function
                                                                  | [] ->
                                                                    ([])
                                                                  | (h :: t) ->
                                                                    let rec loop =
                                                                    fun dst ->
                                                                    fun n ->
                                                                    function
                                                                    | [] ->
                                                                    ()
                                                                    | 
                                                                    (h :: t) ->
                                                                    let r =
                                                                    {hd = (
                                                                    (f n h) );
                                                                    tl = [] } in
                                                                    (
                                                                    dst.tl <-
                                                                    (inj r)
                                                                    );
                                                                    (loop r (
                                                                    (n + 1) )
                                                                    t) in
                                                                    let r =
                                                                    {hd = (
                                                                    (f 0 h) );
                                                                    tl = [] } in
                                                                    (
                                                                    (loop r 1
                                                                    t)
                                                                    );
                                                                    (inj r)

                                                                let iteri =
                                                                 fun f ->
                                                                  fun l ->
                                                                   let rec loop =
                                                                    fun n ->
                                                                    function
                                                                    | [] ->
                                                                    ()
                                                                    | 
                                                                    (h :: t) ->
                                                                    (
                                                                    (f n h)
                                                                    );
                                                                    (loop (
                                                                    (n + 1) )
                                                                    t) in
                                                                   (loop 0 l)

                                                                let first =
                                                                 hd

                                                                let rec last =
                                                                 function
                                                                 | [] ->
                                                                    (invalid_arg
                                                                    "Empty List")
                                                                 | (h :: []) ->
                                                                    h
                                                                 | (_ :: t) ->
                                                                    (last t)

                                                                let split_nth =
                                                                 fun index ->
                                                                  function
                                                                  | [] ->
                                                                    if 
                                                                    (index =
                                                                    0) then
                                                                    ([] , []
                                                                    )
                                                                    else
                                                                    (invalid_arg
                                                                    "Index past end of list")
                                                                  | ((h :: t) as
                                                                    l) ->
                                                                    if 
                                                                    (index =
                                                                    0) then
                                                                    ([] , l)
                                                                    else 
                                                                    if 
                                                                    (index <
                                                                    0) then
                                                                    (
                                                                    (invalid_arg
                                                                    "Negative index not allowed")
                                                                    )
                                                                    else
                                                                    let rec loop =
                                                                    fun n ->
                                                                    fun dst ->
                                                                    fun l ->
                                                                    if 
                                                                    (n = 0) then
                                                                    l
                                                                    else
                                                                    (match
                                                                    l with
                                                                    | [] ->
                                                                    (invalid_arg
                                                                    "Index past end of list")
                                                                    | 
                                                                    (h :: t) ->
                                                                    let r =
                                                                    {hd = h;
                                                                    tl = [] } in
                                                                    (
                                                                    dst.tl <-
                                                                    (inj r)
                                                                    );
                                                                    (loop (
                                                                    (n - 1) )
                                                                    r t)) in
                                                                    let r =
                                                                    {hd = h;
                                                                    tl = [] } in
                                                                    ((
                                                                    (inj r)
                                                                    ), (
                                                                    (loop (
                                                                    (index -
                                                                    1) ) r t)
                                                                    ))

                                                                let split_at =
                                                                 split_nth

                                                                let find_exn =
                                                                 fun f ->
                                                                  fun e ->
                                                                   fun l ->
                                                                    (
                                                                    try
                                                                    (find f
                                                                    l)
                                                                    with
                                                                    Not_found ->
                                                                    (raise e))

                                                                let remove =
                                                                 fun l ->
                                                                  fun x ->
                                                                   let rec loop =
                                                                    fun dst ->
                                                                    function
                                                                    | [] ->
                                                                    ()
                                                                    | 
                                                                    (h :: t) ->
                                                                    if 
                                                                    (x = h) then
                                                                    (
                                                                    dst.tl <-
                                                                    t
                                                                    )
                                                                    else
                                                                    let r =
                                                                    {hd = h;
                                                                    tl = [] } in
                                                                    (
                                                                    dst.tl <-
                                                                    (inj r)
                                                                    );
                                                                    (loop r
                                                                    t) in
                                                                   let dummy =
                                                                    (dummy_node
                                                                    () ) in
                                                                   (
                                                                   (loop
                                                                    dummy l)
                                                                   );
                                                                   dummy.tl

                                                                let remove_if =
                                                                 fun f ->
                                                                  fun lst ->
                                                                   let rec loop =
                                                                    fun dst ->
                                                                    function
                                                                    | [] ->
                                                                    ()
                                                                    | 
                                                                    (x :: l) ->
                                                                    if 
                                                                    (f x) then
                                                                    (
                                                                    dst.tl <-
                                                                    l
                                                                    )
                                                                    else
                                                                    let r =
                                                                    {hd = x;
                                                                    tl = [] } in
                                                                    (
                                                                    dst.tl <-
                                                                    (inj r)
                                                                    );
                                                                    (loop r
                                                                    l) in
                                                                   let dummy =
                                                                    (dummy_node
                                                                    () ) in
                                                                   (
                                                                   (loop
                                                                    dummy
                                                                    lst)
                                                                   );
                                                                   dummy.tl

                                                                let remove_all =
                                                                 fun l ->
                                                                  fun x ->
                                                                   let rec loop =
                                                                    fun dst ->
                                                                    function
                                                                    | [] ->
                                                                    ()
                                                                    | 
                                                                    (h :: t) ->
                                                                    if 
                                                                    (x = h) then
                                                                    (
                                                                    (loop dst
                                                                    t)
                                                                    )
                                                                    else
                                                                    let r =
                                                                    {hd = h;
                                                                    tl = [] } in
                                                                    (
                                                                    dst.tl <-
                                                                    (inj r)
                                                                    );
                                                                    (loop r
                                                                    t) in
                                                                   let dummy =
                                                                    (dummy_node
                                                                    () ) in
                                                                   (
                                                                   (loop
                                                                    dummy l)
                                                                   );
                                                                   dummy.tl

                                                                let transpose =
                                                                 function
                                                                 | [] -> ([])
                                                                 | (x :: []) ->
                                                                    (List.map
                                                                    (
                                                                    fun x ->
                                                                    [x] ) x)
                                                                 | (x :: xs) ->
                                                                    let heads =
                                                                    (List.map
                                                                    (
                                                                    fun x ->
                                                                    {hd = x;
                                                                    tl = [] }
                                                                    ) x) in
                                                                    let _list =
                                                                    (List.fold_left
                                                                    (
                                                                    fun acc ->
                                                                    fun x ->
                                                                    (List.map2
                                                                    (
                                                                    fun x ->
                                                                    fun xs ->
                                                                    let r =
                                                                    {hd = x;
                                                                    tl = [] } in
                                                                    (
                                                                    xs.tl <-
                                                                    (inj r)
                                                                    );
                                                                    r ) x
                                                                    acc) )
                                                                    heads xs) in
                                                                    (Obj.magic
                                                                    heads)

                                                                let assoc_inv =
                                                                 fun e ->
                                                                  fun l ->
                                                                   let rec aux =
                                                                    function
                                                                    | [] ->
                                                                    (raise
                                                                    Not_found
                                                                    )
                                                                    | ((a, b)
                                                                    :: _)
                                                                    when
                                                                    (b = e) ->
                                                                    a
                                                                    | 
                                                                    (_ :: t) ->
                                                                    (aux t) in
                                                                   (aux l)

                                                                let assq_inv =
                                                                 fun e ->
                                                                  fun l ->
                                                                   let rec aux =
                                                                    function
                                                                    | [] ->
                                                                    (raise
                                                                    Not_found
                                                                    )
                                                                    | ((a, b)
                                                                    :: _)
                                                                    when
                                                                    (b == e) ->
                                                                    a
                                                                    | 
                                                                    (_ :: t) ->
                                                                    (aux t) in
                                                                   (aux l)

                                                                let sort_unique =
                                                                 fun (type s)
                                                                 ->fun (cmp :
                                                                    (s ->
                                                                    (s ->
                                                                    int))) ->
                                                                    fun lst ->
                                                                    let (module
                                                                    M)
                                                                     =
                                                                    (mk_set
                                                                    ~cmp:cmp) in
                                                                    let open
                                                                    M in
                                                                    (elements
                                                                    & (
                                                                    (fold_left
                                                                    (
                                                                    (flip
                                                                    add) )
                                                                    empty
                                                                    lst) ))

                                                                let group =
                                                                 fun cmp ->
                                                                  fun lst ->
                                                                   let sorted =
                                                                    (List.sort
                                                                    cmp lst) in
                                                                   let fold =
                                                                    fun first ->
                                                                    fun rest ->
                                                                    (List.fold_left
                                                                    (
                                                                    fun 
                                                                    (acc,
                                                                    agr, last) ->
                                                                    fun elem ->
                                                                    if 
                                                                    ((
                                                                    (cmp last
                                                                    elem) ) =
                                                                    0) then
                                                                    (acc, (
                                                                    (
                                                                    elem ) ::
                                                                    agr  ),
                                                                    elem)
                                                                    else
                                                                    ((
                                                                    (
                                                                    agr ) ::
                                                                    acc  ), (
                                                                    [elem] ),
                                                                    elem) )
                                                                    ([] , (
                                                                    [first]
                                                                    ), first)
                                                                    rest) in
                                                                   (match
                                                                    sorted with
                                                                    | [] ->
                                                                    ([])
                                                                    | 
                                                                    (hd ::
                                                                    tl) ->
                                                                    let 
                                                                    (groups,
                                                                    lastgr, _) =
                                                                    (fold hd
                                                                    tl) in
                                                                    (List.rev_map
                                                                    List.rev
                                                                    (
                                                                    (
                                                                    lastgr ) ::
                                                                    groups 
                                                                    )))

                                                                let cross_product =
                                                                 fun l1 ->
                                                                  fun l2 ->
                                                                   (List.concat
                                                                    (
                                                                    (List.map
                                                                    (
                                                                    fun i ->
                                                                    (List.map
                                                                    (
                                                                    fun j ->
                                                                    (i, j) )
                                                                    l2) ) l1)
                                                                    ))

                                                                let rec ncross_product =
                                                                 function
                                                                 | [] ->
                                                                    assert false
                                                                 | (l :: []) ->
                                                                    (List.map
                                                                    (
                                                                    fun i ->
                                                                    [i] ) l)
                                                                 | (h :: t) ->
                                                                    let rest =
                                                                    (ncross_product
                                                                    t) in
                                                                    (List.concat
                                                                    (
                                                                    (List.map
                                                                    (
                                                                    fun i ->
                                                                    (List.map
                                                                    (
                                                                    fun r ->
                                                                    ( i ) ::
                                                                    r  )
                                                                    rest) )
                                                                    h) ))

                                                                let fold_lefti =
                                                                 fun f ->
                                                                  fun init ->
                                                                   fun ls ->
                                                                    (fold_left
                                                                    (
                                                                    fun 
                                                                    (i, acc) ->
                                                                    fun x ->
                                                                    ((
                                                                    (i + 1)
                                                                    ), (
                                                                    (f i acc
                                                                    x) )) )
                                                                    (0, init)
                                                                    ls)

                                                                let reduce_left_with =
                                                                 fun ~compose ->
                                                                  fun ~map ->
                                                                   fun lst ->
                                                                    (
                                                                    match
                                                                    lst with
                                                                    | [] ->
                                                                    (invalid_arg
                                                                    "reduce_left length zero")
                                                                    | 
                                                                    (x :: xs) ->
                                                                    let rec loop =
                                                                    fun x ->
                                                                    fun xs ->
                                                                    (
                                                                    match
                                                                    xs with
                                                                    | [] -> x
                                                                    | 
                                                                    (y :: ys) ->
                                                                    (loop (
                                                                    (compose
                                                                    x (
                                                                    (map y)
                                                                    )) ) ys)) in
                                                                    (loop (
                                                                    (map x) )
                                                                    xs))

                                                                let reduce_left =
                                                                 fun compose ->
                                                                  (reduce_left_with
                                                                    ~compose:compose
                                                                    ~map:(
                                                                    fun x ->
                                                                    x ))

                                                                let reduce_right_with =
                                                                 fun ~compose ->
                                                                  fun ~map ->
                                                                   fun lst ->
                                                                    (
                                                                    match
                                                                    lst with
                                                                    | [] ->
                                                                    (invalid_arg
                                                                    "reduce_right length zero")
                                                                    | 
                                                                    xs ->
                                                                    let rec loop =
                                                                    fun xs ->
                                                                    (
                                                                    match
                                                                    xs with
                                                                    | [] ->
                                                                    assert false
                                                                    | (y ::
                                                                    []) ->
                                                                    (map y)
                                                                    | 
                                                                    (y :: ys) ->
                                                                    (compose
                                                                    ( (map y)
                                                                    ) (
                                                                    (loop ys)
                                                                    ))) in
                                                                    (loop xs))

                                                                let reduce_right =
                                                                 fun compose ->
                                                                  (reduce_right_with
                                                                    ~compose:compose
                                                                    ~map:(
                                                                    fun x ->
                                                                    x ))

                                                                let find_first =
                                                                 fun f ->
                                                                  fun lst ->
                                                                   let module M =
                                                                    struct
                                                                    exception
                                                                    First

                                                                    end in
                                                                   let res =
                                                                    (ref None
                                                                    ) in
                                                                   (try
                                                                    (
                                                                    (List.iter
                                                                    (
                                                                    fun x ->
                                                                    (
                                                                    match
                                                                    (f x) with
                                                                    | Some
                                                                    (v) ->
                                                                    (
                                                                    (res := (
                                                                    (Some (v))
                                                                    ))
                                                                    );
                                                                    (raise
                                                                    M.First )
                                                                    | 
                                                                    None ->
                                                                    ()) )
                                                                    lst)
                                                                    );
                                                                    (
                                                                    None)
                                                                    with
                                                                    M.First ->
                                                                    !res)

                                                                let rec filter_map =
                                                                 fun f ->
                                                                  function
                                                                  | [] ->
                                                                    ([])
                                                                  | (x :: xs) ->
                                                                    (
                                                                    match
                                                                    (f x) with
                                                                    | Some
                                                                    (v) ->
                                                                    ( v ) ::
                                                                    (filter_map
                                                                    f xs) 
                                                                    | 
                                                                    None ->
                                                                    (filter_map
                                                                    f xs))

                                                                let find_first_result =
                                                                 fun preds ->
                                                                  fun v ->
                                                                   (match
                                                                    (find_first
                                                                    (
                                                                    fun pred ->
                                                                    (pred v)
                                                                    ) preds) with
                                                                    | None ->
                                                                    (failwith
                                                                    "find_first_result")
                                                                    | 
                                                                    Some (r) ->
                                                                    r)

                                                                let init =
                                                                 fun n ->
                                                                  fun f ->
                                                                   let open
                                                                   Array in
                                                                   if 
                                                                    (n < 0) then
                                                                    (
                                                                    (invalid_arg
                                                                    "List.init <0")
                                                                    )
                                                                   else
                                                                    ((
                                                                    (init n
                                                                    f) ) |>
                                                                    to_list)

                                                                let rec drop =
                                                                 fun n ->
                                                                  function
                                                                  | (_ :: l)
                                                                    when
                                                                    (n > 0) ->
                                                                    (drop (
                                                                    (n - 1) )
                                                                    l)
                                                                  | l -> l

                                                                let lastbut1 =
                                                                 fun ls ->
                                                                  (match
                                                                    ls with
                                                                   | [] ->
                                                                    (failwith
                                                                    "lastbut1 empty")
                                                                   | 
                                                                   _ ->
                                                                    let l =
                                                                    (List.rev
                                                                    ls) in
                                                                    ((
                                                                    (List.tl
                                                                    l) ), (
                                                                    (List.hd
                                                                    l) )))

                                                                let range_up =
                                                                 fun ?
                                                                  (from = 0) ->
                                                                  fun ?
                                                                   (step = 1) ->
                                                                   fun until ->
                                                                    let rec loop =
                                                                    fun dst ->
                                                                    fun i ->
                                                                    if 
                                                                    (i >
                                                                    until) then
                                                                    ()
                                                                    
                                                                    else
                                                                    let cell =
                                                                    {hd = i;
                                                                    tl = [] } in
                                                                    (
                                                                    dst.tl <-
                                                                    (inj
                                                                    cell)
                                                                    );
                                                                    (loop
                                                                    cell (
                                                                    (i +
                                                                    step) )) in
                                                                    if 
                                                                    (from >
                                                                    until) then
                                                                    []
                                                                    
                                                                    else
                                                                    let res =
                                                                    {hd =
                                                                    from;
                                                                    tl = [] } in
                                                                    (
                                                                    (loop res
                                                                    (
                                                                    (from +
                                                                    step) ))
                                                                    );
                                                                    (inj res)

                                                                let range_down =
                                                                 fun ?
                                                                  (until = 0) ->
                                                                  fun ?
                                                                   (step = 1) ->
                                                                   fun from ->
                                                                    let rec loop =
                                                                    fun dst ->
                                                                    fun i ->
                                                                    if 
                                                                    (i <
                                                                    until) then
                                                                    ()
                                                                    
                                                                    else
                                                                    let cell =
                                                                    {hd = i;
                                                                    tl = [] } in
                                                                    (
                                                                    dst.tl <-
                                                                    (inj
                                                                    cell)
                                                                    );
                                                                    (loop
                                                                    cell (
                                                                    (i -
                                                                    step) )) in
                                                                    if 
                                                                    (from <
                                                                    until) then
                                                                    []
                                                                    
                                                                    else
                                                                    let res =
                                                                    {hd =
                                                                    from;
                                                                    tl = [] } in
                                                                    (
                                                                    (loop res
                                                                    (
                                                                    (from -
                                                                    step) ))
                                                                    );
                                                                    (inj res)

                                                                let ( @ ) =
                                                                 append

                                                                let ( /@ ) =
                                                                 fun lst ->
                                                                  fun f ->
                                                                   (map f
                                                                    lst)

                                                                let ( @/ ) =
                                                                 map

                                                                let ( // ) =
                                                                 fun lst ->
                                                                  fun f ->
                                                                   (filter f
                                                                    lst)

                                                                let ( //@ ) =
                                                                 fun lst ->
                                                                  fun f ->
                                                                   (filter_map
                                                                    f lst)

                                                                let ( @// ) =
                                                                 filter_map

                                                               end

include List


module ErrorMonad =
 struct
  type log = string

  type 'a result = Left of 'a | Right of log

  let return = fun x -> (Left (x))

  let fail = fun x -> (Right (x))

  let ( >>= ) =
   fun ma ->
    fun f ->
     (match ma with | Left (v) -> (f v) | Right (x) -> (Right (x)))

  let bind = (>>=)

  let map =
   fun f ->
    function | Left (v) -> (Left (f v)) | Right (s) -> (Right (s))

  let ( >>| ) =
   fun ma ->
    fun (str, f) ->
     (match ma with | Left (v) -> (f v) | Right (x) -> (Right (x ^ str)))

  let ( >>? ) =
   fun ma ->
    fun str ->
     (match ma with | Left (_) -> ma | Right (x) -> (Right (x ^ str)))

  let ( <|> ) =
   fun fa ->
    fun fb ->
     fun a ->
      (match (fa a) with
       | (Left (_) as x) -> x
       | Right (str) -> (( (fb a) ) >>? str))

  let unwrap =
   fun f ->
    fun a ->
     (match (f a) with
      | Left (res) -> res
      | Right (msg) -> (failwith msg))

  let mapi_m =
   fun f ->
    fun xs ->
     let rec aux =
      fun acc ->
       fun xs ->
        (match xs with
         | [] -> (return [] )
         | (x :: xs) ->
            (( (f x acc) ) >>= (
              fun x ->
               (( (aux ( (acc + 1) ) xs) ) >>= (
                 fun xs -> (return ( ( x ) :: xs  )) )) ))) in
     (aux 0 xs)

 end

module Option =
       struct
        let bind =
         fun o ->
          fun f -> (match o with | Some (x) -> (f x) | None -> (None))

        let map =
         fun f -> function | Some (x) -> (Some (f x)) | None -> (None)

        let adapt = fun f -> fun a -> (Some (f a))

       end

module Log =
             struct
              let verbose = (ref 1)

              let dprintf =
               fun ?(level = 1) ->
                if (( !verbose ) > level) then eprintf
                else (ifprintf err_formatter)

              let info_printf = fun a -> (dprintf ~level:2 a)

              let warn_printf = fun a -> (dprintf ~level:1 a)

              let error_printf = fun a -> (dprintf ~level:0 a)

             end

module Char =
                   struct
                    include Char

                    let is_whitespace =
                     function
                     | (((((' ' | '\010') | '\013') | '\009') | '\026')
                        | '\012') ->
                        (true)
                     | _ -> (false)

                    let is_newline =
                     function
                     | ('\010' | '\013') -> (true)
                     | _ -> (false)

                    let is_digit =
                     function
                     | ('0'
                        | ('1'
                           | ('2'
                              | ('3'
                                 | ('4'
                                    | ('5' | ('6' | ('7' | ('8' | '9'))))))))) ->
                        (true)
                     | _ -> (false)

                    let is_uppercase =
                     fun c -> (( ('A' <= c) ) && ( (c <= 'Z') ))

                    let is_lowercase =
                     fun c -> (( ('a' <= c) ) && ( (c <= 'z') ))

                    let is_uppercase_latin1 =
                     fun c ->
                      (( (is_uppercase c) ) || (
                        (( (( ('\192' <= c) ) && ( (c <= '\214') )) ) ||
                          ( (( ('\216' <= c) ) && ( (c <= '\221') )) ))
                        ))

                    let is_lowercase_latin1 =
                     fun c ->
                      (( (is_lowercase c) ) || (
                        (( (( ('\222' <= c) ) && ( (c <= '\246') )) ) ||
                          ( (( ('\248' <= c) ) && ( (c <= '\255') )) ))
                        ))

                    let is_latin1 =
                     fun c ->
                      (( (is_uppercase_latin1 c) ) || (
                        (is_lowercase_latin1 c) ))

                    let is_symbol =
                     function
                     | (((((((((((((((((('!' | '%') | '&') | '$') | '#')
                                     | '+') | '-') | '/') | ':') | '<')
                                | '=') | '>') | '?') | '@') | '\\')
                           | '~') | '^') | '|') | '*') ->
                        (true)
                     | _ -> (false)

                    let is_letter =
                     fun c ->
                      (( (is_uppercase c) ) || ( (is_lowercase c) ))

                    external unsafe_int : (char -> int) = "%identity"

                    external unsafe_chr : (int -> char) = "%identity"

                    let is_capital =
                     fun c ->
                      let c = (code c) in
                      (( (c >= ( (code 'A') )) ) && (
                        (c <= ( (code 'Z') )) ))

                    let of_digit =
                     fun i ->
                      if (( (i >= 0) ) && ( (i < 10) )) then
                       (
                       (unsafe_chr ( (i + ( (code '0') )) ))
                       )
                      else (invalid_arg "Char.of_digit")

                   end

module String =
                         struct
                          include String

                          let init =
                           fun len ->
                            fun f ->
                             let s = (create len) in
                             for i = 0 to (len - 1) do
                              (unsafe_set s i ( (f i) ))
                             done;
                             s

                          let ends_with =
                           fun s ->
                            fun e ->
                             let ne = (String.length e)
                             and ns = (String.length s) in
                             (( (ns >= ne) ) && (
                               (callcc (
                                 fun k ->
                                  let diff = (ns - ne) in
                                  for i = 0 to (ne - 1) do
                                   if (( (unsafe_get s ( (diff + i) )) )
                                        <> ( (unsafe_get e i) )) then
                                    (
                                    (raise & ( (k false ) ))
                                    )
                                   else ()
                                  done;
                                  (true) )) ))

                          let starts_with =
                           fun s ->
                            fun e ->
                             let ne = (length e) in
                             let ns = (length s) in
                             (( (ns >= ne) ) && (
                               (callcc (
                                 fun k ->
                                  for i = 0 to (ne - 1) do
                                   if (( (unsafe_get s i) ) <> (
                                        (unsafe_get e i) )) then
                                    (
                                    (raise & ( (k false ) ))
                                    )
                                   else ()
                                  done;
                                  (true) )) ))

                          let neg =
                           fun n ->
                            let len = (String.length n) in
                            if (( (len > 0) ) && (
                                 (( (String.get n 0) ) = '-') )) then
                             (
                             (String.sub n 1 ( (len - 1) ))
                             )
                            else ("-" ^ n)

                          let drop_while =
                           fun f ->
                            fun s ->
                             let len = (String.length s) in
                             let found = (ref false ) in
                             let i = (ref 0) in
                             while
                              (( (( !i ) < len) ) && ( (not ( !found ))
                                )) do
                              if (not ( (f ( (String.get s ( !i )) )) )) then
                               (
                               (found := true )
                               )
                              else (incr i)
                             done;
                             (String.sub s ( !i ) ( (len - ( !i )) ))

                          let find_from =
                           fun str ->
                            fun pos ->
                             fun sub ->
                              let len = (length str) in
                              let sublen = (length sub) in
                              (
                              if (( (pos < 0) ) || ( (pos > len) )) then
                               (
                               (raise (
                                 (Invalid_argument ("String.find_from"))
                                 ))
                               )
                              else ()
                              );
                              if (sublen = 0) then pos
                              else
                               (callcc (
                                 fun k ->
                                  for i = pos to (len - sublen) do
                                   let j = (ref 0) in
                                   while
                                    (( (unsafe_get str ( (i + ( !j )) ))
                                      ) = ( (unsafe_get sub ( !j )) )) do
                                    (
                                   (incr j)
                                   );
                                    if (( !j ) = sublen) then
                                     (
                                     (raise & ( (k i) ))
                                     )
                                    else ()
                                   done
                                  done;
                                  (raise Not_found ) ))

                          let find =
                           fun str -> fun sub -> (find_from str 0 sub)

                          let rfind_from =
                           fun str ->
                            fun pos ->
                             fun sub ->
                              let sublen = (length sub)
                              and len = (length str) in
                              (
                              if (( (( (pos + 1) ) < 0) ) || (
                                   (( (pos + 1) ) > len) )) then
                               (
                               (raise (
                                 (Invalid_argument ("String.rfind_from"))
                                 ))
                               )
                              else ()
                              );
                              if (sublen = 0) then ( (pos + 1) )
                              else
                               (callcc (
                                 fun k ->
                                  for i = (( (pos - sublen) ) + 1) downto
                                   0 do
                                   let j = (ref 0) in
                                   while
                                    (( (unsafe_get str ( (i + ( !j )) ))
                                      ) = ( (unsafe_get sub ( !j )) )) do
                                    (
                                   (incr j)
                                   );
                                    if (( !j ) = sublen) then
                                     (
                                     (raise & ( (k i) ))
                                     )
                                    else ()
                                   done
                                  done;
                                  (raise Not_found ) ))

                          let rfind =
                           fun str ->
                            fun sub ->
                             (rfind_from str (
                               (( (String.length str) ) - 1) ) sub)

                          let strip =
                           fun ?(chars = " \t\r\n") ->
                            fun s ->
                             let p = (ref 0) in
                             let l = (length s) in
                             while
                              (( (( !p ) < l) ) && (
                                (contains chars ( (unsafe_get s ( !p ))
                                  )) )) do
                              (incr p)
                             done;
                             let p = !p in
                             let l = (ref ( (l - 1) )) in
                             while
                              (( (( !l ) >= p) ) && (
                                (contains chars ( (unsafe_get s ( !l ))
                                  )) )) do
                              (decr l)
                             done;
                             (sub s p ( (( (( !l ) - p) ) + 1) ))

                          let exists =
                           fun str ->
                            fun sub ->
                             (try
                               (
                              (ignore ( (find str sub) ))
                              );
                               (true)
                              with
                              Not_found -> (false))

                          let left =
                           fun s ->
                            fun len ->
                             if (len >= ( (length s) )) then s
                             else (sub s 0 len)

                          let right =
                           fun s ->
                            fun len ->
                             let slen = (length s) in
                             if (len >= slen) then s
                             else (sub s ( (slen - len) ) len)

                          let head = fun s -> fun pos -> (left s pos)

                          let tail =
                           fun s ->
                            fun pos ->
                             let slen = (length s) in
                             if (pos >= slen) then ""
                             else (sub s pos ( (slen - pos) ))

                          let split =
                           fun str ->
                            fun ~by:sep ->
                             let p = (find str sep) in
                             let len = (length sep) in
                             let slen = (length str) in
                             (( (sub str 0 p) ), (
                              (sub str ( (p + len) ) (
                                (( (slen - p) ) - len) )) ))

                          let rsplit =
                           fun str ->
                            fun ~by:sep ->
                             let p = (rfind str sep) in
                             let len = (length sep) in
                             let slen = (length str) in
                             (( (sub str 0 p) ), (
                              (sub str ( (p + len) ) (
                                (( (slen - p) ) - len) )) ))

                          let nsplit =
                           fun str ->
                            fun ~by:sep ->
                             if (str = "") then [] 
                             else if (sep = "") then
                                   (
                                   (invalid_arg
                                     "nsplit: empty sep not allowed")
                                   )
                             else
                              let seplen = (String.length sep) in
                              let rec aux =
                               fun acc ->
                                fun ofs ->
                                 if (ofs >= 0) then
                                  (
                                  (match
                                     (try
                                       (Some (rfind_from str ofs sep))
                                      with
                                      Not_found -> (None)) with
                                   | Some (idx) ->
                                      let end_of_sep =
                                       (( (idx + seplen) ) - 1) in
                                      if (end_of_sep = ofs) then
                                       (
                                       (aux ( ( "" ) :: acc  ) (
                                         (idx - 1) ))
                                       )
                                      else
                                       let token =
                                        (sub str ( (end_of_sep + 1) ) (
                                          (ofs - end_of_sep) )) in
                                       (aux ( ( token ) :: acc  ) (
                                         (idx - 1) ))
                                   | None ->
                                      ( ( (sub str 0 ( (ofs + 1) ))
                                       ) ) :: acc )
                                  )
                                 else ( "" ) :: acc  in
                              (aux []  ( (( (length str) ) - 1) ))

                          let join = concat

                          let unsafe_slice =
                           fun i ->
                            fun j ->
                             fun s ->
                              if (( (i >= j) ) || ( (i = ( (length s) ))
                                   )) then
                               (
                               (create 0)
                               )
                              else (sub s i ( (j - i) ))

                          let clip =
                           fun ~lo ->
                            fun ~hi ->
                             fun (x :
                               int) ->
                              if (x < lo) then lo
                              else if (x > hi) then hi
                              else x

                          let wrap =
                           fun (x :
                             int) ->
                            fun ~hi ->
                             if (x < 0) then ( (hi + x) ) else x

                          let slice =
                           fun ?(first = 0) ->
                            fun ?(last = Sys.max_string_length) ->
                             fun s ->
                              let lo = 0
                              and hi = (length s) in
                              let i =
                               (clip ~lo:lo ~hi:hi ( (wrap first ~hi:hi)
                                 )) in
                              let j =
                               (clip ~lo:lo ~hi:hi ( (wrap last ~hi:hi)
                                 )) in
                              (unsafe_slice i j s)

                          let lchop =
                           fun ?(n = 1) ->
                            fun s ->
                             if (n < 0) then
                              (
                              (invalid_arg
                                "lchop: number of characters to chop is negative")
                              )
                             else
                              let slen = (length s) in
                              if (slen <= n) then ""
                              else (sub s n ( (slen - n) ))

                          let rchop =
                           fun ?(n = 1) ->
                            fun s ->
                             if (n < 0) then
                              (
                              (invalid_arg
                                "rchop: number of characters to chop is negative")
                              )
                             else
                              let slen = (length s) in
                              if (slen <= n) then ""
                              else (sub s 0 ( (slen - n) ))

                          let of_int = string_of_int

                          let of_float = string_of_float

                          let of_char = (make 1)

                          let to_int = fun s -> (int_of_string s)

                          let to_float = fun s -> (float_of_string s)

                         end

exception Not_implemented

module Ref =
                                                          struct
                                                           type 'a t =
                                                            'a ref

                                                           let post =
                                                            fun r ->
                                                             fun f ->
                                                              let old =
                                                               !r in
                                                              (
                                                              (r := (
                                                                (f old)
                                                                ))
                                                              );
                                                              old

                                                           let pre =
                                                            fun r ->
                                                             fun f ->
                                                              (
                                                              (r := (
                                                                (f ( !r
                                                                  )) ))
                                                              );
                                                              !r

                                                           let modify =
                                                            fun r ->
                                                             fun f ->
                                                              (r := (
                                                                (f ( !r
                                                                  )) ))

                                                           let swap =
                                                            fun a ->
                                                             fun b ->
                                                              let buf =
                                                               !a in
                                                              (
                                                              (a := ( !b
                                                                ))
                                                              );
                                                              (b := buf)

                                                           let pre_incr =
                                                            fun r ->
                                                             (pre r (
                                                               ((+) 1) ))

                                                           let pre_decr =
                                                            fun r ->
                                                             (pre r (
                                                               ((+) (-1))
                                                               ))

                                                           let post_incr =
                                                            fun r ->
                                                             (post r (
                                                               ((+) 1) ))

                                                           let post_decr =
                                                            fun r ->
                                                             (post r (
                                                               ((+) (-1))
                                                               ))

                                                           let copy =
                                                            fun r ->
                                                             (ref ( !r ))

                                                           let protect =
                                                            fun r ->
                                                             fun v ->
                                                              fun body ->
                                                               let old =
                                                                !r in
                                                               (try
                                                                 (
                                                                (r := v)
                                                                );
                                                                 let res =
                                                                  (body
                                                                    () ) in
                                                                 (
                                                                 (r :=
                                                                   old)
                                                                 );
                                                                 res
                                                                with
                                                                x ->
                                                                 (
                                                                 (r :=
                                                                   old)
                                                                 );
                                                                 (raise
                                                                   x))

                                                           external ref :
                                                            ('a ->
                                                             'a ref) =
                                                             "%makemutable"

                                                           external
                                                            ( ! ) :
                                                            ('a ref ->
                                                             'a) =
                                                             "%field0"

                                                           external
                                                            ( := ) :
                                                            ('a ref ->
                                                             ('a -> unit)) =
                                                             "%setfield0"

                                                           external set :
                                                            ('a ref ->
                                                             ('a -> unit)) =
                                                             "%setfield0"

                                                           external get :
                                                            ('a ref ->
                                                             'a) =
                                                             "%field0"

                                                           let print =
                                                            fun print_a ->
                                                             fun out ->
                                                              fun r ->
                                                               (print_a
                                                                 out ( !r
                                                                 ))

                                                           let toggle =
                                                            fun r ->
                                                             (r := (
                                                               (not ( !r
                                                                 )) ))

                                                           let oset =
                                                            fun r ->
                                                             fun x ->
                                                              (r := (
                                                                (Some (x))
                                                                ))

                                                           let oget_exn =
                                                            fun r ->
                                                             (match
                                                                !r with
                                                              | None ->
                                                                 (raise
                                                                   Not_found
                                                                   )
                                                              | Some (x) ->
                                                                 x)

                                                           let ord =
                                                            fun o ->
                                                             fun x ->
                                                              fun y ->
                                                               (o ( !x )
                                                                 ( !y ))

                                                           let eq =
                                                            fun e ->
                                                             fun x ->
                                                              fun y ->
                                                               (e ( !x )
                                                                 ( !y ))

                                                          end

module Buffer =
                                                                struct
                                                                 include Buffer

                                                                 module Ops =
                                                                  struct
                                                                   let ( +> ) =
                                                                    fun buf ->
                                                                    fun chr ->
                                                                    (
                                                                    (Buffer.add_char
                                                                    buf
                                                                    chr)
                                                                    );
                                                                    buf

                                                                   let ( +>> ) =
                                                                    fun buf ->
                                                                    fun str ->
                                                                    (
                                                                    (Buffer.add_string
                                                                    buf
                                                                    str)
                                                                    );
                                                                    buf

                                                                  end

                                                                end


module Hashtbl =
 struct
  include Hashtbl

  let keys =
   fun tbl ->
    (Hashtbl.fold ( fun k -> fun _v -> fun acc -> ( k ) :: acc  ) tbl []
      )

  let lets =
   fun tbl ->
    (Hashtbl.fold ( fun _k -> fun v -> fun acc -> ( v ) :: acc  ) tbl []
      )

 end

module SMap = (Map.Make)(String)

module SSet = (Set.Make)(String)


let ( |> ) = fun x -> fun f -> (f x)

let ( & ) = fun f -> fun x -> (f x)


type log = string

type 'a result = Left of 'a | Right of log

let ret =
                                                              fun x ->
                                                               (Left
                                                                 (x))


let right = fun x -> (Right (x))

let ( >>= ) =
                                   fun ma ->
                                    fun f ->
                                     (match ma with
                                      | Left (v) -> (f v)
                                      | Right (x) -> (Right (x)))

let ( >>| ) =
                                                                    fun ma ->
                                                                    fun 
                                                                    (str,
                                                                    f) ->
                                                                    (
                                                                    match
                                                                    ma with
                                                                    | Left
                                                                    (v) ->
                                                                    (f
                                                                    v)
                                                                    | 
                                                                    Right
                                                                    (x) ->
                                                                    (
                                                                    Right
                                                                    (x
                                                                    ^
                                                                    str)))


let ( |- ) = fun f -> fun g -> fun x -> (g ( (f x) ))

let ( >>? ) =
                                                        fun ma ->
                                                         fun str ->
                                                          (match
                                                             ma with
                                                           | Left (_) ->
                                                              ma
                                                           | Right
                                                              (x) ->
                                                              (Right
                                                                (x ^
                                                                  str)))


let ( <|> ) =
 fun fa ->
  fun fb ->
   fun a ->
    (match (fa a) with
     | (Left (_) as x) -> x
     | Right (str) -> (( (fb a) ) >>? str))

let unwrap =
                                              fun f ->
                                               fun a ->
                                                (match (f a) with
                                                 | Left (res) -> res
                                                 | Right (msg) ->
                                                    (invalid_arg msg))


let failwithf = fun fmt -> (ksprintf failwith fmt)

let opt_bind =
                                                     fun o ->
                                                      fun f ->
                                                       (match o with
                                                        | Some (x) ->
                                                           (f x)
                                                        | None ->
                                                           (None))

let opt_map =
                                                                    fun o ->
                                                                    fun f ->
                                                                    (
                                                                    match
                                                                    o with
                                                                    | Some
                                                                    (x) ->
                                                                    (
                                                                    Some
                                                                    (f,
                                                                    x))
                                                                    | 
                                                                    None ->
                                                                    (
                                                                    None))


let prerr_endlinef = fun fmt -> (ksprintf prerr_endline fmt)

let verbose =
                                                               (ref
                                                                 1)


let dprintf =
 fun ?(log_level = 1) ->
  if (( !verbose ) >= log_level) then eprintf
  else (ifprintf err_formatter)

let ends_with =
                                  fun s ->
                                   fun e ->
                                    let ne = (String.length e)
                                    and ns = (String.length s) in
                                    (( (ns >= ne) ) && (
                                      ((
                                        (String.sub s ( (ns - ne) )
                                          ne) ) = e) ))

let starts_with =
                                                          fun s ->
                                                           fun e ->
                                                            let ne =
                                                             (String.length
                                                               e)
                                                            and ns =
                                                             (String.length
                                                               s) in
                                                            ((
                                                              (ns >=
                                                                ne) )
                                                              && (
                                                              ((
                                                                (String.sub
                                                                  s 0
                                                                  ne)
                                                                ) =
                                                                e) ))


let is_antiquot_data_ctor = fun s -> (ends_with s "Ant")

let neg_string =
                                                           fun n ->
                                                            let len =
                                                             (String.length
                                                               n) in
                                                            if ((
                                                                 (len
                                                                   >
                                                                   0)
                                                                 ) &&
                                                                 (
                                                                 ((
                                                                   (String.get
                                                                    n
                                                                    0)
                                                                   )
                                                                   =
                                                                   '-')
                                                                 )) then
                                                             (
                                                             (String.sub
                                                               n 1 (
                                                               (len -
                                                                 1)
                                                               ))
                                                             )
                                                            else
                                                             ("-" ^
                                                               n)

let fold_lefti =
                                                                    fun (f :
                                                                    ('a
                                                                    ->
                                                                    (int
                                                                    ->
                                                                    ('b
                                                                    ->
                                                                    'b)))) ->
                                                                    fun (ty :
                                                                    'a list) ->
                                                                    fun (init :
                                                                    'b) ->
                                                                    (let 
                                                                    (_,
                                                                    res) =
                                                                    (List.fold_left
                                                                    (
                                                                    fun 
                                                                    (i,
                                                                    acc) ->
                                                                    fun ty ->
                                                                    ((
                                                                    (i
                                                                    +
                                                                    1)
                                                                    ),
                                                                    (
                                                                    (f
                                                                    ty
                                                                    i
                                                                    acc)
                                                                    ))
                                                                    )
                                                                    (0,
                                                                    init)
                                                                    ty) in
                                                                    res :
                                                                    'b)


let mapi_m =
 fun f ->
  fun xs ->
   let rec aux =
    fun acc ->
     fun xs ->
      (match xs with
       | [] -> (ret [] )
       | (x :: xs) ->
          (( (f x acc) ) >>= (
            fun x ->
             (( (aux ( (acc + 1) ) xs) ) >>= (
               fun xs -> (ret ( ( x ) :: xs  )) )) ))) in
   (aux 0 xs)

let mapi =
                fun f ->
                 fun xs ->
                  let rec aux =
                   fun acc ->
                    fun xs ->
                     (match xs with
                      | [] -> ([])
                      | (x :: xs) ->
                         ( ( (f x acc) ) ) :: (aux ( (acc + 1) ) xs) ) in
                  (aux 0 xs)

let fold_nat_left =
                               fun ?(start = 0) ->
                                fun ~until ->
                                 fun ~acc ->
                                  fun f ->
                                   let v = (ref acc) in
                                   for x = start to until do
                                    (v := ( (f ( !v ) x) ))
                                   done;
                                   !v

let iteri =
                                        fun f ->
                                         fun lst ->
                                          let i = (ref 0) in
                                          (List.iter (
                                            fun x ->
                                             let () = (f ( !i ) x) in
                                             (incr i) ) lst)

type dir =
                                                               [ `Left |
                                                                `Right ]


let reduce_left =
 fun f ->
  fun lst ->
   (match lst with
    | [] -> (invalid_arg "reduce_left length zero")
    | (x :: xs) ->
       let rec loop =
        fun x ->
         fun xs ->
          (match xs with
           | [] -> x
           | (y :: ys) -> (loop ( (f x y) ) ys)) in
       (loop x xs))

let reduce_right_with =
                      fun ~compose ->
                       fun ~f ->
                        fun lst ->
                         (match lst with
                          | [] ->
                             (invalid_arg "reduce_right length zero")
                          | xs ->
                             let rec loop =
                              fun xs ->
                               (match xs with
                                | [] -> assert false
                                | (y :: []) -> (f y)
                                | (y :: ys) ->
                                   (compose ( (f y) ) ( (loop ys) ))) in
                             (loop xs))

let reduce_right =
                                          fun compose ->
                                           (reduce_right_with
                                             ~compose:compose ~f:(
                                             fun x -> x ))

let string_drop_while =
                                                             fun f ->
                                                              fun s ->
                                                               let len =
                                                                (String.length
                                                                  s) in
                                                               let found =
                                                                (ref
                                                                  false
                                                                  ) in
                                                               let i =
                                                                (ref
                                                                  0) in
                                                               while
                                                                ((
                                                                  ((
                                                                    !i
                                                                    )
                                                                    <
                                                                    len)
                                                                  )
                                                                  &&
                                                                  (
                                                                  (not
                                                                    (
                                                                    !found
                                                                    ))
                                                                  )) do
                                                                if (not
                                                                    (
                                                                    (f
                                                                    (
                                                                    (String.get
                                                                    s
                                                                    (
                                                                    !i
                                                                    ))
                                                                    ))
                                                                    )) then
                                                                 (
                                                                 (found
                                                                   :=
                                                                   true
                                                                   )
                                                                 )
                                                                else
                                                                 (incr
                                                                   i)
                                                               done;
                                                               (String.sub
                                                                 s (
                                                                 !i )
                                                                 (
                                                                 (len
                                                                   -
                                                                   (
                                                                   !i
                                                                   ))
                                                                 ))


exception First

let find_first =
                  fun f ->
                   fun lst ->
                    let res = (ref None ) in
                    (try
                      (
                     (List.iter (
                       fun x ->
                        (match (f x) with
                         | Some (v) ->
                            (
                            (res := ( (Some (v)) ))
                            );
                            (raise First )
                         | None -> ()) ) lst)
                     );
                      (None)
                     with
                     First -> !res)

let mkmke =
                                      fun preds ->
                                       fun (cons, tyargs) ->
                                        (match
                                           (find_first (
                                             fun pred ->
                                              (pred (cons, tyargs)) )
                                             preds) with
                                         | None ->
                                            (invalid_arg "mkmke")
                                         | Some (r) -> r)

let adapt =
                                                            fun f ->
                                                             fun a ->
                                                              (Some
                                                                (f a))


let rec intersperse =
 fun y ->
  fun xs ->
   (match xs with
    | [] -> ([])
    | (_x :: []) -> xs
    | (x :: xs) -> ( x ) :: y :: (intersperse y xs) )

let init =
                                                        fun n ->
                                                         fun f ->
                                                          let open
                                                          Array in
                                                          ((
                                                            (init n
                                                              f) ) |>
                                                            to_list)


let to_string_of_printer =
 fun printer ->
  fun v ->
   let buf = (Buffer.create 30) in
   let () = (Format.bprintf buf "@[%a@]" printer v) in
   (Buffer.contents buf)

let mk_anti =
                           fun ?(c = "") ->
                            fun n ->
                             fun s ->
                              ("\\$" ^ (
                                (n ^ ( (c ^ ( (":" ^ s) )) )) ))

let is_antiquot =
                                                                   fun s ->
                                                                    let len =
                                                                    (String.length
                                                                    s) in
                                                                    ((
                                                                    (len
                                                                    >
                                                                    2)
                                                                    )
                                                                    &&
                                                                    (
                                                                    ((
                                                                    ((
                                                                    (String.get
                                                                    s
                                                                    0)
                                                                    )
                                                                    =
                                                                    '\\')
                                                                    )
                                                                    &&
                                                                    (
                                                                    ((
                                                                    (String.get
                                                                    s
                                                                    1)
                                                                    )
                                                                    =
                                                                    '$')
                                                                    ))
                                                                    ))


let handle_antiquot_in_string =
 fun ~s ->
  fun ~term ->
   fun ~parse ->
    fun ~loc ->
     fun ~decorate ->
      if (is_antiquot s) then
       (
       let pos = (String.index s ':') in
       let name = (String.sub s 2 ( (pos - 2) ))
       and code =
        (String.sub s ( (pos + 1) ) (
          (( (( (String.length s) ) - pos) ) - 1) )) in
       (decorate name ( (parse loc code) ))
       )
      else term

let is_capital =
                  fun c ->
                   let c = (Char.code c) in
                   (( (c >= ( (Char.code 'A') )) ) && (
                     (c <= ( (Char.code 'Z') )) ))

let is_digit =
                                                     fun c ->
                                                      let c =
                                                       (Char.code c) in
                                                      ((
                                                        (c >= (
                                                          (Char.code
                                                            '0') )) )
                                                        && (
                                                        (c <= (
                                                          (Char.code
                                                            '9') ))
                                                        ))

let destruct_poly =
                                                             fun s ->
                                                              let n =
                                                               (String.length
                                                                 s) in
                                                              if (n =
                                                                   0) then
                                                               (
                                                               (invalid_arg
                                                                 "destruct_poly length=0")
                                                               )
                                                              else if 
                                                                    ((
                                                                    (String.get
                                                                    s
                                                                    0)
                                                                    )
                                                                    =
                                                                    '`') then
                                                                    (
                                                                    (
                                                                    Some
                                                                    (String.sub
                                                                    s
                                                                    1
                                                                    (
                                                                    (n
                                                                    -
                                                                    1)
                                                                    )))
                                                                    )
                                                              else
                                                               (None)


let uncurry = fun f -> fun (x, y) -> (f x y)

let rec drop =
                                               fun n ->
                                                function
                                                | (_ :: l) when
                                                   (n > 0) ->
                                                   (drop ( (n - 1) )
                                                     l)
                                                | l -> l

let lastbut1 =
                                                           fun ls ->
                                                            (match
                                                               ls with
                                                             | [] ->
                                                                (failwith
                                                                  "lastbut1 empty")
                                                             | _ ->
                                                                let l =
                                                                 (List.rev
                                                                   ls) in
                                                                ((
                                                                 (List.tl
                                                                   l)
                                                                 ), (
                                                                 (List.hd
                                                                   l)
                                                                 )))
