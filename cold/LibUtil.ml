include Format

module SMap = (Map.Make)(String)

module SSet =
                                                   (Set.Make)(String)


let (|>) = fun x -> fun f -> (f x)

let (&) = fun f -> fun x -> (f x)


type log = string

type 'a result = Left of 'a | Right of log

let ret =
                                                              fun x ->
                                                               (Left (x))


let right = fun x -> (Right (x))

let (>>=) =
                                   fun ma ->
                                    fun f ->
                                     (match ma with
                                      | Left (v) -> (f v)
                                      | Right (x) -> (Right (x)))

let (>>|) =
                                                                    fun ma ->
                                                                    fun 
                                                                    (str,
                                                                    f) ->
                                                                    (
                                                                    match
                                                                    ma with
                                                                    | Left
                                                                    (v) ->
                                                                    (f v)
                                                                    | 
                                                                    Right
                                                                    (x) ->
                                                                    (
                                                                    Right
                                                                    (x ^
                                                                    str)))


let (|-) = fun f -> fun g -> fun x -> (g ( (f x) ))

let (>>?) =
                                                      fun ma ->
                                                       fun str ->
                                                        (match ma with
                                                         | Left (_) -> ma
                                                         | Right (x) ->
                                                            (Right
                                                              (x ^ str)))


let (<|>) =
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
                                                        | None -> (None))


let opt_map =
 fun o ->
  fun f -> (match o with | Some (x) -> (Some (f, x)) | None -> (None))


let prerr_endlinef = fun fmt -> (ksprintf prerr_endline fmt)

let verbose =
                                                               (ref 1)


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
                                      (( (String.sub s ( (ns - ne) ) ne)
                                        ) = e) ))

let starts_with =
                                                    fun s ->
                                                     fun e ->
                                                      let ne =
                                                       (String.length e)
                                                      and ns =
                                                       (String.length s) in
                                                      (( (ns >= ne) ) &&
                                                        (
                                                        ((
                                                          (String.sub s 0
                                                            ne) ) = e) ))


let is_antiquot_data_ctor = fun s -> (ends_with s "Ant")

let neg_string =
                                                           fun n ->
                                                            let len =
                                                             (String.length
                                                               n) in
                                                            if ((
                                                                 (len >
                                                                   0) )
                                                                 && (
                                                                 ((
                                                                   (String.get
                                                                    n 0)
                                                                   ) =
                                                                   '-')
                                                                 )) then
                                                             (
                                                             (String.sub
                                                               n 1 (
                                                               (len - 1)
                                                               ))
                                                             )
                                                            else
                                                             ("-" ^ n)


let fold_lefti =
 fun (f :
   ('a -> (int -> ('b -> 'b)))) ->
  fun (ty :
    'a list) ->
   fun (init :
     'b) ->
    (let (_, res) =
      (List.fold_left (
        fun (i, acc) -> fun ty -> (( (i + 1) ), ( (f ty i acc) )) )
        (0, init) ty) in
     res : 'b)

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
                                         ( ( (f x acc) ) ) ::
                                          (aux ( (acc + 1) ) xs) ) in
                                  (aux 0 xs)

let fold_nat_left =
                                               fun ?(start = 0) ->
                                                fun ~until ->
                                                 fun ~acc ->
                                                  fun f ->
                                                   let v = (ref acc) in
                                                   for x = start to
                                                    until do
                                                    (v := ( (f ( !v ) x)
                                                      ))
                                                   done;
                                                   !v

let iteri =
                                                        fun f ->
                                                         fun lst ->
                                                          let i = (ref 0) in
                                                          (List.iter (
                                                            fun x ->
                                                             let () =
                                                              (f ( !i )
                                                                x) in
                                                             (incr i) )
                                                            lst)

type dir =
                                                                   [ 
                                                                    `Left |
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
          (match xs with | [] -> x | (y :: ys) -> (loop ( (f x y) ) ys)) in
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
                                                                  false ) in
                                                               let i =
                                                                (ref 0) in
                                                               while
                                                                ((
                                                                  (( !i )
                                                                    <
                                                                    len)
                                                                  ) && (
                                                                  (not (
                                                                    !found
                                                                    )) )) do
                                                                if (not (
                                                                    (f (
                                                                    (String.get
                                                                    s (
                                                                    !i ))
                                                                    )) )) then
                                                                 (
                                                                 (found
                                                                   :=
                                                                   true )
                                                                 )
                                                                else
                                                                 (incr i)
                                                               done;
                                                               (String.sub
                                                                 s ( !i )
                                                                 (
                                                                 (len - (
                                                                   !i ))
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
                            ( (res := ( (Some (v)) )) ); (raise First )
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
                                         | None -> (invalid_arg "mkmke")
                                         | Some (r) -> r)

let adapt =
                                                            fun f ->
                                                             fun a ->
                                                              (Some (f a))


let rec intersperse =
 fun y ->
  fun xs ->
   (match xs with
    | [] -> ([])
    | (x :: []) -> xs
    | (x :: xs) -> ( x ) :: y :: (intersperse y xs) )

let init =
                                                        fun n ->
                                                         fun f ->
                                                          let open
                                                          Array in
                                                          (( (init n f) )
                                                            |> to_list)


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
                              ("\\$" ^ ( (n ^ ( (c ^ ( (":" ^ s) )) )) ))


let is_antiquot =
 fun s ->
  let len = (String.length s) in
  (( (len > 2) ) && (
    (( (( (String.get s 0) ) = '\\') ) && ( (( (String.get s 1) ) = '$')
      )) ))

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
                                                                    (Char.code
                                                                    c) in
                                                                   ((
                                                                    (c >=
                                                                    (
                                                                    (Char.code
                                                                    '0')
                                                                    )) )
                                                                    && (
                                                                    (c <=
                                                                    (
                                                                    (Char.code
                                                                    '9')
                                                                    )) ))


let destruct_poly =
 fun s ->
  let n = (String.length s) in
  if (n = 0) then ( (invalid_arg "destruct_poly length=0") )
  else if (( (String.get s 0) ) = '`') then
        (
        (Some (String.sub s 1 ( (n - 1) )))
        )
  else (None)

let uncurry = fun f -> fun (x, y) -> (f x y)

let rec drop =
                                                              fun n ->
                                                               function
                                                               | (_ :: l)
                                                                  when
                                                                  (n > 0) ->
                                                                  (drop (
                                                                    (n -
                                                                    1) )
                                                                    l)
                                                               | l -> l


let lastbut1 =
 fun ls ->
  (match ls with
   | [] -> (failwith "lastbut1 empty")
   | _ -> let l = (List.rev ls) in (( (List.tl l) ), ( (List.hd l) )))
