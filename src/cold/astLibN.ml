open Util
open Astfn
let sem a b = `Sem (a, b)
let com a b = `Com (a, b)
let app a b = `App (a, b)
let apply a b = `Apply (a, b)
let sta a b = `Sta (a, b)
let bar a b = `Bar (a, b)
let anda a b = `And (a, b)
let dot a b = `Dot (a, b)
let par x = `Par x
let seq a = `Seq a
let arrow a b = `Arrow (a, b)
let typing a b = `Constraint (a, b)
let rec bar_of_list =
  function
  | [] -> failwithf "bar_of_list empty"
  | t::[] -> t
  | t::ts -> bar t (bar_of_list ts)
let rec and_of_list =
  function
  | [] -> failwithf "and_of_list empty"
  | t::[] -> t
  | t::ts -> anda t (and_of_list ts)
let rec sem_of_list =
  function
  | [] -> failwithf "sem_of_list empty"
  | t::[] -> t
  | t::ts -> sem t (sem_of_list ts)
let rec com_of_list =
  function
  | [] -> failwithf "com_of_list empty"
  | t::[] -> t
  | t::ts -> com t (com_of_list ts)
let rec sta_of_list =
  function
  | [] -> failwithf "sta_of_list empty"
  | t::[] -> t
  | t::ts -> sta t (sta_of_list ts)
let rec dot_of_list =
  function
  | [] -> failwithf "dot_of_list empty"
  | t::[] -> t
  | t::ts -> dot t (dot_of_list ts)
let rec appl_of_list x =
  match x with
  | [] -> failwithf "appl_of_list empty"
  | x::[] -> x
  | x::y::xs -> appl_of_list ((app x y) :: xs)
let rec list_of_and x acc =
  match x with
  | `And (x,y) -> list_of_and x (list_of_and y acc)
  | _ -> x :: acc
let rec list_of_com x acc =
  match x with
  | `Com (x,y) -> list_of_com x (list_of_com y acc)
  | _ -> x :: acc
let rec list_of_star x acc =
  match x with
  | `Sta (x,y) -> list_of_star x (list_of_star y acc)
  | _ -> x :: acc
let rec list_of_bar x acc =
  match x with
  | `Bar (x,y) -> list_of_bar x (list_of_bar y acc)
  | _ -> x :: acc
let rec list_of_or x acc =
  match x with
  | `Bar (x,y) -> list_of_or x (list_of_or y acc)
  | _ -> x :: acc
let rec list_of_sem x acc =
  match x with
  | `Sem (x,y) -> list_of_sem x (list_of_sem y acc)
  | _ -> x :: acc
let rec list_of_dot x acc =
  match x with
  | `Dot (x,y) -> list_of_dot x (list_of_dot y acc)
  | x -> x :: acc
let rec list_of_app x acc =
  match x with
  | `App (t1,t2) -> list_of_app t1 (list_of_app t2 acc)
  | x -> x :: acc
let rec list_of_arrow_r x acc =
  match x with
  | `Arrow (t1,t2) -> list_of_arrow_r t1 (list_of_arrow_r t2 acc)
  | x -> x :: acc
let rec view_app acc =
  function | `App (f,a) -> view_app (a :: acc) f | f -> (f, acc)
let seq_sem ls = seq (sem_of_list ls)
let binds bs (e : exp) =
  match bs with
  | [] -> e
  | _ ->
      let binds = and_of_list bs in
      (`LetIn (`Negative, binds, e) : Astfn.exp )
let lid n = `Lid n
let uid n = `Uid n
let unit: ep = `Uid "()"
let ep_of_cons n ps = appl_of_list ((uid n) :: ps)
let tuple_com_unit =
  function | [] -> unit | p::[] -> p | y -> `Par (com_of_list y)
let tuple_com y =
  match y with
  | [] -> failwith "tuple_com empty"
  | x::[] -> x
  | _ -> `Par (com_of_list y)
let tuple_sta y =
  match y with
  | [] -> failwith "tuple_sta empty"
  | x::[] -> x
  | _ -> `Par (sta_of_list y)
let (+>) f names = appl_of_list (f :: (List.map lid names))
let meta_here location =
  let (a,b,c,d,e,f,g,h) = Locf.to_tuple location in
  `App
    ((`Dot ((`Uid "Locf"), (`Lid "of_tuple"))),
      (`Par
         (`Com
            ((`Str (String.escaped a)),
              (`Com
                 ((`Com
                     ((`Com
                         ((`Com
                             ((`Com
                                 ((`Com
                                     ((`Int (string_of_int b)),
                                       (`Int (string_of_int c)))),
                                   (`Int (string_of_int d)))),
                               (`Int (string_of_int e)))),
                           (`Int (string_of_int f)))),
                       (`Int (string_of_int g)))),
                   (if h then `Lid "true" else `Lid "false")))))))
