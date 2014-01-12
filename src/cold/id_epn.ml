let xid = Id.xid
open Astfn
open Astn_util
let of_vstr_number name i =
  (let items = Listf.init i xid in
   if items = []
   then `Vrn name
   else (let item = tuple_com items in `App ((`Vrn name), item)) : ep )
let gen_tuple_first ~number  ~off  =
  match number with
  | 1 -> xid ~off 0
  | n when n > 1 ->
      let lst =
        Int.fold_left ~start:1 ~until:(number - 1) ~acc:(xid ~off 0)
          (fun acc  i  -> com acc (xid ~off i)) in
      `Par lst
  | _ -> invalid_arg ("Id_epn" ^ ("." ^ "Id_epn.gen_tuple_first"))
let gen_tuple_second ~number  ~off  =
  match number with
  | 1 -> xid ~off:0 off
  | n when n > 1 ->
      let lst =
        Int.fold_left ~start:1 ~until:(number - 1) ~acc:(xid ~off:0 off)
          (fun acc  i  -> com acc (xid ~off:i off)) in
      `Par lst
  | _ -> invalid_arg "n < 1 in gen_tuple_first "
let tuple_of_number ast n =
  (let res =
     Int.fold_left ~start:1 ~until:(n - 1) ~acc:ast
       (fun acc  _  -> com acc ast) in
   if n > 1 then `Par res else res : ep )
let gen_tuple_n ?(cons_transform= fun x  -> x)  ~arity  cons n =
  let args =
    Listf.init arity (fun i  -> Listf.init n (fun j  -> xid ~off:i j)) in
  let pat = of_str @@ (cons_transform cons) in
  (args |>
     (List.map (function | [] -> pat | lst -> `App (pat, (tuple_com lst)))))
    |> tuple_com
let mk_tuple ~arity  ~number  =
  match arity with
  | 1 -> gen_tuple_first ~number ~off:0
  | n when n > 1 ->
      let e =
        Int.fold_left ~start:1 ~until:(n - 1)
          ~acc:(gen_tuple_first ~number ~off:0)
          (fun acc  i  -> com acc (gen_tuple_first ~number ~off:i)) in
      (`Par e :>Astfn.ep)
  | _ -> invalid_arg ("Id_epn" ^ ("." ^ "Id_epn.mk_tuple"))
