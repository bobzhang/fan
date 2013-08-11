open FAstN
open LibUtil
open AstLibN
open BasicN
let of_str (s : string) =
  (let len = String.length s in
   if len = 0
   then invalid_arg "[exp|pat]_of_str len=0"
   else
     (match s.[0] with
      | '`' -> `Vrn (String.sub s 1 (len - 1))
      | x when Char.is_uppercase x -> `Uid s
      | _ -> `Lid s) : ep )
let gen_tuple_first ~number  ~off  =
  match number with
  | 1 -> xid ~off 0
  | n when n > 1 ->
      let lst =
        zfold_left ~start:1 ~until:(number - 1) ~acc:(xid ~off 0)
          (fun acc  i  -> com acc (xid ~off i)) in
      `Par lst
  | _ -> invalid_arg "n < 1 in gen_tuple_first"
let gen_tuple_second ~number  ~off  =
  match number with
  | 1 -> xid ~off:0 off
  | n when n > 1 ->
      let lst =
        zfold_left ~start:1 ~until:(number - 1) ~acc:(xid ~off:0 off)
          (fun acc  i  -> com acc (xid ~off:i off)) in
      `Par lst
  | _ -> invalid_arg "n < 1 in gen_tuple_first "
let tuple_of_number ast n =
  (let res =
     zfold_left ~start:1 ~until:(n - 1) ~acc:ast (fun acc  _  -> com acc ast) in
   if n > 1 then `Par res else res : ep )
let of_vstr_number name i =
  (let items = List.init i xid in
   if items = []
   then `Vrn name
   else (let item = tuple_com items in `App ((`Vrn name), item)) : ep )
let gen_tuple_n ?(cons_transform= fun x  -> x)  ~arity  cons n =
  let args = List.init arity (fun i  -> List.init n (fun j  -> xid ~off:i j)) in
  let pat = of_str (cons_transform cons) in
  (List.map (fun lst  -> appl_of_list (pat :: lst)) args) |> tuple_com
let mk_record ?(arity= 1)  cols =
  (let mk_list off =
     List.mapi
       (fun i  { CtypN.col_label = col_label;_}  ->
          `RecBind ((`Lid col_label), (xid ~off i))) cols in
   let res =
     zfold_left ~start:1 ~until:(arity - 1)
       ~acc:(`Record (sem_of_list (mk_list 0)))
       (fun acc  i  -> com acc (`Record (sem_of_list (mk_list i)))) in
   if arity > 1 then `Par res else res : ep )
let mk_tuple ~arity  ~number  =
  match arity with
  | 1 -> gen_tuple_first ~number ~off:0
  | n when n > 1 ->
      let e =
        zfold_left ~start:1 ~until:(n - 1)
          ~acc:(gen_tuple_first ~number ~off:0)
          (fun acc  i  -> com acc (gen_tuple_first ~number ~off:i)) in
      (`Par e : FAstN.ep )
  | _ -> invalid_arg "mk_tuple arity < 1 "