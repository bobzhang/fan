let xid = Fid.xid
open Astfn
open Astn_util
let of_str =
  function
  | (s : string) ->
      (let len = String.length s in
       if len = 0
       then invalid_arg "[exp|pat]_of_str len=0"
       else
         (match s.[0] with
          | '`' -> (`Vrn (String.sub s 1 (len - 1)) :> Astfn.ep)
          | x when Charf.is_uppercase x -> (`Uid s :> Astfn.ep)
          | _ -> (`Lid s :> Astfn.ep)) : ep)
let of_vstr_number =
  function
  | name ->
      (function
       | i ->
           (let items = Listf.init i xid in
            if items = []
            then `Vrn name
            else (let item = tuple_com items in `App ((`Vrn name), item)) : 
           ep))
let gen_tuple_first ~number  ~off  =
  match number with
  | 1 -> xid ~off 0
  | n when n > 1 ->
      let lst =
        Int.fold_left ~start:1 ~until:(number - 1) ~acc:(xid ~off 0)
          (function | acc -> (function | i -> com acc (xid ~off i))) in
      `Par lst
  | _ -> invalid_arg "n < 1 in gen_tuple_first"
let gen_tuple_second ~number  ~off  =
  match number with
  | 1 -> xid ~off:0 off
  | n when n > 1 ->
      let lst =
        Int.fold_left ~start:1 ~until:(number - 1) ~acc:(xid ~off:0 off)
          (function | acc -> (function | i -> com acc (xid ~off:i off))) in
      `Par lst
  | _ -> invalid_arg "n < 1 in gen_tuple_first "
let tuple_of_number =
  function
  | ast ->
      (function
       | n ->
           (let res =
              Int.fold_left ~start:1 ~until:(n - 1) ~acc:ast
                (function | acc -> (function | _ -> com acc ast)) in
            if n > 1 then `Par res else res : ep))
let gen_tuple_n ?(cons_transform= function | x -> x)  ~arity  =
  function
  | cons ->
      (function
       | n ->
           let args =
             Listf.init arity
               (function | i -> Listf.init n (function | j -> xid ~off:i j)) in
           let pat = of_str @@ (cons_transform cons) in
           (args |>
              (List.map
                 (function | [] -> pat | lst -> `App (pat, (tuple_com lst)))))
             |> tuple_com)
let mk_record ?(arity= 1)  =
  function
  | cols ->
      (let mk_list =
         function
         | off ->
             Listf.mapi
               (function
                | i ->
                    (function
                     | (x : Ctyp.col) ->
                         `RecBind ((`Lid (x.label)), (xid ~off i)))) cols in
       let res =
         let ls = sem_of_list (mk_list 0) in
         (Int.fold_left ~start:1 ~until:(arity - 1) ~acc:(`Record ls)) @@
           (function
            | acc ->
                (function
                 | i ->
                     let v = sem_of_list @@ (mk_list i) in
                     com acc (`Record v))) in
       if arity > 1 then `Par res else res : ep)
let mk_tuple ~arity  ~number  =
  match arity with
  | 1 -> gen_tuple_first ~number ~off:0
  | n when n > 1 ->
      let e =
        Int.fold_left ~start:1 ~until:(n - 1)
          ~acc:(gen_tuple_first ~number ~off:0)
          (function
           | acc ->
               (function | i -> com acc (gen_tuple_first ~number ~off:i))) in
      (`Par e :> Astfn.ep)
  | _ -> invalid_arg "mk_tuple arity < 1 "
