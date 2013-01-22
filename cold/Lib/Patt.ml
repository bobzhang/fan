open LibUtil
open Basic
open FSig
open FanAst
let _loc = FanLoc.ghost
let of_str s =
  let len = String.length s in
  if len = 0
  then invalid_arg "[expr|patt]_of_str len=0"
  else
    (match s.[0] with
     | '`' -> `Vrn (_loc, (String.sub s 1 (len - 1)))
     | x when Char.is_uppercase x -> `Id (_loc, (`Uid (_loc, s)))
     | _ -> `Id (_loc, (`Lid (_loc, s))))
let of_ident_number cons n =
  appl_of_list ((`Id (_loc, cons)) ::
    (List.init n (fun i  -> `Id (_loc, (xid i)))))
let (+>) f names =
  appl_of_list (f ::
    (List.map (fun lid  -> `Id (_loc, (`Lid (_loc, lid)))) names))
let gen_tuple_first ~number  ~off  =
  match number with
  | 1 -> `Id (_loc, (xid ~off 0))
  | n when n > 1 ->
      let lst =
        zfold_left ~start:1 ~until:(number - 1)
          ~acc:(`Id (_loc, (xid ~off 0)))
          (fun acc  i  -> com acc (`Id (_loc, (xid ~off i)))) in
      `Tup (_loc, lst)
  | _ -> invalid_arg "n < 1 in gen_tuple_first"
let gen_tuple_second ~number  ~off  =
  match number with
  | 1 -> `Id (_loc, (xid ~off:0 off))
  | n when n > 1 ->
      let lst =
        zfold_left ~start:1 ~until:(number - 1)
          ~acc:(`Id (_loc, (xid ~off:0 off)))
          (fun acc  i  -> com acc (`Id (_loc, (xid ~off:i off)))) in
      `Tup (_loc, lst)
  | _ -> invalid_arg "n < 1 in gen_tuple_first "
let tuple_of_number ast n =
  let res =
    zfold_left ~start:1 ~until:(n - 1) ~acc:ast (fun acc  _  -> com acc ast) in
  if n > 1 then `Tup (_loc, res) else res
let of_vstr_number name i =
  let items = List.init i (fun i  -> `Id (_loc, (xid i))) in
  if items = []
  then `Vrn (_loc, name)
  else
    (let item = items |> tuple_com in `App (_loc, (`Vrn (_loc, name)), item))
let gen_tuple_n ?(cons_transform= fun x  -> x)  ~arity  cons n =
  let args =
    List.init arity
      (fun i  -> List.init n (fun j  -> `Id (_loc, (xid ~off:i j)))) in
  let pat = of_str (cons_transform cons) in
  (List.map (fun lst  -> appl_of_list (pat :: lst)) args) |> tuple_com
let mk_record ?(arity= 1)  cols =
  let mk_list off =
    List.mapi
      (fun i  ({ label;_} : col)  ->
         `PaEq (_loc, (`Lid (_loc, label)), (`Id (_loc, (xid ~off i))))) cols in
  let res =
    zfold_left ~start:1 ~until:(arity - 1)
      ~acc:(`PaRec (_loc, (FanAst.sem_of_list (mk_list 0))))
      (fun acc  i  ->
         com acc (`PaRec (_loc, (FanAst.sem_of_list (mk_list i))))) in
  if arity > 1 then `Tup (_loc, res) else res
let mk_tuple ~arity  ~number  =
  match arity with
  | 1 -> gen_tuple_first ~number ~off:0
  | n when n > 1 ->
      let e =
        zfold_left ~start:1 ~until:(n - 1)
          ~acc:(gen_tuple_first ~number ~off:0)
          (fun acc  i  -> com acc (gen_tuple_first ~number ~off:i)) in
      `Tup (_loc, e)
  | _ -> invalid_arg "mk_tuple arity < 1 "