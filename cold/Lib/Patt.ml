open LibUtil
open Basic
open FSig
let _loc = FanLoc.ghost
let app a b = `PaApp (_loc, a, b)
let comma a b = `Com (_loc, a, b)
let rec apply acc = function | [] -> acc | x::xs -> apply (app acc x) xs
let sem a b =
  let _loc = FanLoc.merge (FanAst.loc_of a) (FanAst.loc_of b) in
  `Sem (_loc, a, b)
let list_of_app ty =
  let rec loop t acc =
    match t with
    | `PaApp (_loc,t1,t2) -> loop t1 (t2 :: acc)
    | `Nil _loc -> acc
    | i -> i :: acc in
  loop ty []
let list_of_com ty =
  let rec loop t acc =
    match t with
    | `Com (_loc,t1,t2) -> t1 :: (loop t2 acc)
    | `Nil _loc -> acc
    | i -> i :: acc in
  loop ty []
let list_of_sem ty =
  let rec loop t acc =
    match t with
    | `Sem (_loc,t1,t2) -> t1 :: (loop t2 acc)
    | `Nil _loc -> acc
    | i -> i :: acc in
  loop ty []
let rec view_app acc =
  function | `PaApp (_loc,f,a) -> view_app (a :: acc) f | f -> (f, acc)
let app_of_list = function | [] -> `Nil _loc | l -> List.reduce_left app l
let com_of_list = function | [] -> `Nil _loc | l -> List.reduce_right comma l
let sem_of_list = function | [] -> `Nil _loc | l -> List.reduce_right sem l
let tuple_of_list =
  function
  | [] -> invalid_arg "tuple_of_list while list is empty"
  | x::[] -> x
  | xs -> `Tup (_loc, (com_of_list xs))
let mklist loc =
  let rec loop top =
    function
    | [] -> `Id (_loc, (`Uid (_loc, "[]")))
    | e1::el ->
        let _loc = if top then loc else FanLoc.merge (FanAst.loc_of e1) loc in
        `PaApp
          (_loc, (`PaApp (_loc, (`Id (_loc, (`Uid (_loc, "::")))), e1)),
            (loop false el)) in
  loop true
let rec apply accu =
  function
  | [] -> accu
  | x::xs -> let _loc = FanAst.loc_of x in apply (`PaApp (_loc, accu, x)) xs
let mkarray loc arr =
  let rec loop top =
    function
    | [] -> `Id (_loc, (`Uid (_loc, "[]")))
    | e1::el ->
        let _loc = if top then loc else FanLoc.merge (FanAst.loc_of e1) loc in
        `Array (_loc, (`Sem (_loc, e1, (loop false el)))) in
  let items = arr |> Array.to_list in loop true items
let of_str s =
  let len = String.length s in
  if len = 0
  then invalid_arg "[expr|patt]_of_str len=0"
  else
    (match s.[0] with
     | '`' -> `PaVrn (_loc, (String.sub s 1 (len - 1)))
     | x when Char.is_uppercase x -> `Id (_loc, (`Uid (_loc, s)))
     | _ -> `Id (_loc, (`Lid (_loc, s))))
let of_ident_number cons n =
  apply (`Id (_loc, cons)) (List.init n (fun i  -> `Id (_loc, (xid i))))
let (+>) f names =
  apply f (List.map (fun lid  -> `Id (_loc, (`Lid (_loc, lid)))) names)
let gen_tuple_first ~number  ~off  =
  match number with
  | 1 -> `Id (_loc, (xid ~off 0))
  | n when n > 1 ->
      let lst =
        zfold_left ~start:1 ~until:(number - 1)
          ~acc:(`Id (_loc, (xid ~off 0)))
          (fun acc  i  -> comma acc (`Id (_loc, (xid ~off i)))) in
      `Tup (_loc, lst)
  | _ -> invalid_arg "n < 1 in gen_tuple_first"
let gen_tuple_second ~number  ~off  =
  match number with
  | 1 -> `Id (_loc, (xid ~off:0 off))
  | n when n > 1 ->
      let lst =
        zfold_left ~start:1 ~until:(number - 1)
          ~acc:(`Id (_loc, (xid ~off:0 off)))
          (fun acc  i  -> comma acc (`Id (_loc, (xid ~off:i off)))) in
      `Tup (_loc, lst)
  | _ -> invalid_arg "n < 1 in gen_tuple_first "
let tuple_of_number ast n =
  let res =
    zfold_left ~start:1 ~until:(n - 1) ~acc:ast
      (fun acc  _  -> comma acc ast) in
  if n > 1 then `Tup (_loc, res) else res
let tuple_of_list lst =
  let len = List.length lst in
  match len with
  | 1 -> List.hd lst
  | n when n > 1 -> `Tup (_loc, (List.reduce_left comma lst))
  | _ -> invalid_arg "tuple_of_list n < 1"
let of_vstr_number name i =
  let items = List.init i (fun i  -> `Id (_loc, (xid i))) in
  if items = []
  then `PaVrn (_loc, name)
  else
    (let item = items |> tuple_of_list in
     `PaApp (_loc, (`PaVrn (_loc, name)), item))
let gen_tuple_n ?(cons_transform= fun x  -> x)  ~arity  cons n =
  let args =
    List.init arity
      (fun i  -> List.init n (fun j  -> `Id (_loc, (xid ~off:i j)))) in
  let pat = of_str (cons_transform cons) in
  (List.map (fun lst  -> apply pat lst) args) |> tuple_of_list
let tuple _loc =
  function
  | [] -> `Id (_loc, (`Uid (_loc, "()")))
  | p::[] -> p
  | e::es -> `Tup (_loc, (`Com (_loc, e, (FanAst.paCom_of_list es))))
let mk_record ?(arity= 1)  cols =
  let mk_list off =
    List.mapi
      (fun i  ({ label;_} : col)  ->
         `PaEq (_loc, (`Lid (_loc, label)), (`Id (_loc, (xid ~off i))))) cols in
  let res =
    zfold_left ~start:1 ~until:(arity - 1)
      ~acc:(`PaRec (_loc, (FanAst.paSem_of_list (mk_list 0))))
      (fun acc  i  ->
         comma acc (`PaRec (_loc, (FanAst.paSem_of_list (mk_list i))))) in
  if arity > 1 then `Tup (_loc, res) else res
let mk_tuple ~arity  ~number  =
  match arity with
  | 1 -> gen_tuple_first ~number ~off:0
  | n when n > 1 ->
      let e =
        zfold_left ~start:1 ~until:(n - 1)
          ~acc:(gen_tuple_first ~number ~off:0)
          (fun acc  i  -> comma acc (gen_tuple_first ~number ~off:i)) in
      `Tup (_loc, e)
  | _ -> invalid_arg "mk_tuple arity < 1 "