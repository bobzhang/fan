open Camlp4Ast
open LibUtil
open Basic
open FSig
module Ast = Camlp4Ast
let mklist _loc =
  let rec loop top =
    function
    | [] -> Ast.PaId (_loc, (Ast.IdUid (_loc, "[]")))
    | p1::pl ->
        let _loc = if top then _loc else FanLoc.merge (loc_of_patt p1) _loc in
        Ast.PaApp
          (_loc,
            (Ast.PaApp
               (_loc, (Ast.PaId (_loc, (Ast.IdUid (_loc, "::")))), p1)),
            (loop false pl)) in
  loop true
let tuple _loc =
  function
  | [] -> Ast.PaId (_loc, (Ast.IdUid (_loc, "()")))
  | p::[] -> p
  | e::es -> Ast.PaTup (_loc, (Ast.PaCom (_loc, e, (Ast.paCom_of_list es))))
let _loc = FanLoc.ghost
let app a b = Ast.PaApp (_loc, a, b)
let comma a b = Ast.PaCom (_loc, a, b)
let (<$) = app
let rec apply acc = function | [] -> acc | x::xs -> apply (app acc x) xs
let sem a b = Ast.PaSem (_loc, a, b)
let list_of_app ty =
  let rec loop t acc =
    match t with
    | Ast.PaApp (_loc,t1,t2) -> loop t1 (t2 :: acc)
    | Ast.PaNil _loc -> acc
    | i -> i :: acc in
  loop ty []
let list_of_com ty =
  let rec loop t acc =
    match t with
    | Ast.PaCom (_loc,t1,t2) -> t1 :: (loop t2 acc)
    | Ast.PaNil _loc -> acc
    | i -> i :: acc in
  loop ty []
let list_of_sem ty =
  let rec loop t acc =
    match t with
    | Ast.PaSem (_loc,t1,t2) -> t1 :: (loop t2 acc)
    | Ast.PaNil _loc -> acc
    | i -> i :: acc in
  loop ty []
let app_of_list =
  function | [] -> Ast.PaNil _loc | l -> List.reduce_left app l
let com_of_list =
  function | [] -> Ast.PaNil _loc | l -> List.reduce_right comma l
let sem_of_list =
  function | [] -> Ast.PaNil _loc | l -> List.reduce_right sem l
let tuple_of_list =
  function
  | [] -> invalid_arg "tuple_of_list while list is empty"
  | x::[] -> x
  | xs -> Ast.PaTup (_loc, (com_of_list xs))
let mk_list lst =
  let rec loop =
    function
    | [] -> Ast.PaId (_loc, (Ast.IdUid (_loc, "[]")))
    | x::xs ->
        Ast.PaApp
          (_loc,
            (Ast.PaApp (_loc, (Ast.PaId (_loc, (Ast.IdUid (_loc, "::")))), x)),
            (loop xs)) in
  loop lst
let mk_array arr =
  let items = (arr |> Array.to_list) |> sem_of_list in
  Ast.PaArr (_loc, items)
let of_str s =
  let len = String.length s in
  if len = 0
  then invalid_arg "[expr|patt]_of_str len=0"
  else
    (match s.[0] with
     | '`' -> Ast.PaVrn (_loc, (String.sub s 1 (len - 1)))
     | x when Char.is_uppercase x -> Ast.PaId (_loc, (Ast.IdUid (_loc, s)))
     | _ -> Ast.PaId (_loc, (Ast.IdLid (_loc, s))))
let of_ident_number cons n =
  apply (Ast.PaId (_loc, cons))
    (List.init n (fun i  -> Ast.PaId (_loc, (xid i))))
let (+>) f names =
  apply f
    (List.map (fun lid  -> Ast.PaId (_loc, (Ast.IdLid (_loc, lid)))) names)
let gen_tuple_first ~number  ~off  =
  match number with
  | 1 -> Ast.PaId (_loc, (xid ~off 0))
  | n when n > 1 ->
      let lst =
        zfold_left ~start:1 ~until:(number - 1)
          ~acc:(Ast.PaId (_loc, (xid ~off 0)))
          (fun acc  i  -> comma acc (Ast.PaId (_loc, (xid ~off i)))) in
      Ast.PaTup (_loc, lst)
  | _ -> invalid_arg "n < 1 in gen_tuple_first"
let gen_tuple_second ~number  ~off  =
  match number with
  | 1 -> Ast.PaId (_loc, (xid ~off:0 off))
  | n when n > 1 ->
      let lst =
        zfold_left ~start:1 ~until:(number - 1)
          ~acc:(Ast.PaId (_loc, (xid ~off:0 off)))
          (fun acc  i  -> comma acc (Ast.PaId (_loc, (xid ~off:i off)))) in
      Ast.PaTup (_loc, lst)
  | _ -> invalid_arg "n < 1 in gen_tuple_first "
let tuple_of_number ast n =
  let res =
    zfold_left ~start:1 ~until:(n - 1) ~acc:ast
      (fun acc  _  -> comma acc ast) in
  if n > 1 then Ast.PaTup (_loc, res) else res
let tuple_of_list lst =
  let len = List.length lst in
  match len with
  | 1 -> List.hd lst
  | n when n > 1 -> Ast.PaTup (_loc, (List.reduce_left comma lst))
  | _ -> invalid_arg "tuple_of_list n < 1"
let gen_tuple_n ~arity  cons n =
  let args =
    List.init arity
      (fun i  -> List.init n (fun j  -> Ast.PaId (_loc, (xid ~off:i j)))) in
  let pat = of_str cons in
  (List.map (fun lst  -> apply pat lst) args) |> tuple_of_list
let mk_record ?(arity= 1)  cols =
  let mk_list off =
    List.mapi
      (fun i  { col_label;_}  ->
         Ast.PaEq
           (_loc, (Ast.IdLid (_loc, col_label)),
             (Ast.PaId (_loc, (xid ~off i))))) cols in
  let res =
    zfold_left ~start:1 ~until:(arity - 1)
      ~acc:(Ast.PaRec (_loc, (Ast.paSem_of_list (mk_list 0))))
      (fun acc  i  ->
         comma acc (Ast.PaRec (_loc, (Ast.paSem_of_list (mk_list i))))) in
  if arity > 1 then Ast.PaTup (_loc, res) else res
let mk_tuple ~arity  ~number  =
  match arity with
  | 1 -> gen_tuple_first ~number ~off:0
  | n when n > 1 ->
      let e =
        zfold_left ~start:1 ~until:(n - 1)
          ~acc:(gen_tuple_first ~number ~off:0)
          (fun acc  i  -> comma acc (gen_tuple_first ~number ~off:i)) in
      Ast.PaTup (_loc, e)
  | _ -> invalid_arg "mk_tuple arity < 1 "