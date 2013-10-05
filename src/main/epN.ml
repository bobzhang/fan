
#{:control| default "exp-'"; |}



open FAstN 
(* open LibUtil *)
open AstLibN
open Fid

let of_str (s:string) : ep =
  let len = String.length s in 
  if len = 0 then
    invalid_arg "[exp|pat]_of_str len=0"
  else
    match s.[0] with
    | '`'-> {|  $(vrn: String.sub s 1 (len - 1)) |}
    | x when Fchar.is_uppercase x -> {| $uid:s |}
    | _ -> {| $lid:s |} 




let gen_tuple_first ~number ~off =
  match number with
  | 1 -> xid ~off 0 
  | n when n > 1 -> 
    let lst =
      LibUtil.zfold_left ~start:1 ~until:(number-1)
        ~acc:(xid ~off 0 )
        (fun acc i -> com acc (xid ~off i) ) in
    {| $par:lst |}
  | _ -> invalid_arg "n < 1 in gen_tuple_first" 

(*
   {[
   gen_tuple_second 3 2 |> eprint;
   (a2, b2, c2)
   ]}
 *)

let gen_tuple_second ~number ~off =
  match number with 
  | 1 -> {| $(id:xid ~off:0 off) |}
      
  | n when n > 1 -> 
    let lst =
      LibUtil.zfold_left ~start:1 ~until:(number - 1)
        ~acc:({| $(id:xid ~off:0 off) |})
        (fun acc i -> com acc {| $(id:xid ~off:i off ) |} ) in
    {| $par:lst |}
  | _ -> 
        invalid_arg "n < 1 in gen_tuple_first "


(*
   For pattern it's not very useful since it's not allowed
   to have the same name in pattern language
   {[
   tuple_of_number <:pat< x >> 4 |> eprint;
   (x, x, x, x)

   tuple_of_number <:pat< x >> 1 |> eprint;
   x
   ]}
 *)    
let tuple_of_number ast n : ep =
  let res = LibUtil.zfold_left ~start:1 ~until:(n-1) ~acc:ast
   (fun acc _ -> com acc ast) in
  if n > 1 then {| $par:res |} (* FIXME why {| $par:x |} cause an ghost location error*)
  else res

let of_vstr_number name i : ep=
  let items = Flist.init i xid  in
  if items = [] then {|$vrn:name|}
  else
    let item = tuple_com items  in
    {| $vrn:name $item |}
      
    
(*
  {[
    gen_tuple_n "X" 4 ~arity:2 |> opr#pat std_formatter ;
    (X a0 a1 a2 a3, X b0 b1 b2 b3)

    gen_tuple_n "`X" 4 ~arity:2 |> opr#pat std_formatter ;
   (`X a0 a1 a2 a3, `X b0 b1 b2 b3)

    gen_tuplen "`X" 4 ~arity:1 |> opr#pat std_formatter ;
   `X a0 a1 a2 a3
  ]}
  
*)
let gen_tuple_n ?(cons_transform=fun x -> x) ~arity cons n =
  let args = Flist.init arity
      (fun i -> Flist.init n (fun j -> {| $(id:xid ~off:i j) |} )) in
  let pat = of_str (cons_transform cons) in 
  Flist.map (fun lst -> appl_of_list (pat:: lst)) args |> tuple_com 
    

  


(*
  Example:
   {[
  mk_record ~arity:3 (CtypN.list_of_record {:ctyp| u:int; v:mutable float |} )
  |> Ast2pt.print_pat f;
  ({ u = a0; v = a1 },{ u = b0; v = b1 },{ u = c0; v = c1 })

   ]}
 *)
let mk_record ?(arity=1) cols : ep  =
  let mk_list off = 
    Flist.mapi
      (fun i {CtypN.col_label;_} ->
        {:rec_exp-'| $lid:col_label = $(xid ~off i) |} ) cols in
  let res = LibUtil.zfold_left
      ~start:1 ~until:(arity-1) ~acc:(`Record(sem_of_list (mk_list  0))
        (* {| { $(list:mk_list 0) } |} *) )
      (fun acc i -> com acc (`Record (sem_of_list (mk_list i))) ) in
  if arity > 1 then {| $par:res |}
  else res     


(*
   @raise Invalid_argument 
   {[
   
   mk_tuple ~arity:2 ~number:5 |> eprint ;
   ((a0, a1, a2, a3, a4), (b0, b1, b2, b3, b4))

   mk_tuple ~arity:1 ~number:5 |> eprint ;
   (a0, a1, a2, a3, a4)
   ]}
 *)      
let mk_tuple ~arity ~number  = 
  match arity with
  | 1 -> gen_tuple_first ~number ~off:0
  | n when n > 1 -> 
      let e = LibUtil.zfold_left
        ~start:1 ~until:(n-1) ~acc:(gen_tuple_first ~number ~off:0)
        (fun acc i -> com acc (gen_tuple_first ~number ~off:i)) in
      {:ep-| $par:e |}
  | _ -> invalid_arg "mk_tuple arity < 1 " 

  

(*
   @raise Invalid_argument
   when the length of lst is less than 1
  {[
  tuple_of_list [ {|a|} ; {| b|};  {|c|} ] |> eprint;
  (a, b, c)
  ]}
 *)
(* let tuple_of_list lst = *)
(*   let len = Flist.length lst in *)
(*   match len with *)
(*   [ 1  ->  List.hd lst *)
(*   | n when n > 1 ->  {| $(tup:List.reduce_left com lst) |}  *)
(*   | _ -> invalid_arg "tuple_of_list n < 1"] ; *)






