#default_quotation "expr";;
let _loc = FanLoc.ghost;
open LibUtil;
open AstLoc;
open Basic;

(*
   A very naive lifting. It does not do any parsing at all
   It is applied to both expr and patt
   {[
   of_str "`A";
   Vrn  "A" || Vrn "A"
   
   of_str "A";
   ExId  (Uid  "A")

   of_str "abs";
   ExId  (Lid  "abs")

   of_str "&&";
   ExId  (Lid  "&&")
   ]}
  *)
let of_str s =
  let len = String.length s in 
  if len = 0 then
    invalid_arg "[expr|patt]_of_str len=0"
  else
    match s.[0] with
    [ '`'->   
        (* {| ` $(String.sub s 1 (len - 1)) |} *)
      {|  $(vrn: String.sub s 1 (len - 1)) |}
    | x when Char.is_uppercase x -> {| $uid:s |}
    | _ -> {| $lid:s |} ];    


(*
   Applied to both expression and pattern
   {[
    of_ident_number <:ident< X >> 3 |> eprint;
    X a0 a1 a2
    ]}
*)
let  of_ident_number  cons n = 
  appl_of_list [{| $id:cons |}:: (List.init n (fun  i -> {| $(id:xid i) |} ))];



(*
   For all strings, we don't do parsing at all. So keep your strings
   input as simple as possible
   
   {[
   ( {|blabla|} +> ["x0";"x1";"x2"] ) |> eprint;
   blabla x0 x1 x2
   ]}
 *)
let (+>) f names  =
  appl_of_list [f:: (List.map (fun lid -> {| $lid:lid |} ) names)];


(*
   {[
   gen_tuple_first 3 2  |> eprint;
   (c0, c1, c2)

   ]}
 *)
let gen_tuple_first ~number ~off =
  match number with
  [ 1 -> {| $(id:xid ~off 0 ) |}  
  | n when n > 1 -> 
    let lst =
      zfold_left ~start:1 ~until:(number-1)
        ~acc:({| $(id:xid ~off 0 ) |})
        (fun acc i -> com acc {| $(id:xid ~off i) |} ) in
    {| $tup:lst |}
  | _ -> invalid_arg "n < 1 in gen_tuple_first" ];

(*
   {[
   gen_tuple_second 3 2 |> eprint;
   (a2, b2, c2)
   ]}
 *)
let gen_tuple_second ~number ~off =
  match number with 
  [ 1 -> {| $(id:xid ~off:0 off) |}
      
  | n when n > 1 -> 
    let lst =
      zfold_left ~start:1 ~until:(number - 1)
        ~acc:({| $(id:xid ~off:0 off) |})
        (fun acc i -> com acc {| $(id:xid ~off:i off ) |} ) in
    {| $tup:lst |}
  | _ -> 
        invalid_arg "n < 1 in gen_tuple_first "];    


(*
   For pattern it's not very useful since it's not allowed
   to have the same name in pattern language
   {[
   tuple_of_number <:patt< x >> 4 |> eprint;
   (x, x, x, x)

   tuple_of_number <:patt< x >> 1 |> eprint;
   x
   ]}
 *)    
let tuple_of_number ast n =
  let res = zfold_left ~start:1 ~until:(n-1) ~acc:ast
   (fun acc _ -> com acc ast) in
  if n > 1 then {| $tup:res |}
  else res;

(*
   @raise Invalid_argument
   when the length of lst is less than 1
  {[
  tuple_of_list [ {|a|} ; {| b|};  {|c|} ] |> eprint;
  (a, b, c)
  ]}
 *)
(* let tuple_of_list lst = *)
(*   let len = List.length lst in *)
(*   match len with *)
(*   [ 1  ->  List.hd lst *)
(*   | n when n > 1 ->  {| $(tup:List.reduce_left com lst) |}  *)
(*   | _ -> invalid_arg "tuple_of_list n < 1"] ; *)


let of_vstr_number name i =
  let items = List.init i (fun i -> {|$(id:xid i) |} ) in
  if items = [] then
    (* {|`$name|} *)
   {|$vrn:name|}
  else
    let item = items |> tuple_com(* tuple_of_list *) in
    (* {| `$name $item |} *)
    (* `App (_loc, (`Vrn (_loc, name)), item) (\* FIXME*\) *)
    {| $vrn:name $item |}
      
    ;
    
(*
  {[
    gen_tuple_n "X" 4 ~arity:2 |> opr#patt std_formatter ;
    (X a0 a1 a2 a3, X b0 b1 b2 b3)

    gen_tuple_n "`X" 4 ~arity:2 |> opr#patt std_formatter ;
   (`X a0 a1 a2 a3, `X b0 b1 b2 b3)

    gen_tuplen "`X" 4 ~arity:1 |> opr#patt std_formatter ;
   `X a0 a1 a2 a3
  ]}
  
*)
let gen_tuple_n ?(cons_transform=fun x -> x) ~arity cons n =
  let args = List.init arity
      (fun i -> List.init n (fun j -> {| $(id:xid ~off:i j) |} )) in
  let pat = of_str (cons_transform cons) in 
  List.map (fun lst -> appl_of_list [pat:: lst]) args |> tuple_com ;
    

  
(*
  This is used for Prolog, deprecated soon
 *)  
(* let tuple _loc  =   fun *)
(*   [[] -> {|()|} *)
(*   |[p] -> p *)
(*   | [e::es] -> {| ($e, $list:es) |} ]; *)


(*
  Example:
   {[
  mk_record ~arity:3 (Lib.Ctyp.list_of_record {:ctyp| u:int; v:mutable float |} )
  |> FanBasic.p_patt f;
  ({ u = a0; v = a1 },{ u = b0; v = b1 },{ u = c0; v = c1 })

   ]}
 *)
let mk_record ?(arity=1) cols  =
  let mk_list off = 
    List.mapi (fun i -> fun  [ ({FSig.col_label;_}:FSig.col) ->
      (* `RecBind (_loc, (`Lid (_loc, col_label)), (`Id (_loc, (xid ~off i)))) *)
      {:rec_expr| $lid:col_label = $(id:xid ~off i )  |} ]) cols in
  let res = zfold_left
      ~start:1 ~until:(arity-1) ~acc:(`Record(_loc,sem_of_list1 (mk_list  0))
        (* {| { $(list:mk_list 0) } |} *) )
      (fun acc i ->
        com acc (`Record (_loc, (sem_of_list1 (mk_list i))))
       (* {| { $(list:mk_list i) } |} *)  ) in
  if arity > 1 then
    {| $tup:res |}
  else res ;    


(*
   @raise Invalid_argument 
   {[
   
   mk_tuple ~arity:2 ~number:5 |> eprint ;
   ((a0, a1, a2, a3, a4), (b0, b1, b2, b3, b4))

   mk_tuple ~arity:1 ~number:5 |> eprint ;
   (a0, a1, a2, a3, a4)
   ]}
 *)      
let mk_tuple ~arity ~number =
  match arity with
  [ 1 -> gen_tuple_first ~number ~off:0
  | n when n > 1 -> 
      let e = zfold_left
        ~start:1 ~until:(n-1) ~acc:(gen_tuple_first ~number ~off:0)
        (fun acc i -> com acc (gen_tuple_first ~number ~off:i)) in
      {| $tup:e |}
  | _ -> invalid_arg "mk_tuple arity < 1 " ];        

  
