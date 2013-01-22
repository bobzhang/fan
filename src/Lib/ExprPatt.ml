
(* +-----------------------------------------------------------------+
   | it is a code template, it needs macro =GETLOC= and              |
   | [default_quotation] to compile                                  |
   +-----------------------------------------------------------------+ *)

(*
  Given an location, and a list of expression node,
  return an expression node which represents the list
  of the expresson nodes

  Example:
  {[
  mklist _loc [{|b|}; {|c|}; {|d|}] |> FanBasic.p_expr f;
  [b; c; d]
  ]}
  (* {:expr| [1;2;3::[]]|} *)
  DoubleColon
 *)
let mklist loc =
  let rec loop top =  fun
    [ [] -> {| [] |}
    | [e1 :: el] ->
        let _loc =
          if top then loc else FanLoc.merge (loc_of e1) loc in
        {| [$e1 :: $(loop false el)] |} ] in loop true ;

(* It is the inverse operation by [view_app]
   Example:
   {[
   apply {|a|} [{|b|}; {|c|}; {|d|}] |> FanBasic.p_expr f;
   a b c d
   ]}
 *)
let rec apply accu = fun
  [ [] -> accu
  | [x :: xs] -> let _loc = loc_of x in apply {| $accu $x |} xs ];
  
(*
  mk_array [| {| 1 |} ; {| 2 |} ; {| 3 |} |] |> e2s = ({| [|1;2;3|] |} |> e2s);
  True
 *)
let mkarray loc arr =
  let rec loop top =  fun
    [ [] -> {| [] |}
    | [e1 :: el] ->
        let _loc =
          if top then loc else FanLoc.merge (loc_of e1) loc in
        {| [| $e1 ; $(loop false el) |] |} ] in
  let items = arr |> Array.to_list in 
  loop true items;
  
(* let mk_array arr = *)
(*   let items = arr |> Array.to_list |> sem_of_list in  *)
(*   {| [| $items |] |};   *)


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
    [ '`' ->   
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
  apply {| $id:cons |} (List.init n (fun  i -> {| $(id:xid i) |} ));



(*
   For all strings, we don't do parsing at all. So keep your strings
   input as simple as possible
   
   {[
   ( {|blabla|} +> ["x0";"x1";"x2"] ) |> eprint;
   blabla x0 x1 x2
   ]}
 *)
let (+>) f names  =
  apply f (List.map (fun lid -> {| $lid:lid |} ) names);


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
  List.map (fun lst -> apply pat lst) args |> tuple_com(* tuple_of_list *) ;
    

  
(*
  This is used for Prolog, deprecated soon
 *)  
let tuple _loc  =   fun
  [[] -> {|()|}
  |[p] -> p
  | [e::es] -> {| ($e, $list:es) |} ];
