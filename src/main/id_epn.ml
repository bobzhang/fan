


%import{
Id:
  xid
;
};;

open Astfn 
open Astn_util



let of_vstr_number name i : ep=
  let items = Listf.init i xid  in
  if items = [] then %exp-'{$vrn:name}
  else
    let item = tuple_com items  in
    %exp-'{ $vrn:name $item }



let gen_tuple_first ~number ~off =
  match number with
  | 1 -> xid ~off 0 
  | n when n > 1 -> 
    let lst =
      Int.fold_left ~start:1 ~until:(number-1)
        ~acc:(xid ~off 0 )
        (fun acc i -> com acc (xid ~off i) ) in
    %exp-'{$par:lst}
  | _ -> %invalid_arg{}

(*
   {[
   gen_tuple_second 3 2 |> eprint;
   (a2, b2, c2)
   ]}
 *)

let gen_tuple_second ~number ~off =
  match number with 
  | 1 -> %exp-'{ $id{xid ~off:0 off} }
      
  | n when n > 1 -> 
    let lst =
      Int.fold_left ~start:1 ~until:(number - 1)
        ~acc:(%exp-'{ $id{xid ~off:0 off} })
        (fun acc i -> com acc %exp-'{ $id{xid ~off:i off } } ) in
    %exp-'{ $par:lst }
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
  let res = Int.fold_left ~start:1 ~until:(n-1) ~acc:ast
   (fun acc _ -> com acc ast) in
  if n > 1 then %exp-'{ $par:res } (* FIXME why %exp-'{ $par:x } cause an ghost location error*)
  else res

      
    
(*
  {[
    gen_tuple_n "X" 4 ~arity:2 |> opr#pat std_formatter ;
    (X a0 a1 a2 a3, X b0 b1 b2 b3)

    gen_tuple_n "`X" 4 ~arity:2 |> opr#pat std_formatter ;
   (`X (a0, a1, a2, a3), `X (b0, b1, b2, b3))

    gen_tuplen "`X" 4 ~arity:1 |> opr#pat std_formatter ;
   `X a0 a1 a2 a3
  ]}
  
*)
let gen_tuple_n ?(cons_transform=fun x -> x) ~arity cons n =
  let args = Listf.init arity
      (fun i -> Listf.init n (fun j -> %exp-'{ $id{xid ~off:i j} } )) in
  let pat = of_str @@ cons_transform cons in 
  args |> List.map
    (function
      | [] -> pat
      | lst ->
          %pat-'{ $pat ${tuple_com lst}} ) |> tuple_com 
    

  




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
      let e =
        Int.fold_left ~start:1 ~until:(n-1) ~acc:(gen_tuple_first ~number ~off:0)
        (fun acc i -> com acc (gen_tuple_first ~number ~off:i)) in
      %ep-{ $par:e }
  | _ -> %invalid_arg{} 

  








(* local variables: *)
(* compile-command: "cd .. && pmake main_annot/id_epn.cmo" *)
(* end: *)
