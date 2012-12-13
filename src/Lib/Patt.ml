
(* open lang "patt"; *)
#default_quotation "patt"  ;;
open Camlp4Ast;
open LibUtil;
open Basic;
open FSig;
module Ast = Camlp4Ast; (* it has a nested  Ast module, FIXME *)


let mklist _loc =
  let rec loop top = fun
    [ [] -> {:patt| [] |}
    | [p1 :: pl] ->
        let _loc =
          if top then _loc else FanLoc.merge (loc_of_patt p1) _loc in
        {:patt| [$p1 :: $(loop false pl)] |} ] in loop true ;


let tuple _loc  =   fun
  [[] -> {|()|}
  |[p] -> p
  | [e::es] -> {| ($e, $list:es) |} ];


let _loc = FanLoc.ghost ;
INCLUDE "src/Lib/CommonStructure.ml"  ;
INCLUDE "src/Lib/ExprPatt.ml"  ;

(*
   {[

   (Fan_ctyp.list_of_record <:ctyp< u:int;v:mutable float >> )
   |> mk_record ~arity:3 |> Fan_patt.eprint;
   ({ u = a0; v = a1 }, { u = b0; v = b1 }, { u = c0; v = c1 })
   
   (Fan_ctyp.list_of_record <:ctyp< u:int;v:mutable float >> )
     |> mk_record ~arity:1 |> Fan_patt.eprint;
   { u = a0; v = a1 }

   ]}
 *)
let mk_record ?(arity=1) cols =
  let mk_list off = 
    List.mapi (fun i -> fun  [ {col_label;_} ->
      {| $lid:col_label = $(id:xid ~off i )  |} ]) cols in
  let res = zfold_left
      ~start:1 ~until:(arity-1) ~acc:({| { $(list:mk_list 0) } |} )
      (fun acc i -> comma acc {| { $(list:mk_list i) } |}  ) in
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
        (fun acc i ->
        comma acc (gen_tuple_first ~number ~off:i)) in
      {| $tup:e |}
  | _ -> invalid_arg "mk_tuple arity < 1 " ];        













  
