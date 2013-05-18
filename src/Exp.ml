
  
#default_quotation     "exp";;


open Ast
open AstLib
open LibUtil
open Basic
open FanUtil





        
(* +-----------------------------------------------------------------+
   | Utiliies for macro expansion                                    |
   +-----------------------------------------------------------------+ *)
(* Environment is a [string*pat] pair,

   Try to convert the 
   [exp] node into [pat] node.
   when do the conversion, if the exp node has an identifier which
   has a special meaning, then that replacment will be used
 *)  
let substp loc env =
  let bad_pat _loc =
    FanLoc.errorf _loc "this macro cannot be used in a pattern (see its definition)" in
  let rec loop (x:exp)= with {pat:exp;exp:pat}
    match x with
    | {| $e1 $e2 |} -> {@loc| $(loop e1) $(loop e2) |} 
    | {| $lid:x |} ->
        begin try List.assoc x env with
           Not_found -> {@loc| $lid:x |}
        end
    | {| $uid:x |} ->
        (try List.assoc x env with Not_found -> {@loc| $uid:x |})

    | {| $int:x |} -> {@loc| $int:x |}
    | {| $str:s |} -> {@loc| $str:s |}
    | {| $par:x |} -> {@loc| $(par:loop x) |}
    | {| $x1, $x2 |} -> {@loc| $(loop x1), $(loop x2) |}
    | {| { $bi } |} ->
        let rec substbi = with {pat:rec_exp;exp:pat} function
          | {| $b1; $b2 |} ->
            `Sem(_loc,substbi b1, substbi b2)
          | {| $id:i = $e |} -> `RecBind (loc,i,loop e)(* {@loc| $i = $(loop e) |} *)
          | _ -> bad_pat _loc  in
        {@loc| { $(substbi bi) } |}
    | _ -> bad_pat loc  in loop

(*
  [env] is a list of [string*exp],

  traverse the [exp] node
  when the identifier in pos exp in the exp has a speical meaning, using that instead
  when the identifier in pos pat in the exp has a special meaning,
  try to convert the exp meaning into pat and use that instead
 *)  
class subst loc env =  object
  inherit Objs.reloc loc as super
  method! exp = with exp function
    | {| $lid:x |} | {| $uid:x |} as e ->
         (try List.assoc x env with Not_found -> super#exp e)
    | {| LOCATION_OF $lid:x |} | {| LOCATION_OF $uid:x |} as e ->
          (try
            let loc = loc_of (List.assoc x env) in
            let (a, b, c, d, e, f, g, h) = FanLoc.to_tuple loc in
            {| FanLoc.of_tuple
              ($`str:a, $`int:b, $`int:c, $`int:d,
               $`int:e, $`int:f, $`int:g,
               $(if h then {| true |} else {| false |} )) |}
          with  Not_found -> super#exp e)
    | e -> super#exp e
  method! pat =  function
    | {:pat| $lid:x |} | {:pat| $uid:x |} as p ->
      (* convert expession into pattern only *)
        (try substp loc [] (List.assoc x env) with 
          Not_found -> super#pat p)
    | p -> super#pat p 
end


class type antiquot_filter =object
  inherit Objs.map
  method get_captured_variables: (exp * exp) list
  method clear_captured_variables: unit
end
      
(* We don't do any parsing for antiquots here, so it's parser-independent *)  
let capture_antiquot : antiquot_filter = object
  inherit Objs.map as super
  val mutable constraints =[]
  method! pat = function
    | `Ant(_loc,s) -> 
        begin match s with {content=code;_} ->
          (* begin  *)
          (* eprintf "Warning: the antiquot modifier %s is ignored@." name; *)
          let cons = {| $lid:code |} in
          let code' = "__fan__"^code in  (* prefix "fan__" FIXME *)
          let cons' = {| $lid:code' |} in 
          let () = constraints <- (cons,cons')::constraints in 
          {:pat| $lid:code' |} (* only allows lidentifiers here *)
        end
    | p -> super#pat p 
  method get_captured_variables =
    constraints
  method clear_captured_variables =
    constraints <- []
end

let filter_pat_with_captured_variables pat= begin 
  capture_antiquot#clear_captured_variables;
  let pat=capture_antiquot#pat pat in
  let constraints = capture_antiquot#get_captured_variables in
  (pat,constraints)
end





let fun_args _loc args body = with exp
  if args = [] then {| fun () -> $body |}
  else
    List.fold_right
      (fun arg body ->
	{| fun $arg -> $body |}) args body
  


let _loc = FanLoc.ghost 

let mk_record label_exps : exp=
  let rec_exps =
    List.map
      (fun (label, exp) ->
        {:rec_exp| $lid:label = $exp |} ) label_exps in
  `Record (_loc, (sem_of_list rec_exps))

let mkfun names acc  = with exp
  List.fold_right
  (fun name acc ->  {| function | $lid:name -> $acc |}) names acc 

    
let (<+<) pats acc =
  List.fold_right (fun p acc -> {| function | $pat:p -> $acc |} ) pats acc



(* +-----------------------------------------------------------------+
   | Multiple staging code generation.                               |
   +-----------------------------------------------------------------+ *)
  
  
let mee_comma x y =
  with exp'
  {| {| $($x), $($y) |} |}(* BOOTSTRAPPING*)



let mee_app x y =
  with exp' (* BOOTSTRAPPING *)
  {| {| $($x) $($y) |}|}


let mee_of_str s = with exp' (*    BOOTSTRAPING *)  
  let len = String.length s in
  if s.[0]='`' then
    let s = String.sub s 1 (len - 1) in 
    {|{|$(vrn:($str:s))|}|}
  else
    let u = {| {:ident'| $(uid:$str:s) |} |} in
    {| {| $(id:$u) |} |}
    (* {| {| $(uid:$s)|}|} *)
      (* {| A |}
           `App
    (_loc,
      (`App
         (_loc, (`Vrn (_loc, "ExId")),
           (`ExId (_loc, (`Lid (_loc, "_loc")))))),
      (`App
         (_loc, (`Vrn (_loc, "Uid")),
           (`Par
              (_loc,
                (`Com (_loc, (`ExId (_loc, (`Lid (_loc, "_loc")))), s)))))))
         
         `ExId (_loc, (`Uid (_loc, "A")))
         {:exp| `Uid (_loc,"A") |}
         {:exp| `ExId (_loc, (`Uid (_loc, "A"))) |}
         {:exp| {:exp| A |}|}
         {| {| A |}|}
       *)



(*
   @raise Invalid_argument
   
   There are 2 stages here 
   We want to generate code  like this
   {[

    {|  {| ( $(meta_int _loc x0), $(meta_int _loc x1) ) |}  |}
   ]}

  Normal practice:
  First print the result, then find a mechanical way to   construct

  Here we should avoid singleton tuple error
  {| $par:a |} when a is  single, it will cause error FIXME

 *)      

  
(*
  Here we want to generate something like
  
  {[
  ({| {| { u = $meta } |} |} );
  ]}
  [meta] could be parameterized
  
  First we need to construct this part
  {[
  (App 
       (App  (ExId  (IdAcc  (Uid  "Ast") (Uid  "RbEq")))
         (ExId  (Lid  "_loc")))
       (App 
         (App  (ExId  (IdAcc  (Uid  "Ast") (Uid  "Lid")))
           (ExId  (Lid  "_loc")))
         (Str  "u")))
  ]}
  given string input u
  we finally want to make 
  {[
  {| << {u = $meta_u$ ; v = $meta_v$ } |} >> 
  ]}
  given string input "u" and [ {| meta_u |} ]
 *)

let mk_tuple_ee = function (* BOOTSTRAPPING *)
  | [] -> invalid_arg "mktupee arity is zero "
  | [x] -> x
  | xs  ->
      {| `Par (_loc, $(List.reduce_right mee_comma xs)) |}

  
(**
  Example:
  {[
  mee_record_col "a" {|3|} = {| {:rec_exp| a = $($({|3|})) |}|};
  ]}
 *)
let mee_record_col label exp =
  {:exp'| {:rec_exp| $(lid:($str:label)) = $($exp) |}|} 


let mee_record_semi a b =
  {:exp'| {:rec_exp| $($a);$($b) |} |}


(*
  Example:
  {[
  mk_record_ee [("a",{|3|})] = {| {| { a = $($({|3|})) }|}|};
  ]}
 *)  
let mk_record_ee label_exps =
  with exp'
  label_exps
  |> List.map (fun (label,exp) -> mee_record_col label exp)
  |> (fun es -> {| {| { $($(List.reduce_right mee_record_semi es)) } |}|} )

    

(* Mainly used to overcome the value restriction
   {[
    eta_expand {|f |} 3 |> FanBasic.p_exp f;
    fun a0  a1  a2  -> f a0 a1 a2
   ]}
 *)
let eta_expand (exp:exp) number : exp =
  let names = List.init number (fun i -> x ~off:0 i ) in
  mkfun names (exp +> names )


(*
  Example:
  {[
  gen_curry_n {|3|} ~arity:2 "`X" 2 ;
  fun [ `X (a0, a1) -> fun [ `X (b0, b1) -> 3 ] ]

  gen_curry_n {|3|} ~arity:2 "X" 2 ;
  fun (X (a0,a1))  (X (b0,b1))  -> 3
  ]}
  
 *)
let gen_curry_n (acc:exp) ~arity cons n : exp =
  let args = List.init arity
      (fun i -> List.init n (fun j -> {:pat| $(id:xid ~off:i j) |})) in
  let pat = (EP.of_str cons :> pat) in
  List.fold_right
    (fun p acc -> {| function | $pat:p -> $acc  |} )
    (List.map (fun lst -> appl_of_list (pat:: lst)) args) acc

(*
  Example:
  {[
   let u  =  list_of_or' {:case|
  (A0 (a0, a1),A0 (b0, b1)) -> 1
  |   (A1 (a0, a1), A1 (b0, b1)) -> 2
  |   (A2 (a0, a1), A2 (b0, b1)) -> 3 |} [] in currying ~arity:2 u ;

  fun a0  b0  ->
  match (a0, b0) with
  | (A0 (a0,a1),A0 (b0,b1)) -> 1
  | (A1 (a0,a1),A1 (b0,b1)) -> 2
  | (A2 (a0,a1),A2 (b0,b1)) -> 3
  ]}

  Make Sure the names generated are shadowed by
  gen_tuple_n
 *)
  
let currying cases ~arity =
  let cases = bar_of_list cases in (* FIXME when cases is []*)
  if  arity >= 2 then 
    let names = List.init arity (fun i -> x ~off:i 0) in
    let exps = List.map (fun s-> {| $lid:s |} ) names in
    let x = tuple_com exps in
    mkfun names  {| match $x with | $cases |} 
    (* names <+ {| match $(tuple_com exps) with [ $list:cases ] |} *)
  else {| function | $cases |}
      (* {| fun [ $list:cases ] |} *)


let unknown len =
  if len = 0 then
    {| self# $(`Lid (_loc, "unknown")) |} (* FIXME*)
  else {| failwith $(str:"not implemented!") |}

  
