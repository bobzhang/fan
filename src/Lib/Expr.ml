open Camlp4Ast;
open FanUtil;

(*
  {[
  sep_expr_acc <:expr< A.B.g.h >>

  [(, ["A"; "B"], Camlp4Ast.Ast.ExId (, Camlp4Ast.Ast.IdLid (, "g")));
  (, [], Camlp4Ast.Ast.ExId (, Camlp4Ast.Ast.IdLid (, "h")))]

  The first two dots are IdAcc, the last dot is ExAcc

  sep_expr_acc <:expr< A.B.g.h + 3 >>

  [(, [],
  Camlp4Ast.Ast.ExApp (,
   Camlp4Ast.Ast.ExApp (, Camlp4Ast.Ast.ExId (, Camlp4Ast.Ast.IdLid (, "+")),
    Camlp4Ast.Ast.ExAcc (,
     Camlp4Ast.Ast.ExId (,
      Camlp4Ast.Ast.IdAcc (, Camlp4Ast.Ast.IdUid (, "A"),
       Camlp4Ast.Ast.IdAcc (, Camlp4Ast.Ast.IdUid (, "B"),
        Camlp4Ast.Ast.IdLid (, "g")))),
     Camlp4Ast.Ast.ExId (, Camlp4Ast.Ast.IdLid (, "h")))),
   Camlp4Ast.Ast.ExInt (, "3")))]

  sep_expr_acc <:expr< A.B.g.h.i>>
  [(, ["A"; "B"], Camlp4Ast.Ast.ExId (, Camlp4Ast.Ast.IdLid (, "g")));
  (, [], Camlp4Ast.Ast.ExId (, Camlp4Ast.Ast.IdLid (, "h")));
  (, [], Camlp4Ast.Ast.ExId (, Camlp4Ast.Ast.IdLid (, "i")))]
  ]}
 *)
let rec sep_expr_acc l = fun
  [ <:expr< $e1.$e2>> ->
    sep_expr_acc (sep_expr_acc l e2) e1
  | <:expr@loc< $uid:s >> as e ->
      match l with
      [ [] -> [(loc, [], e)]
      | [(loc', sl, e) :: l] -> [(FanLoc.merge loc loc', [s :: sl], e) :: l] ]
  | <:expr< $(id:(<:ident< $_.$_ >> as i)) >> ->
      sep_expr_acc l (Ident.normalize_acc i)
  | e -> [(loc_of_expr e, [], e) :: l] ];


let rec fa al = fun
  [ <:expr< $f $a >> ->fa [a :: al] f
  | f -> (f, al) ];


let rec apply accu = fun
  [ [] -> accu
  | [x :: xs] ->
      let _loc = loc_of_expr x
      in apply <:expr< $accu $x >> xs ];

let mklist _loc =
  let rec loop top =  fun
    [ [] -> <:expr< [] >>
    | [e1 :: el] ->
        let _loc =
          if top then _loc else FanLoc.merge (loc_of_expr e1) _loc in
        <:expr< [$e1 :: $(loop False el)] >> ] in loop True ;
  
let mkumin _loc f arg = match arg with
  [ <:expr< $int:n >> -> <:expr< $(int:neg_string n) >>
  | <:expr< $int32:n >> -> <:expr< $(int32:neg_string n) >>
  | <:expr< $(int64:n) >> -> <:expr< $(int64:neg_string n) >>
  | <:expr< $nativeint:n >> -> <:expr< $(nativeint:neg_string n) >>
  | <:expr< $flo:n >> -> <:expr< $(flo:neg_string n) >>
  | _ -> <:expr< $(lid:"~" ^ f) $arg >> ];
  
let mkassert _loc = fun
  [ <:expr< False >> ->
    <:expr< assert False >>
   (* this case takes care about
      the special assert false node *)
  | e -> <:expr< assert $e >> ] ;


let mklist_last ?last _loc  =
  let rec loop top = fun
    [ [] -> match last with
      [ Some e -> e
      | None -> <:expr< [] >> ]
    | [e1 :: el] ->
        let _loc =
          if top then _loc else FanLoc.merge (loc_of_expr e1) _loc in
        <:expr< [$e1 :: $(loop False el)] >> ] in
  loop True ;

let mksequence _loc = fun
  [ <:expr< $_; $_ >> | <:expr< $anti:_ >> as e -> <:expr< do { $e } >>
  | e -> e ];

let mksequence' _loc = fun
  [ <:expr< $_; $_ >> as e -> <:expr< do { $e } >>
  | e -> e ];


let bigarray_get _loc arr arg =
  let coords =  match arg with
  [ <:expr< ($e1, $e2) >> | <:expr< $e1, $e2 >> ->
      list_of_expr e1 (list_of_expr e2 [])
  | _ -> [arg] ] in
  match coords with
  [ [] -> failwith "bigarray_get null list"
  | [c1] -> <:expr< $arr.{$c1} >>  
  | [c1; c2] -> <:expr< $arr.{$c1,$c2} >>  
  | [c1; c2; c3] -> <:expr< $arr.{$c1,$c2,$c3} >> 
  | [c1;c2;c3::coords] ->
      <:expr< $arr.{$c1,$c2,$c3,$(exSem_of_list coords) } >> ]; (* FIXME 1.ExArr, 2. can we just write $list:coords? *)

let bigarray_set _loc var newval = match var with
    [ <:expr<  $arr.{$c1} >> ->
        Some <:expr< $arr.{$c1} := $newval >> 
    | <:expr<  $arr.{$c1, $c2} >> ->
        Some <:expr<  $arr.{$c1, $c2} :=  $newval >>
    | <:expr<  $arr.{$c1, $c2, $c3} >> ->
        Some <:expr< $arr.{$c1,$c2,$c3} := $newval >> 
    |  <:expr< Bigarray.Genarray.get $arr [| $coords |] >> -> (* FIXME how to remove Bigarray here?*)
        Some <:expr< Bigarray.Genarray.set $arr [| $coords |] $newval >>
    | _ -> None ];
  

(*************************************************************************)
(* List comprehension *)  
let map _loc p e l =  match (p, e) with
  [ (<:patt< $lid:x >>, <:expr< $lid:y >>) when x = y -> l
  | _ ->
      if is_irrefut_patt p then
        <:expr< List.map (fun $p -> $e) $l >>
      else
        <:expr< List.fold_right
          (fun
            [ $pat:p when True -> (fun x xs -> [ x :: xs ]) $e
            | _ -> (fun l -> l) ])
          $l [] >> ];


let filter _loc p b l =
    if is_irrefut_patt p then
      <:expr< List.filter (fun $p -> $b) $l >>
    else
      <:expr< List.filter (fun [ $p when True -> $b | _ -> False ]) $l >>;
let concat _loc l = <:expr< List.concat $l >>;
(* only this function needs to be exposed *)
let rec compr _loc e =  fun
    [ [`gen (p, l)] -> map _loc p e l
    | [`gen (p, l); `cond b :: items] ->
        compr _loc e [`gen (p, filter _loc p b l) :: items]
    | [`gen (p, l) :: ([ `gen (_, _) :: _ ] as is )] ->
        concat _loc (map _loc p (compr _loc e is) l)
    | _ -> raise Stream.Failure ];

(*************************************************************************)    

(*************************************************************************)
(* Utility for macro *)
let bad_patt _loc =
  FanLoc.raise _loc
    (Failure
       "this macro cannot be used in a pattern (see its definition)");
let substp _loc env =
  let rec loop = fun
      [ <:expr< $e1 $e2 >> -> <:patt< $(loop e1) $(loop e2) >>
      | <:expr< >> -> <:patt< >>
      | <:expr< $lid:x >> ->
          try List.assoc x env with
          [ Not_found -> <:patt< $lid:x >> ]
      | <:expr< $uid:x >> ->
          try List.assoc x env with
          [ Not_found -> <:patt< $uid:x >> ]
      | <:expr< $int:x >> -> <:patt< $int:x >>
      | <:expr< $str:s >> -> <:patt< $str:s >>
      | <:expr< ($tup:x) >> -> <:patt< $(tup:loop x) >>
      | <:expr< $x1, $x2 >> -> <:patt< $(loop x1), $(loop x2) >>
      | <:expr< { $bi } >> ->
          let rec substbi = fun
            [ <:rec_binding< $b1; $b2 >> -> <:patt< $(substbi b1); $(substbi b2) >>
            | <:rec_binding< $i = $e >> -> <:patt< $i = $(loop e) >>
            | _ -> bad_patt _loc ]
          in <:patt< { $(substbi bi) } >>
      | _ -> bad_patt _loc ] in loop;
  
  class subst _loc env = object
    inherit reloc _loc as super;
    method! expr =
      fun
      [ <:expr< $lid:x >> | <:expr< $uid:x >> as e ->
          try List.assoc x env with
          [ Not_found -> super#expr e ]
      | <:expr@_loc< LOCATION_OF $lid:x >> | <:expr@_loc< LOCATION_OF $uid:x >> as e ->
          try
            let loc = loc_of_expr (List.assoc x env) in
            let (a, b, c, d, e, f, g, h) = FanLoc.to_tuple loc in
            <:expr< FanLoc.of_tuple
              ($`str:a, $`int:b, $`int:c, $`int:d,
               $`int:e, $`int:f, $`int:g,
               $(if h then <:expr< True >> else <:expr< False >> )) >>
          with [ Not_found -> super#expr e ]
      | e -> super#expr e ];

    method! patt =  fun
      [ <:patt< $lid:x >> | <:patt< $uid:x >> as p ->
         try substp _loc [] (List.assoc x env) with
         [ Not_found -> super#patt p ]
      | p -> super#patt p ];
  end;

(*************************************************************************)
(* utilit for MakeNothing *)
 let map_expr = fun
   [ <:expr< $e NOTHING >> | <:expr< fun $(<:patt< NOTHING >> ) -> $e >> -> e
   | <:expr@_loc< $(lid:"__FILE__") >> -> <:expr< $(`str:FanLoc.file_name _loc) >>
   | <:expr@_loc< $(lid:"__LOCATION__") >> ->
     let (a, b, c, d, e, f, g, h) = FanLoc.to_tuple _loc in
     <:expr< FanLoc.of_tuple
       ($`str:a, $`int:b, $`int:c, $`int:d,
        $`int:e, $`int:f, $`int:g,
        $(if h then <:expr< True >> else <:expr< False >> )) >>
   | e -> e];
    
