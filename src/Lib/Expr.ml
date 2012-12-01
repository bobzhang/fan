(* open Format; *)
open lang "expr";
open FanUtil;
module Ast= Camlp4Ast; (* it contains a module named Meta *)
(*
  {[
  
  sep_expr [] {| A.B.g.h|}; ;
  [(, ["A"; "B"], ExId (, IdLid (, "g"))); (, [], ExId (, IdLid (, "h")))]

  The first two dots are IdAcc, the last dot is ExAcc

  sep_expr [] {| A.B.g.h + 3 |}
  [(, [],
  ExApp (,
   ExApp (, ExId (, IdLid (, "+")),
    ExAcc (,
     ExId (, IdAcc (, IdUid (, "A"), IdAcc (, IdUid (, "B"), IdLid (, "g")))),
     ExId (, IdLid (, "h")))),
   ExInt (, "3")))]


  sep_expr [] {| A.B.g.h.i|}; ;
  [(, ["A"; "B"], ExId (, IdLid (, "g"))); (, [], ExId (, IdLid (, "h")));
  (, [], ExId (, IdLid (, "i")))]

  sep_expr [] {| $(uid:"").t |} ; ;
  - : (Camlp4Ast.Ast.loc * string list * Camlp4Ast.Ast.expr) list =
  [(, [""], Camlp4Ast.Ast.ExId (, Camlp4Ast.Ast.IdLid (, "t")))]

  ]}
 *)

let rec sep_expr acc = fun
  [ {| $e1.$e2|} ->
    sep_expr (sep_expr acc e2) e1
  | {@loc| $uid:s |} as e ->
      match acc with
      [ [] -> [(loc, [], e)]
      | [(loc', sl, e) :: l] -> [(FanLoc.merge loc loc', [s :: sl], e) :: l] ]
  | {| $(id:({:ident| $_.$_ |} as i)) |} ->
      sep_expr acc (Ident.normalize_acc i)
  | e -> [(Ast.loc_of_expr e, [], e) :: acc] ];


let rec fa al = fun
  [ {| $f $a |} ->fa [a :: al] f
  | f -> (f, al) ];


let rec apply accu = fun
  [ [] -> accu
  | [x :: xs] ->
      let _loc = Ast.loc_of_expr x
      in apply {| $accu $x |} xs ];

(* Ast.loc -> Ast.expr list -> Ast.expr *)  
let mklist _loc =
  let rec loop top =  fun
    [ [] -> {| [] |}
    | [e1 :: el] ->
        let _loc =
          if top then _loc else FanLoc.merge (Ast.loc_of_expr e1) _loc in
        {| [$e1 :: $(loop false el)] |} ] in loop true ;
  
let mkumin _loc f arg = match arg with
  [ {| $int:n |} -> {| $(int:neg_string n) |}
  | {| $int32:n |} -> {| $(int32:neg_string n) |}
  | {| $(int64:n) |} -> {| $(int64:neg_string n) |}
  | {| $nativeint:n |} -> {| $(nativeint:neg_string n) |}
  | {| $flo:n |} -> {| $(flo:neg_string n) |}
  | _ -> {| $(lid:"~" ^ f) $arg |} ];

(* FIXME refer to mkuplus *)  
let mkassert _loc = fun
  [ {| false |} -> {| assert false |} 
  | e -> {| assert $e |} ] ;


let mklist_last ?last _loc  =
  let rec loop top = fun
    [ [] -> match last with
      [ Some e -> e
      | None -> {| [] |} ]
    | [e1 :: el] ->
        let _loc =
          if top then _loc else FanLoc.merge (Ast.loc_of_expr e1) _loc in
        {| [$e1 :: $(loop false el)] |} ] in
  loop true ;

let mksequence _loc = fun
  [ {| $_; $_ |} | {| $anti:_ |} as e -> {| begin  $e end |}
  | e -> e ];

let mksequence' _loc = fun
  [ {| $_; $_ |} as e -> {| begin  $e  end |}
  | e -> e ];



  
let bigarray_get _loc arr arg =
  let coords =  match arg with
  [ {| ($e1, $e2) |} | {| $e1, $e2 |} ->
      Ast.list_of_expr e1 (Ast.list_of_expr e2 [])
  | _ -> [arg] ] in
  match coords with
  [ [] -> failwith "bigarray_get null list"
  | [c1] -> {| $arr.{$c1} |}  
  | [c1; c2] -> {| $arr.{$c1,$c2} |}  
  | [c1; c2; c3] -> {| $arr.{$c1,$c2,$c3} |} 
  | [c1;c2;c3::coords] ->
      {| $arr.{$c1,$c2,$c3,$(Ast.exSem_of_list coords) } |} ];
(* FIXME 1.ExArr, 2. can we just write $list:coords? *)

let bigarray_set _loc var newval = match var with
    [ {|  $arr.{$c1} |} ->
        Some {| $arr.{$c1} := $newval |} 
    | {|  $arr.{$c1, $c2} |} ->
        Some {|  $arr.{$c1, $c2} :=  $newval |}
    | {|  $arr.{$c1, $c2, $c3} |} ->
        Some {| $arr.{$c1,$c2,$c3} := $newval |} 
    |  {| Bigarray.Genarray.get $arr [| $coords |] |} -> (* FIXME how to remove Bigarray here?*)
        Some {| Bigarray.Genarray.set $arr [| $coords |] $newval |}
    | _ -> None ];
  

(* FIXME later *)
let rec pattern_eq_expression p e =
  match (p, e) with
  [ ({:patt| $lid:a |}, {| $lid:b |}) -> a = b
  | ({:patt| $uid:a |}, {| $uid:b |}) -> a = b
  | ({:patt| $p1 $p2 |}, {| $e1 $e2 |}) ->
      pattern_eq_expression p1 e1 && pattern_eq_expression p2 e2
  | _ -> false ] ;

  
(*************************************************************************)
(* List comprehension *)  
let map _loc p e l =  match (p, e) with
  [ ({:patt| $lid:x |}, {| $lid:y |}) when x = y -> l
  | _ ->
      if Ast.is_irrefut_patt p then
        {| List.map (fun $p -> $e) $l |}
      else
        {| List.fold_right
          (fun
            [ $pat:p when true -> (fun x xs -> [ x :: xs ]) $e
            | _ -> (fun l -> l) ])
          $l [] |} ];


let filter _loc p b l =
    if Ast.is_irrefut_patt p then
      {| List.filter (fun $p -> $b) $l |}
    else
      {| List.filter (fun [ $pat:p when true -> $b | _ -> false ]) $l |};
let concat _loc l = {| List.concat $l |};
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
      [ {| $e1 $e2 |} -> {:patt| $(loop e1) $(loop e2) |} 
      | {| |} -> {:patt| |}
      | {| $lid:x |} ->
          try List.assoc x env with
          [ Not_found -> {:patt| $lid:x |} ]
      | {| $uid:x |} ->
          try List.assoc x env with
          [ Not_found -> {:patt| $uid:x |} ]
      | {| $int:x |} -> {:patt| $int:x |}
      | {| $str:s |} -> {:patt| $str:s |}
      | {| ($tup:x) |} -> {:patt| $(tup:loop x) |}
      | {| $x1, $x2 |} -> {:patt| $(loop x1), $(loop x2) |}
      | {| { $bi } |} ->
          let rec substbi = fun
            [ {:rec_binding| $b1; $b2 |} -> {:patt| $(substbi b1); $(substbi b2) |}
            | {:rec_binding| $id:i = $e |} -> {:patt| $i = $(loop e) |}
            | _ -> bad_patt _loc ]
          in {:patt| { $(substbi bi) } |}
      | _ -> bad_patt _loc ] in loop;
  
  class subst _loc env = object
    inherit Ast.reloc _loc as super;
    method! expr =
      fun
      [ {| $lid:x |} | {| $uid:x |} as e ->
          try List.assoc x env with
          [ Not_found -> super#expr e ]
      | {@_loc| LOCATION_OF $lid:x |} | {@_loc| LOCATION_OF $uid:x |} as e ->
          try
            let loc = Ast.loc_of_expr (List.assoc x env) in
            let (a, b, c, d, e, f, g, h) = FanLoc.to_tuple loc in
            {| FanLoc.of_tuple
              ($`str:a, $`int:b, $`int:c, $`int:d,
               $`int:e, $`int:f, $`int:g,
               $(if h then {| true |} else {| false |} )) |}
          with [ Not_found -> super#expr e ]
      | e -> super#expr e ];

    method! patt =  fun
      [ {:patt| $lid:x |} | {:patt| $uid:x |} as p ->
         try substp _loc [] (List.assoc x env) with
         [ Not_found -> super#patt p ]
      | p -> super#patt p ];
  end;

(*************************************************************************)
(* utilit for MakeNothing *)
 let map_expr = fun
   [ {| $e NOTHING |} | {| fun $({:patt| NOTHING |} ) -> $e |} -> e
   | {@_loc| $(lid:"__FILE__") |} -> {| $(`str:FanLoc.file_name _loc) |}
   | {@_loc| $(lid:"__LOCATION__") |} ->
     let (a, b, c, d, e, f, g, h) = FanLoc.to_tuple _loc in
     {| FanLoc.of_tuple
       ($`str:a, $`int:b, $`int:c, $`int:d,
        $`int:e, $`int:f, $`int:g,
        $(if h then {| true |} else {| false |} )) |}
   | e -> e];
    



let antiquot_expander ~parse_patt ~parse_expr = object
  inherit Ast.map as super;
  method! patt =
    with "patt"
    fun
    [ {@_loc| $anti:s |} | {@_loc| $str:s |} as p ->
      let mloc _loc = Meta.MetaLocQuotation.meta_loc_patt _loc _loc in
      handle_antiquot_in_string ~s ~default:p ~parse:parse_patt ~loc:_loc
        ~decorate:(fun n p ->
            match n with
            [ "antisig_item" -> {| Ast.SgAnt ($(mloc _loc), $p) |}
            | "antistr_item" -> {| Ast.StAnt ($(mloc _loc), $p) |}
            | "antictyp" -> {| Ast.TyAnt ($(mloc _loc), $p) |}
            | "antipatt" -> {| Ast.PaAnt ($(mloc _loc), $p) |}
            | "antiexpr" -> {| Ast.ExAnt ($(mloc _loc), $p) |}
            | "antimodule_type" -> {| Ast.MtAnt($(mloc _loc), $p) |}
            | "antimodule_expr" -> {| Ast.MeAnt ($(mloc _loc), $p) |}
            | "anticlass_type" -> {| Ast.CtAnt ($(mloc _loc), $p) |}
            | "anticlass_expr" -> {| Ast.CeAnt ($(mloc _loc), $p) |}
            | "anticlass_sig_item" -> {| Ast.CgAnt ($(mloc _loc), $p) |}
            | "anticlass_str_item" -> {| Ast.CrAnt ($(mloc _loc), $p) |}
            | "antiwith_constr" -> {| Ast.WcAnt ($(mloc _loc), $p) |}
            | "antibinding" -> {| Ast.BiAnt ($(mloc _loc), $p) |}
            | "antirec_binding" -> {| Ast.RbAnt ($(mloc _loc), $p) |}
            | "antimatch_case" -> {| Ast.McAnt ($(mloc _loc), $p) |}
            | "antimodule_binding" -> {| Ast.MbAnt ($(mloc _loc), $p) |}
            | "antiident" -> {| Ast.IdAnt ($(mloc _loc), $p) |}
            | "tupexpr" -> {|Ast.ExTup ($(mloc _loc), $p)|}
            | "tuppatt" -> {|Ast.PaTup ($(mloc _loc), $p)|}
            | "seqexpr" -> {|Ast.ExSeq ($(mloc _loc), $p) |}
            | _ -> p ])
      | p -> super#patt p ];
    method! expr = with "expr" fun
      [ {@_loc| $anti:s |} | {@_loc| $str:s |} as e ->
          let mloc _loc = Meta.MetaLocQuotation.meta_loc_expr _loc _loc in
          handle_antiquot_in_string ~s ~default:e ~parse:parse_expr ~loc:_loc
            ~decorate:(fun n e -> (* e is the parsed Ast node already *)
            match n with
            [ "`int" -> {| string_of_int $e |}
            | "`int32" -> {| Int32.to_string $e |}
            | "`int64" -> {| Int64.to_string $e |}
            | "`nativeint" -> {| Nativeint.to_string $e |}
            | "`flo" -> {| FanUtil.float_repres $e |}
            | "`str" -> {| Ast.safe_string_escaped $e |}
            | "`chr" -> {| Char.escaped $e |}
            | "`boolexpr" ->
                let x = {|Ast.IdLid $(mloc _loc) (if $e then "true" else "false" ) |} in
                {| {| $(id:$x)  |} |}
            | "tupexpr" ->   {| Ast.ExTup $(mloc _loc) $e |}
            | "tuppatt" ->  {|Ast.PaTup $(mloc _loc) $e |}
            | "seqexpr" -> {| Ast.ExSeq $(mloc _loc) $e |}
                  
            | "liststr_item" -> {| Ast.stSem_of_list $e |}
            | "listsig_item" -> {| Ast.sgSem_of_list $e |}
            | "listclass_sig_item" -> {| Ast.cgSem_of_list $e |}
            | "listclass_str_item" -> {| Ast.crSem_of_list $e |}
            | "listmodule_expr" -> {| Ast.meApp_of_list $e |}
            | "listmodule_type" -> {| Ast.mtApp_of_list $e |}
            | "listmodule_binding" -> {| Ast.mbAnd_of_list $e |}
            | "listbinding" -> {| Ast.biAnd_of_list $e |}
            | "listbinding;" -> {| Ast.biSem_of_list $e |}
            | "listrec_binding" -> {| Ast.rbSem_of_list $e |}
            | "listclass_type" -> {| Ast.ctAnd_of_list $e |}
            | "listclass_expr" -> {| Ast.ceAnd_of_list $e |}
            | "listident" -> {| Ast.idAcc_of_list $e |}
            | "listctypand" -> {| Ast.tyAnd_of_list $e |}
            | "listctyp;" -> {| Ast.tySem_of_list $e |}
            | "listctyp*" -> {| Ast.tySta_of_list $e |}
            | "listctyp|" -> {| Ast.tyOr_of_list $e |}
            | "listctyp," -> {| Ast.tyCom_of_list $e |}
            | "listctyp&" -> {| Ast.tyAmp_of_list $e |}
            | "listwith_constr" -> {| Ast.wcAnd_of_list $e |}
            | "listmatch_case" -> {| Ast.mcOr_of_list $e |}
            | "listpatt," -> {| Ast.paCom_of_list $e |}
            | "listpatt;" -> {| Ast.paSem_of_list $e |}
            | "listexpr," -> {| Ast.exCom_of_list $e |}
            | "listexpr;" -> {| Ast.exSem_of_list $e |}
            | "listforall" -> {| Ast.tyVarApp_of_list $e |}
            | "antisig_item" -> {| Ast.SgAnt $(mloc _loc) $e |}
            | "antistr_item" -> {| Ast.StAnt $(mloc _loc) $e |}
            | "antictyp" -> {| Ast.TyAnt $(mloc _loc) $e |}
            | "antipatt" -> {| Ast.PaAnt $(mloc _loc) $e |}
            | "antiexpr" -> {| Ast.ExAnt $(mloc _loc) $e |}
            | "antimodule_type" -> {| Ast.MtAnt $(mloc _loc) $e |}
            | "antimodule_expr" -> {| Ast.MeAnt $(mloc _loc) $e |}
            | "anticlass_type" -> {| Ast.CtAnt $(mloc _loc) $e |}
            | "anticlass_expr" -> {| Ast.CeAnt $(mloc _loc) $e |}
            | "anticlass_sig_item" -> {| Ast.CgAnt $(mloc _loc) $e |}
            | "anticlass_str_item" -> {| Ast.CrAnt $(mloc _loc) $e |}
            | "antiwith_constr" -> {| Ast.WcAnt $(mloc _loc) $e |}
            | "antibinding" -> {| Ast.BiAnt $(mloc _loc) $e |}
            | "antirec_binding" -> {| Ast.RbAnt $(mloc _loc) $e |}
            | "antimatch_case" -> {| Ast.McAnt $(mloc _loc) $e |}
            | "antimodule_binding" -> {| Ast.MbAnt $(mloc _loc) $e |}
            | "antiident" -> {| Ast.IdAnt $(mloc _loc) $e |}
            | "antidirection_flag" -> {| Ast.DiAnt  $e |}
            | "antioverride_flag" -> {| Ast.OvAnt $e |}
            | "antiprivate_flag" -> {|Ast.PrAnt $e |}
            | "antimutable_flag" -> {|Ast.MuAnt $e|}
            | "antivirtual_flag" -> {|Ast.ViAnt $e|}
            | "antirow_var_flag" -> {|Ast.RvAnt $e|}
            | "antirec_flag" -> {|Ast.ReAnt $e|}
            | _ -> e ])
      | e -> super#expr e ];
  end;

  
(* We don't do any parsing for antiquots here, so it's parser-independent *)  
let capture_antiquot = object
  inherit Camlp4Ast.map as super;
  val mutable constraints =[];
  method! patt = fun
  [ {:patt@_loc| $anti:s |} | {:patt@_loc| $str:s |} as p when is_antiquot s -> begin
    match view_antiquot s with
    [Some(_name,code) -> begin 
      (* eprintf "Warning: the antiquot modifier %s is ignored@." name; *)
      let cons = {| $lid:code |} in
      let code' = "__fan__"^code in  (* prefix "fan__" FIXME *)
      let cons' = {| $lid:code' |} in 
      let () = constraints <- [(cons,cons')::constraints]in 
      {:patt| $lid:code' |} (* only allows lidentifiers here *)
    end
  | None -> p ];   
  end
  | p -> super#patt p ];
 method get_captured_variables =
   constraints;
 method clear_captured_variables =
   constraints <- [];
end;

let filter_patt_with_captured_variables patt= begin 
  capture_antiquot#clear_captured_variables;
  let patt=capture_antiquot#patt patt in
  let constraints = capture_antiquot#get_captured_variables in
  (patt,constraints)
end;

(* let normalize = object *)
(*   val expr:Ast.expr; *)
(*   inherit Camlp4Ast.fold as super; *)
(*   method! patt = with "patt" fun *)
(*     [ {| $_ |} -> {| "_" |} *)
(*     | {| $lid:_ |} -> {| "_" |} *)
(*     | {| $p as $_ |} -> self#patt p  *)
(*     ] *)
(* end; *)


let rec string_of_ident = (* duplicated with Camlp4Filters remove soon*)
  fun
  [ {:ident| $lid:s |} -> s
  | {:ident| $uid:s |} -> s
  | {:ident| $i1.$i2 |} -> "acc_" ^ (string_of_ident i1) ^ "_" ^ (string_of_ident i2)
  | {:ident| $i1 $i2 |} -> "app_" ^ (string_of_ident i1) ^ "_" ^ (string_of_ident i2)
  | {:ident| $anti:_ |} -> assert false ];

    
(* let rec normalize = let _loc = FanLoc.ghost in with "patt" fun *)
(*   [ {| _ |} -> {|"_"|} *)
(*   | {| $id:_|} -> {:expr| "_"|} *)
(*   | {| ($p as $_) |} -> normalize p *)
(*   | {| $p1 $p2 |} -> {:expr| $(normalize p1) ^ $(normalize p2) |} *)
(*   | {| [| $p |]|} -> {:expr| "[|"^ $(normalize p) ^ "|]"|} (\* FIXME ^$ does not work *\) *)
(*   | {| $p1;$p2 |} -> {:expr| $(normalize p1) ^ ";" ^  $(normalize p2) |} *)
(*   | {| $p1,$p2|} ->  {:expr| $(normalize p1) ^ "," ^ $(normalize p2) |} *)
(*   | {| $chr:x |} -> {:expr| "'" ^ String.make 1 $chr:x ^ "'" |} *)
(*   | {| $int:x |} -> {:expr| $str:x |} *)
(*   | {| $int32:x |} -> {:expr| $str:x |} *)
(*   | {| $int64:x |} -> {:expr| $str:x |} *)
(*   | {| $nativeint:x |} -> {:expr| "\"" ^ $str:x ^ "\""|}  *)
(*   | {| $str:s |} -> {:expr| $str:s |} *)
(*   | {| lazy $p |} -> {:expr| "lazy" ^ $(normalize p)|} *)
(*   | {| (module $s) |}  -> {:expr| "(module" ^ $str:s ^")"|} *)
(*   | {| $flo:x |} -> {:expr| $str:x|} *)

(*   | {| $p1 | $p2 |} -> {:expr| $(normalize p1)  ^ "|" ^ $(normalize p2)  |} *)
        
(*   | {| $p1 .. $p2 |} -> {:expr| $(normalize p1) ^ ".." ^ $(normalize p2) |} *)
        
(*   | {| {$p} |} -> {:expr| "{" ^ $(normalize p)^ "}"|} *)
(*   | {| $i = $p |} -> *)
(*       {:expr| $(str:string_of_ident i) ^"=" ^ $(normalize p) |} *)

(*   | {| ($tup:pl) |} -> {:expr| "("^ $(normalize pl) ^")"|} *)
(*   | {| ($p:$_)|} -> normalize p (\* type was ignored *\) *)
(*   | {| `$s |} -> {:expr| "`" ^ $str:s |} *)
(*   (\* | {| $anti:x |} -> Syntax.parse_expr *\) *)
(*   | {|$anti:_|} | {||} *)
(*     | {| ? $_ |} | (\* FIXME ?$ not supported *\) *)
(*       {| ? $_ : ($_) |} | {| ? $_ : ($_ = $_ )|} | *)
(*       {| ~ $_ |} | {| ~ $_ : $_ |} | {| #$_ |}  *)
(*       -> assert false *)
(*   ]; *)


  
let tuple _loc  =   fun
  [[] -> {|()|}
  |[p] -> p
  | [e::es] -> {| ($e, $list:es) |} ];
  
let fun_args _loc args body =
  if args = [] then {| fun () -> $body |}
  else
    List.fold_right
      (fun arg body ->
	{| fun $arg -> $body |}) args body;
  
let fun_apply _loc e args =
  if args = [] then {| $e () |}
  else
    List.fold_left
      (fun e arg ->
        {| $e $arg |}) e args;
