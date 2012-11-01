module Ast = Camlp4Ast;
type spat_comp =
  [ SpTrm of FanLoc.t and Ast.patt and option Ast.expr
  | SpNtr of FanLoc.t and Ast.patt and Ast.expr
  | SpStr of FanLoc.t and Ast.patt ];

type sexp_comp =
  [ SeTrm of FanLoc.t and Ast.expr | SeNtr of FanLoc.t and Ast.expr ];


let grammar_module_name = let _loc = FanLoc.ghost in ref <:ident< Stream >> ;  
let gm () = !grammar_module_name;  
let strm_n = "__strm";
let peek_fun _loc = <:expr< $(id:gm()).peek >>;
let junk_fun _loc = <:expr< $(id:gm()).junk >>;

let rec pattern_eq_expression p e =  match (p, e) with
  [ (<:patt< $lid:a >>, <:expr< $lid:b >>) -> a = b
  | (<:patt< $uid:a >>, <:expr< $uid:b >>) -> a = b
  | (<:patt< $p1 $p2 >>, <:expr< $e1 $e2 >>) ->
      pattern_eq_expression p1 e1 && pattern_eq_expression p2 e2
  | _ -> False ] ;

let is_raise e =
  match e with
  [ <:expr< raise $_ >> -> True
  | _ -> False ] ;

let is_raise_failure e =
  match e with
  [ <:expr< raise Stream.Failure >> -> True
  | _ -> False ] ;
  
let rec handle_failure e =  match e with
  [ <:expr< try $_ with [ Stream.Failure -> $e] >> ->
      handle_failure e
  | <:expr< match $me with [ $a ] >> ->
      let rec match_case_handle_failure = fun
        [ <:match_case< $a1 | $a2 >> ->
            match_case_handle_failure a1 && match_case_handle_failure a2
        | <:match_case< $pat:_ -> $e >> -> handle_failure e
        | _ -> False ]
      in handle_failure me && match_case_handle_failure a
  | <:expr< let $bi in $e >> ->
      let rec binding_handle_failure =
        fun
        [ <:binding< $b1 and $b2 >> ->
            binding_handle_failure b1 && binding_handle_failure b2
        | <:binding< $_ = $e >> -> handle_failure e
        | _ -> False ]
      in binding_handle_failure bi && handle_failure e
  | <:expr< $lid:_ >> | <:expr< $int:_ >> | <:expr< $str:_ >> |
    <:expr< $chr:_ >> | <:expr< fun [ $_ ] >> | <:expr< $uid:_ >> ->
      True
  | <:expr< raise $e >> ->
      match e with
      [ <:expr< Stream.Failure >> -> False
      | _ -> True ]
  | <:expr< $f $x >> ->
      is_constr_apply f && handle_failure f && handle_failure x
  | _ -> False ]
and is_constr_apply =
  fun
  [ <:expr< $uid:_ >> -> True
  | <:expr< $lid:_ >> -> False
  | <:expr< $x $_ >> -> is_constr_apply x
  | _ -> False ];

let rec subst v e =
  let _loc = Ast.loc_of_expr e in
  match e with
  [ <:expr< $lid:x >> ->
      let x = if x = v then strm_n else x in
      <:expr< $lid:x >>
  | <:expr< $uid:_ >> -> e
  | <:expr< $int:_ >> -> e
  | <:expr< $chr:_ >> -> e
  | <:expr< $str:_ >> -> e
  | <:expr< $_ . $_ >> -> e
  | <:expr< let $rec:rf $bi in $e >> ->
      <:expr< let $rec:rf $(subst_binding v bi) in $(subst v e) >>
  | <:expr< $e1 $e2 >> -> <:expr< $(subst v e1) $(subst v e2) >>
  | <:expr< ( $tup:e ) >> -> <:expr< ( $(tup:subst v e) ) >>
  | <:expr< $e1, $e2 >> -> <:expr< $(subst v e1), $(subst v e2) >>
  | _ -> raise Not_found ]
and subst_binding v =
  fun
  [ <:binding@_loc< $b1 and $b2 >> ->
      <:binding< $(subst_binding v b1) and $(subst_binding v b2) >>
  | <:binding@_loc< $lid:v' = $e >> ->
      <:binding< $lid:v' = $(if v = v' then e else subst v e) >>
  | _ -> raise Not_found ];

let stream_pattern_component skont ckont =
  fun
  [ SpTrm _loc p None ->
      <:expr< match $(peek_fun _loc) $lid:strm_n with
              [ Some $p ->
                  do { $(junk_fun _loc) $lid:strm_n; $skont }
              | _ -> $ckont ] >>
  | SpTrm _loc p (Some w) ->
      <:expr< match $(peek_fun _loc) $lid:strm_n with
              [ Some $p when $w ->
                  do { $(junk_fun _loc) $lid:strm_n; $skont }
              | _ -> $ckont ] >>
  | SpNtr _loc p e ->
      let e =
        match e with
        [ <:expr< fun [ ($lid:v : Stream.t _) -> $e ] >> when v = strm_n -> e
        | _ -> <:expr< $e $lid:strm_n >> ]
      in
      if pattern_eq_expression p skont then
        if is_raise_failure ckont then e
        else if handle_failure e then e
        else <:expr< try $e with [ Stream.Failure -> $ckont ] >>
      else if is_raise_failure ckont then
        <:expr< let $p = $e in $skont >>
      else if pattern_eq_expression <:patt< Some $p >> skont then
        <:expr< try Some $e with [ Stream.Failure -> $ckont ] >>
      else if is_raise ckont then
        let tst =
          if handle_failure e then e
          else <:expr< try $e with [ Stream.Failure -> $ckont ] >>
        in
        <:expr< let $p = $tst in $skont >>
      else
        <:expr< match try Some $e with [ Stream.Failure -> None ] with
                [ Some $p -> $skont
                | _ -> $ckont ] >>
  | SpStr _loc p ->
      try
        match p with
        [ <:patt< $lid:v >> -> subst v skont
        | _ -> raise Not_found ]
      with
      [ Not_found -> <:expr< let $p = $lid:strm_n in $skont >> ] ];

let rec stream_pattern _loc epo e ekont = fun
  [ [] ->
      match epo with
      [ Some ep -> <:expr< let $ep = Stream.count $lid:strm_n in $e >>
      | _ -> e ]
  | [(spc, err) :: spcl] ->
      let skont =
        let ekont err =
          let str = match err with
            [ Some estr -> estr
            | _ -> <:expr< "" >> ] in
          <:expr< raise (Stream.Error $str) >>
        in
        stream_pattern _loc epo e ekont spcl
      in
      let ckont = ekont err in stream_pattern_component skont ckont spc ];

let stream_patterns_term _loc ekont tspel =
  let pel =
    List.fold_right
      (fun (p, w, _loc, spcl, epo, e) acc ->
        let p = <:patt< Some $p >> in
        let e =
          let ekont err =
            let str =
              match err with
              [ Some estr -> estr
              | _ -> <:expr< "" >> ]
            in
            <:expr< raise (Stream.Error $str) >>
          in
          let skont = stream_pattern _loc epo e ekont spcl in
          <:expr< do { $(junk_fun _loc) $lid:strm_n; $skont } >>
        in
        match w with
        [ Some w -> <:match_case< $pat:p when $w -> $e | $acc >>
        | None -> <:match_case< $pat:p -> $e | $acc >> ])
      tspel <:match_case<>>
  in
  <:expr< match $(peek_fun _loc) $lid:strm_n with [ $pel | _ -> $(ekont ()) ] >>
;

let rec group_terms =
  fun
  [ [([(SpTrm _loc p w, None) :: spcl], epo, e) :: spel] ->
      let (tspel, spel) = group_terms spel in
      ([(p, w, _loc, spcl, epo, e) :: tspel], spel)
  | spel -> ([], spel) ]
;

let rec parser_cases _loc = fun
  [ [] -> <:expr< raise Stream.Failure >>
  | spel ->
      match group_terms spel with
      [ ([], [(spcl, epo, e) :: spel]) ->
          stream_pattern _loc epo e (fun _ -> parser_cases _loc spel) spcl
      | (tspel, spel) ->
          stream_patterns_term _loc (fun _ -> parser_cases _loc spel) tspel ] ];

let cparser _loc bpo pc =
  let e = parser_cases _loc pc in
  let e =
    match bpo with
    [ Some bp -> <:expr< let $bp = Stream.count $lid:strm_n in $e >>
    | None -> e ]
  in
  let p = <:patt< ($lid:strm_n : Stream.t _) >> in
  <:expr< fun $p -> $e >> ;

let cparser_match _loc me bpo pc =
  let pc = parser_cases _loc pc in
  let e =
    match bpo with
    [ Some bp -> <:expr< let $bp = Stream.count $lid:strm_n in $pc >>
    | None -> pc ]  in
  let me =   match me with
    [ <:expr@_loc< $_; $_ >> as e -> <:expr< do { $e } >>
    | e -> e ] in
  match me with
  [ <:expr< $lid:x >> when x = strm_n -> e
  | _ -> <:expr< let ($lid:strm_n : Stream.t _) = $me in $e >> ] ;

(* streams *)

let rec not_computing =
  fun
  [ <:expr< $lid:_ >> | <:expr< $uid:_ >> | <:expr< $int:_ >> |
    <:expr< $flo:_ >> | <:expr< $chr:_ >> | <:expr< $str:_ >> -> True
  | <:expr< $x $y >> -> is_cons_apply_not_computing x && not_computing y
  | _ -> False ]
and is_cons_apply_not_computing =
  fun
  [ <:expr< $uid:_ >> -> True
  | <:expr< $lid:_ >> -> False
  | <:expr< $x $y >> -> is_cons_apply_not_computing x && not_computing y
  | _ -> False ];

let slazy _loc e =
  match e with
  [ <:expr< $f () >> ->
      match f with
      [ <:expr< $lid:_ >> -> f
      | _ -> <:expr< fun _ -> $e >> ]
  | _ -> <:expr< fun _ -> $e >> ] ;

let rec cstream gloc = 
  fun
  [ [] -> let _loc = gloc in <:expr< [< >] >>
  | [SeTrm _loc e] ->
      if not_computing e then <:expr< Stream.ising $e >>
      else <:expr< Stream.lsing $(slazy _loc e) >>
  | [SeTrm _loc e :: secl] ->
      if not_computing e then <:expr< Stream.icons $e $(cstream gloc secl) >>
      else <:expr< Stream.lcons $(slazy _loc e) $(cstream gloc secl) >>
  | [SeNtr _loc e] ->
      if not_computing e then e else <:expr< Stream.slazy $(slazy _loc e) >>
  | [SeNtr _loc e :: secl] ->
      if not_computing e then <:expr< Stream.iapp $e $(cstream gloc secl) >>
      else <:expr< Stream.lapp $(slazy _loc e) $(cstream gloc secl) >> ] ;
    
