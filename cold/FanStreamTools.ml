open Ast

open AstLoc

type spat_comp =  
  | SpTrm of FanLoc.t* pat* exp option
  | SpNtr of FanLoc.t* pat* exp
  | SpStr of FanLoc.t* pat 

type sexp_comp =  
  | SeTrm of FanLoc.t* exp
  | SeNtr of FanLoc.t* exp 

let grammar_module_name = ref "XStream"

let gm () = grammar_module_name.contents

let strm_n = "__strm"

let peek_fun _loc =
  `Dot (_loc, (`Uid (_loc, (gm ()))), (`Lid (_loc, "peek")))

let junk_fun _loc =
  `Dot (_loc, (`Uid (_loc, (gm ()))), (`Lid (_loc, "junk")))

let empty _loc = `Dot (_loc, (`Uid (_loc, (gm ()))), (`Lid (_loc, "sempty")))

let is_raise = function | `App (_loc,`Lid (_,"raise"),_) -> true | _ -> false

let is_raise_failure =
  function
  | `App (_loc,`Lid (_,"raise"),`Dot (_,`Uid (_,m),`Uid (_,"Failure"))) when
      m = (gm ()) -> true
  | _ -> false

let rec handle_failure e =
  match e with
  | `Try (_loc,_,`Case (_,`Dot (_,`Uid (_,m),`Uid (_,"Failure")),e)) when
      m = (gm ()) -> handle_failure e
  | `Match (_loc,me,a) ->
      let rec case_handle_failure =
        function
        | `Bar (_loc,a1,a2) ->
            (case_handle_failure a1) && (case_handle_failure a2)
        | `Case (_loc,_,e) -> handle_failure e
        | _ -> false in
      (handle_failure me) && (case_handle_failure a)
  | `LetIn (_loc,`ReNil _,bi,e) ->
      let rec binding_handle_failure =
        function
        | `And (_loc,b1,b2) ->
            (binding_handle_failure b1) && (binding_handle_failure b2)
        | `Bind (_loc,_,e) -> handle_failure e
        | _ -> false in
      (binding_handle_failure bi) && (handle_failure e)
  | `Lid (_loc,_)|`Int (_loc,_)|`Str (_loc,_)|`Chr (_loc,_)|`Fun (_loc,_)
    |`Uid (_loc,_) -> true
  | `App (_loc,`Lid (_,"raise"),e) ->
      (match e with
       | `Dot (_loc,`Uid (_,m),`Uid (_,"Failure")) when m = (gm ()) -> false
       | _ -> true)
  | `App (_loc,f,x) ->
      (is_constr_apply f) && ((handle_failure f) && (handle_failure x))
  | _ -> false
and is_constr_apply =
  function
  | `Uid (_loc,_) -> true
  | `Lid (_loc,_) -> false
  | `App (_loc,x,_) -> is_constr_apply x
  | _ -> false

let rec subst v e =
  let _loc = loc_of e in
  match e with
  | `Lid (_loc,x) -> let x = if x = v then strm_n else x in `Lid (_loc, x)
  | `Uid (_loc,_)|`Int (_loc,_)|`Chr (_loc,_)|`Str (_loc,_)|`Field (_loc,_,_)
      -> e
  | `LetIn (_loc,rf,bi,e) ->
      `LetIn (_loc, rf, (subst_binding v bi), (subst v e))
  | `App (_loc,e1,e2) -> `App (_loc, (subst v e1), (subst v e2))
  | `Par (_loc,e) -> `Par (_loc, (subst v e))
  | `Com (_loc,e1,e2) -> `Com (_loc, (subst v e1), (subst v e2))
  | _ -> raise Not_found
and subst_binding v =
  function
  | (`And (_loc,b1,b2) : Ast.binding) ->
      `And (_loc, (subst_binding v b1), (subst_binding v b2))
  | (`Bind (_loc,`Lid (_,v'),e) : Ast.binding) ->
      `Bind (_loc, (`Lid (_loc, v')), (if v = v' then e else subst v e))
  | _ -> raise Not_found

let stream_pattern_component skont ckont =
  function
  | SpTrm (_loc,p,None ) ->
      `Match
        (_loc, (`App (_loc, (peek_fun _loc), (`Lid (_loc, strm_n)))),
          (`Bar
             (_loc,
               (`Case
                  (_loc, (`App (_loc, (`Uid (_loc, "Some")), p)),
                    (`Seq
                       (_loc,
                         (`Sem
                            (_loc,
                              (`App
                                 (_loc, (junk_fun _loc),
                                   (`Lid (_loc, strm_n)))), skont)))))),
               (`Case (_loc, (`Any _loc), ckont)))))
  | SpTrm (_loc,p,Some w) ->
      `Match
        (_loc, (`App (_loc, (peek_fun _loc), (`Lid (_loc, strm_n)))),
          (`Bar
             (_loc,
               (`CaseWhen
                  (_loc, (`App (_loc, (`Uid (_loc, "Some")), p)), w,
                    (`Seq
                       (_loc,
                         (`Sem
                            (_loc,
                              (`App
                                 (_loc, (junk_fun _loc),
                                   (`Lid (_loc, strm_n)))), skont)))))),
               (`Case (_loc, (`Any _loc), ckont)))))
  | SpNtr (_loc,p,e) ->
      let e =
        match e with
        | `Fun
            (_loc,`Case
                    (_,`Constraint
                         (_,`Lid (_,v),`App
                                         (_,`Dot (_,`Uid (_,m),`Lid (_,"t")),
                                          `Any _)),e))
            when (v = strm_n) && (m = (gm ())) -> e
        | _ -> `App (_loc, e, (`Lid (_loc, strm_n))) in
      if Exp.pattern_eq_expression p skont
      then
        (if is_raise_failure ckont
         then e
         else
           if handle_failure e
           then e
           else
             `Try
               (_loc, e,
                 (`Case
                    (_loc,
                      (`Dot
                         (_loc, (`Uid (_loc, (gm ()))),
                           (`Uid (_loc, "Failure")))), ckont))))
      else
        if is_raise_failure ckont
        then `LetIn (_loc, (`ReNil _loc), (`Bind (_loc, p, e)), skont)
        else
          if
            Exp.pattern_eq_expression (`App (_loc, (`Uid (_loc, "Some")), p))
              skont
          then
            `Try
              (_loc, (`App (_loc, (`Uid (_loc, "Some")), e)),
                (`Case
                   (_loc,
                     (`Dot
                        (_loc, (`Uid (_loc, (gm ()))),
                          (`Uid (_loc, "Failure")))), ckont)))
          else
            if is_raise ckont
            then
              (let tst =
                 if handle_failure e
                 then e
                 else
                   `Try
                     (_loc, e,
                       (`Case
                          (_loc,
                            (`Dot
                               (_loc, (`Uid (_loc, (gm ()))),
                                 (`Uid (_loc, "Failure")))), ckont))) in
               `LetIn (_loc, (`ReNil _loc), (`Bind (_loc, p, tst)), skont))
            else
              `Match
                (_loc,
                  (`Try
                     (_loc, (`App (_loc, (`Uid (_loc, "Some")), e)),
                       (`Case
                          (_loc,
                            (`Dot
                               (_loc, (`Uid (_loc, (gm ()))),
                                 (`Uid (_loc, "Failure")))),
                            (`Uid (_loc, "None")))))),
                  (`Bar
                     (_loc,
                       (`Case
                          (_loc, (`App (_loc, (`Uid (_loc, "Some")), p)),
                            skont)), (`Case (_loc, (`Any _loc), ckont)))))
  | SpStr (_loc,p) ->
      (try
         match p with | `Lid (_loc,v) -> subst v skont | _ -> raise Not_found
       with
       | Not_found  ->
           `LetIn
             (_loc, (`ReNil _loc), (`Bind (_loc, p, (`Lid (_loc, strm_n)))),
               skont))

let rec stream_pattern _loc epo e ekont =
  function
  | [] ->
      (match epo with
       | Some ep ->
           `LetIn
             (_loc, (`ReNil _loc),
               (`Bind
                  (_loc, ep,
                    (`App
                       (_loc,
                         (`Dot
                            (_loc, (`Uid (_loc, (gm ()))),
                              (`Lid (_loc, "count")))),
                         (`Lid (_loc, strm_n)))))), e)
       | _ -> e)
  | (spc,err)::spcl ->
      let skont =
        let ekont err =
          let str = match err with | Some estr -> estr | _ -> `Str (_loc, "") in
          `App
            (_loc, (`Lid (_loc, "raise")),
              (`App
                 (_loc,
                   (`Dot
                      (_loc, (`Uid (_loc, (gm ()))), (`Uid (_loc, "Error")))),
                   str))) in
        stream_pattern _loc epo e ekont spcl in
      let ckont = ekont err in stream_pattern_component skont ckont spc

let stream_patterns_term _loc ekont tspel =
  (let pel =
     List.fold_right
       (fun (p,w,_loc,spcl,epo,e)  acc  ->
          let p = `App (_loc, (`Uid (_loc, "Some")), p) in
          let e =
            let ekont err =
              let str =
                match err with | Some estr -> estr | _ -> `Str (_loc, "") in
              `App
                (_loc, (`Lid (_loc, "raise")),
                  (`App
                     (_loc,
                       (`Dot
                          (_loc, (`Uid (_loc, (gm ()))),
                            (`Uid (_loc, "Error")))), str))) in
            let skont = stream_pattern _loc epo e ekont spcl in
            `Seq
              (_loc,
                (`Sem
                   (_loc,
                     (`App (_loc, (junk_fun _loc), (`Lid (_loc, strm_n)))),
                     skont))) in
          match w with
          | Some w -> `Bar (_loc, (`CaseWhen (_loc, p, w, e)), acc)
          | None  -> `Bar (_loc, (`Case (_loc, p, e)), acc)) tspel
       (`Case (_loc, (`Any _loc), (ekont ()))) in
   `Match (_loc, (`App (_loc, (peek_fun _loc), (`Lid (_loc, strm_n)))), pel) : 
  exp )

let rec group_terms =
  function
  | ((SpTrm (_loc,p,w),None )::spcl,epo,e)::spel ->
      let (tspel,spel) = group_terms spel in
      (((p, w, _loc, spcl, epo, e) :: tspel), spel)
  | spel -> ([], spel)

let rec parser_cases _loc =
  function
  | [] ->
      `App
        (_loc, (`Lid (_loc, "raise")),
          (`Dot (_loc, (`Uid (_loc, (gm ()))), (`Uid (_loc, "Failure")))))
  | spel ->
      (match group_terms spel with
       | ([],(spcl,epo,e)::spel) ->
           stream_pattern _loc epo e (fun _  -> parser_cases _loc spel) spcl
       | (tspel,spel) ->
           stream_patterns_term _loc (fun _  -> parser_cases _loc spel) tspel)

let cparser _loc bpo pc =
  let e = parser_cases _loc pc in
  let e =
    match bpo with
    | Some bp ->
        `LetIn
          (_loc, (`ReNil _loc),
            (`Bind
               (_loc, bp,
                 (`App
                    (_loc,
                      (`Dot
                         (_loc, (`Uid (_loc, (gm ()))),
                           (`Lid (_loc, "count")))), (`Lid (_loc, strm_n)))))),
            e)
    | None  -> e in
  let p =
    `Constraint
      (_loc, (`Lid (_loc, strm_n)),
        (`App
           (_loc, (`Dot (_loc, (`Uid (_loc, (gm ()))), (`Lid (_loc, "t")))),
             (`Any _loc)))) in
  `Fun (_loc, (`Case (_loc, p, e)))

let cparser_match _loc me bpo pc =
  let pc = parser_cases _loc pc in
  let e =
    match bpo with
    | Some bp ->
        `LetIn
          (_loc, (`ReNil _loc),
            (`Bind
               (_loc, bp,
                 (`App
                    (_loc,
                      (`Dot
                         (_loc, (`Uid (_loc, (gm ()))),
                           (`Lid (_loc, "count")))), (`Lid (_loc, strm_n)))))),
            pc)
    | None  -> pc in
  match me with
  | `Lid (_loc,x) when x = strm_n -> e
  | _ ->
      `LetIn
        (_loc, (`ReNil _loc),
          (`Bind
             (_loc,
               (`Constraint
                  (_loc, (`Lid (_loc, strm_n)),
                    (`App
                       (_loc,
                         (`Dot
                            (_loc, (`Uid (_loc, (gm ()))),
                              (`Lid (_loc, "t")))), (`Any _loc))))), me)), e)

let rec not_computing =
  function
  | `Lid (_loc,_)|`Uid (_loc,_)|`Int (_loc,_)|`Flo (_loc,_)|`Chr (_loc,_)
    |`Str (_loc,_) -> true
  | `App (_loc,x,y) -> (is_cons_apply_not_computing x) && (not_computing y)
  | _ -> false
and is_cons_apply_not_computing =
  function
  | `Uid (_loc,_) -> true
  | `Lid (_loc,_) -> false
  | `App (_loc,x,y) -> (is_cons_apply_not_computing x) && (not_computing y)
  | _ -> false

let slazy _loc e =
  match e with
  | `App (_loc,f,`Uid (_,"()")) ->
      (match f with
       | `Lid (_loc,_) -> f
       | _ -> `Fun (_loc, (`Case (_loc, (`Any _loc), e))))
  | _ -> `Fun (_loc, (`Case (_loc, (`Any _loc), e)))

let rec cstream gloc =
  function
  | [] ->
      let _loc = gloc in
      `Dot (_loc, (`Uid (_loc, "XStream")), (`Lid (_loc, "sempty")))
  | (SeTrm (_loc,e))::[] ->
      if not_computing e
      then
        `App
          (_loc,
            (`Dot (_loc, (`Uid (_loc, (gm ()))), (`Lid (_loc, "ising")))), e)
      else
        `App
          (_loc,
            (`Dot (_loc, (`Uid (_loc, (gm ()))), (`Lid (_loc, "lsing")))),
            (slazy _loc e))
  | (SeTrm (_loc,e))::secl ->
      if not_computing e
      then
        `App
          (_loc,
            (`App
               (_loc,
                 (`Dot (_loc, (`Uid (_loc, (gm ()))), (`Lid (_loc, "icons")))),
                 e)), (cstream gloc secl))
      else
        `App
          (_loc,
            (`App
               (_loc,
                 (`Dot (_loc, (`Uid (_loc, (gm ()))), (`Lid (_loc, "lcons")))),
                 (slazy _loc e))), (cstream gloc secl))
  | (SeNtr (_loc,e))::[] ->
      if not_computing e
      then e
      else
        `App
          (_loc,
            (`Dot (_loc, (`Uid (_loc, (gm ()))), (`Lid (_loc, "slazy")))),
            (slazy _loc e))
  | (SeNtr (_loc,e))::secl ->
      if not_computing e
      then
        `App
          (_loc,
            (`App
               (_loc,
                 (`Dot (_loc, (`Uid (_loc, (gm ()))), (`Lid (_loc, "iapp")))),
                 e)), (cstream gloc secl))
      else
        `App
          (_loc,
            (`App
               (_loc,
                 (`Dot (_loc, (`Uid (_loc, (gm ()))), (`Lid (_loc, "lapp")))),
                 (slazy _loc e))), (cstream gloc secl))