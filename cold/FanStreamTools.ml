open AstLoc
open Lib
type spat_comp =  
  | SpTrm of FanLoc.t* patt* expr option
  | SpNtr of FanLoc.t* patt* expr
  | SpStr of FanLoc.t* patt 
type sexp_comp =  
  | SeTrm of FanLoc.t* expr
  | SeNtr of FanLoc.t* expr 
let grammar_module_name = ref "XStream"
let gm () = grammar_module_name.contents
let strm_n = "__strm"
let peek_fun _loc =
  `Id (_loc, (`Dot (_loc, (`Uid (_loc, (gm ()))), (`Lid (_loc, "peek")))))
let junk_fun _loc =
  `Id (_loc, (`Dot (_loc, (`Uid (_loc, (gm ()))), (`Lid (_loc, "junk")))))
let empty _loc =
  `Id (_loc, (`Dot (_loc, (`Uid (_loc, (gm ()))), (`Lid (_loc, "sempty")))))
let is_raise =
  function | `App (_loc,`Id (_,`Lid (_,"raise")),_) -> true | _ -> false
let is_raise_failure =
  function
  | `App
      (_loc,`Id (_,`Lid (_,"raise")),`Id
                                       (_,`Dot
                                            (_,`Uid (_,m),`Uid (_,"Failure"))))
      when m = (gm ()) -> true
  | _ -> false
let rec handle_failure e =
  match e with
  | `Try
      (_loc,_,`Case
                (_,`Id (_,`Dot (_,`Uid (_,m),`Uid (_,"Failure"))),e))
      when m = (gm ()) -> handle_failure e
  | `Match (_loc,me,a) ->
      let rec match_case_handle_failure =
        function
        | `Or (_loc,a1,a2) ->
            (match_case_handle_failure a1) && (match_case_handle_failure a2)
        | `Case (_loc,_,e) -> handle_failure e
        | _ -> false in
      (handle_failure me) && (match_case_handle_failure a)
  | `LetIn (_loc,`ReNil _,bi,e) ->
      let rec binding_handle_failure =
        function
        | `And (_loc,b1,b2) ->
            (binding_handle_failure b1) && (binding_handle_failure b2)
        | `Bind (_loc,_,e) -> handle_failure e
        | _ -> false in
      (binding_handle_failure bi) && (handle_failure e)
  | `Id (_loc,`Lid (_,_))|`Int (_loc,_)|`Str (_loc,_)|`Chr (_loc,_)
    |`Fun (_loc,_)|`Id (_loc,`Uid (_,_)) -> true
  | `App (_loc,`Id (_,`Lid (_,"raise")),e) ->
      (match e with
       | `Id (_loc,`Dot (_,`Uid (_,m),`Uid (_,"Failure"))) when m = (gm ())
           -> false
       | _ -> true)
  | `App (_loc,f,x) ->
      (is_constr_apply f) && ((handle_failure f) && (handle_failure x))
  | _ -> false
and is_constr_apply =
  function
  | `Id (_loc,`Uid (_,_)) -> true
  | `Id (_loc,`Lid (_,_)) -> false
  | `App (_loc,x,_) -> is_constr_apply x
  | _ -> false
let rec subst v e =
  let _loc = loc_of e in
  match e with
  | `Id (_loc,`Lid (_,x)) ->
      let x = if x = v then strm_n else x in `Id (_loc, (`Lid (_loc, x)))
  | `Id (_loc,`Uid (_,_))|`Int (_loc,_)|`Chr (_loc,_)|`Str (_loc,_)
    |`Dot (_loc,_,_) -> e
  | `LetIn (_loc,rf,bi,e) ->
      `LetIn (_loc, rf, (subst_binding v bi), (subst v e))
  | `App (_loc,e1,e2) -> `App (_loc, (subst v e1), (subst v e2))
  | `Tup (_loc,e) -> `Tup (_loc, (subst v e))
  | `Com (_loc,e1,e2) -> `Com (_loc, (subst v e1), (subst v e2))
  | _ -> raise Not_found
and subst_binding v =
  function
  | `And (_loc,b1,b2) ->
      `And (_loc, (subst_binding v b1), (subst_binding v b2))
  | `Bind (_loc,`Id (_,`Lid (_,v')),e) ->
      `Bind
        (_loc, (`Id (_loc, (`Lid (_loc, v')))),
          (if v = v' then e else subst v e))
  | _ -> raise Not_found
let stream_pattern_component skont ckont =
  function
  | SpTrm (_loc,p,None ) ->
      `Match
        (_loc,
          (`App (_loc, (peek_fun _loc), (`Id (_loc, (`Lid (_loc, strm_n)))))),
          (`Or
             (_loc,
               (`Case
                  (_loc,
                    (`App (_loc, (`Id (_loc, (`Uid (_loc, "Some")))), p)),
                   (`Seq
                       (_loc,
                         (`Sem
                            (_loc,
                              (`App
                                 (_loc, (junk_fun _loc),
                                   (`Id (_loc, (`Lid (_loc, strm_n)))))),
                              skont)))))),
               (`Case (_loc, (`Any _loc),  ckont)))))
  | SpTrm (_loc,p,Some w) ->
      `Match
        (_loc,
          (`App (_loc, (peek_fun _loc), (`Id (_loc, (`Lid (_loc, strm_n)))))),
          (`Or
             (_loc,
               (`CaseWhen
                  (_loc,
                    (`App (_loc, (`Id (_loc, (`Uid (_loc, "Some")))), p)), w,
                    (`Seq
                       (_loc,
                         (`Sem
                            (_loc,
                              (`App
                                 (_loc, (junk_fun _loc),
                                   (`Id (_loc, (`Lid (_loc, strm_n)))))),
                              skont)))))),
               (`Case (_loc, (`Any _loc),  ckont)))))
  | SpNtr (_loc,p,e) ->
      let e =
        match e with
        | `Fun
            (_loc,`Case
                    (_,`Constraint
                         (_,`Id (_,`Lid (_,v)),`App
                                                 (_,`Id
                                                      (_,`Dot
                                                           (_,`Uid (_,m),
                                                            `Lid (_,"t"))),
                                                  `Any _)),e))
            when (v = strm_n) && (m = (gm ())) -> e
        | _ -> `App (_loc, e, (`Id (_loc, (`Lid (_loc, strm_n))))) in
      if Expr.pattern_eq_expression p skont
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
                      (`Id
                         (_loc,
                           (`Dot
                              (_loc, (`Uid (_loc, (gm ()))),
                                (`Uid (_loc, "Failure")))))),
                      ckont))))
      else
        if is_raise_failure ckont
        then `LetIn (_loc, (`ReNil _loc), (`Bind (_loc, p, e)), skont)
        else
          if
            Expr.pattern_eq_expression
              (`App (_loc, (`Id (_loc, (`Uid (_loc, "Some")))), p)) skont
          then
            `Try
              (_loc, (`App (_loc, (`Id (_loc, (`Uid (_loc, "Some")))), e)),
                (`Case
                   (_loc,
                     (`Id
                        (_loc,
                          (`Dot
                             (_loc, (`Uid (_loc, (gm ()))),
                               (`Uid (_loc, "Failure")))))), 
                     ckont)))
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
                            (`Id
                               (_loc,
                                 (`Dot
                                    (_loc, (`Uid (_loc, (gm ()))),
                                      (`Uid (_loc, "Failure")))))),
                             ckont))) in
               `LetIn (_loc, (`ReNil _loc), (`Bind (_loc, p, tst)), skont))
            else
              `Match
                (_loc,
                  (`Try
                     (_loc,
                       (`App (_loc, (`Id (_loc, (`Uid (_loc, "Some")))), e)),
                       (`Case
                          (_loc,
                            (`Id
                               (_loc,
                                 (`Dot
                                    (_loc, (`Uid (_loc, (gm ()))),
                                      (`Uid (_loc, "Failure")))))),
                        (`Id (_loc, (`Uid (_loc, "None")))))))),
                  (`Or
                     (_loc,
                       (`Case
                          (_loc,
                            (`App
                               (_loc, (`Id (_loc, (`Uid (_loc, "Some")))), p)),
                        skont)),
                       (`Case (_loc, (`Any _loc),  ckont)))))
  | SpStr (_loc,p) ->
      (try
         match p with
         | `Id (_loc,`Lid (_,v)) -> subst v skont
         | _ -> raise Not_found
       with
       | Not_found  ->
           `LetIn
             (_loc, (`ReNil _loc),
               (`Bind (_loc, p, (`Id (_loc, (`Lid (_loc, strm_n)))))), skont))
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
                         (`Id
                            (_loc,
                              (`Dot
                                 (_loc, (`Uid (_loc, (gm ()))),
                                   (`Lid (_loc, "count")))))),
                         (`Id (_loc, (`Lid (_loc, strm_n)))))))), e)
       | _ -> e)
  | (spc,err)::spcl ->
      let skont =
        let ekont err =
          let str = match err with | Some estr -> estr | _ -> `Str (_loc, "") in
          `App
            (_loc, (`Id (_loc, (`Lid (_loc, "raise")))),
              (`App
                 (_loc,
                   (`Id
                      (_loc,
                        (`Dot
                           (_loc, (`Uid (_loc, (gm ()))),
                             (`Uid (_loc, "Error")))))), str))) in
        stream_pattern _loc epo e ekont spcl in
      let ckont = ekont err in stream_pattern_component skont ckont spc
let stream_patterns_term _loc ekont tspel =
  let pel =
    List.fold_right
      (fun (p,w,_loc,spcl,epo,e)  acc  ->
         let p = `App (_loc, (`Id (_loc, (`Uid (_loc, "Some")))), p) in
         let e =
           let ekont err =
             let str =
               match err with | Some estr -> estr | _ -> `Str (_loc, "") in
             `App
               (_loc, (`Id (_loc, (`Lid (_loc, "raise")))),
                 (`App
                    (_loc,
                      (`Id
                         (_loc,
                           (`Dot
                              (_loc, (`Uid (_loc, (gm ()))),
                                (`Uid (_loc, "Error")))))), str))) in
           let skont = stream_pattern _loc epo e ekont spcl in
           `Seq
             (_loc,
               (`Sem
                  (_loc,
                    (`App
                       (_loc, (junk_fun _loc),
                         (`Id (_loc, (`Lid (_loc, strm_n)))))), skont))) in
         match w with
         | Some w -> `Or (_loc, (`CaseWhen (_loc, p, w, e)), acc)
         | None  -> `Or (_loc, (`Case (_loc, p,  e)), acc)) tspel
      (`Nil _loc) in
  `Match
    (_loc,
      (`App (_loc, (peek_fun _loc), (`Id (_loc, (`Lid (_loc, strm_n)))))),
      (`Or (_loc, pel, (`Case (_loc, (`Any _loc),  (ekont ()))))))
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
        (_loc, (`Id (_loc, (`Lid (_loc, "raise")))),
          (`Id
             (_loc,
               (`Dot (_loc, (`Uid (_loc, (gm ()))), (`Uid (_loc, "Failure")))))))
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
                      (`Id
                         (_loc,
                           (`Dot
                              (_loc, (`Uid (_loc, (gm ()))),
                                (`Lid (_loc, "count")))))),
                      (`Id (_loc, (`Lid (_loc, strm_n)))))))), e)
    | None  -> e in
  let p =
    `Constraint
      (_loc, (`Id (_loc, (`Lid (_loc, strm_n)))),
        (`App
           (_loc,
             (`Id
                (_loc,
                  (`Dot (_loc, (`Uid (_loc, (gm ()))), (`Lid (_loc, "t")))))),
             (`Any _loc)))) in
  `Fun (_loc, (`Case (_loc, p,  e)))
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
                      (`Id
                         (_loc,
                           (`Dot
                              (_loc, (`Uid (_loc, (gm ()))),
                                (`Lid (_loc, "count")))))),
                      (`Id (_loc, (`Lid (_loc, strm_n)))))))), pc)
    | None  -> pc in
  match me with
  | `Id (_loc,`Lid (_,x)) when x = strm_n -> e
  | _ ->
      `LetIn
        (_loc, (`ReNil _loc),
          (`Bind
             (_loc,
               (`Constraint
                  (_loc, (`Id (_loc, (`Lid (_loc, strm_n)))),
                    (`App
                       (_loc,
                         (`Id
                            (_loc,
                              (`Dot
                                 (_loc, (`Uid (_loc, (gm ()))),
                                   (`Lid (_loc, "t")))))), (`Any _loc))))),
               me)), e)
let rec not_computing =
  function
  | `Id (_loc,`Lid (_,_))|`Id (_loc,`Uid (_,_))|`Int (_loc,_)|`Flo (_loc,_)
    |`Chr (_loc,_)|`Str (_loc,_) -> true
  | `App (_loc,x,y) -> (is_cons_apply_not_computing x) && (not_computing y)
  | _ -> false
and is_cons_apply_not_computing =
  function
  | `Id (_loc,`Uid (_,_)) -> true
  | `Id (_loc,`Lid (_,_)) -> false
  | `App (_loc,x,y) -> (is_cons_apply_not_computing x) && (not_computing y)
  | _ -> false
let slazy _loc e =
  match e with
  | `App (_loc,f,`Id (_,`Uid (_,"()"))) ->
      (match f with
       | `Id (_loc,`Lid (_,_)) -> f
       | _ -> `Fun (_loc, (`Case (_loc, (`Any _loc),  e))))
  | _ -> `Fun (_loc, (`Case (_loc, (`Any _loc),  e)))
let rec cstream gloc =
  function
  | [] ->
      let _loc = gloc in
      `Id
        (_loc,
          (`Dot (_loc, (`Uid (_loc, "XStream")), (`Lid (_loc, "sempty")))))
  | (SeTrm (_loc,e))::[] ->
      if not_computing e
      then
        `App
          (_loc,
            (`Id
               (_loc,
                 (`Dot (_loc, (`Uid (_loc, (gm ()))), (`Lid (_loc, "ising")))))),
            e)
      else
        `App
          (_loc,
            (`Id
               (_loc,
                 (`Dot (_loc, (`Uid (_loc, (gm ()))), (`Lid (_loc, "lsing")))))),
            (slazy _loc e))
  | (SeTrm (_loc,e))::secl ->
      if not_computing e
      then
        `App
          (_loc,
            (`App
               (_loc,
                 (`Id
                    (_loc,
                      (`Dot
                         (_loc, (`Uid (_loc, (gm ()))),
                           (`Lid (_loc, "icons")))))), e)),
            (cstream gloc secl))
      else
        `App
          (_loc,
            (`App
               (_loc,
                 (`Id
                    (_loc,
                      (`Dot
                         (_loc, (`Uid (_loc, (gm ()))),
                           (`Lid (_loc, "lcons")))))), (slazy _loc e))),
            (cstream gloc secl))
  | (SeNtr (_loc,e))::[] ->
      if not_computing e
      then e
      else
        `App
          (_loc,
            (`Id
               (_loc,
                 (`Dot (_loc, (`Uid (_loc, (gm ()))), (`Lid (_loc, "slazy")))))),
            (slazy _loc e))
  | (SeNtr (_loc,e))::secl ->
      if not_computing e
      then
        `App
          (_loc,
            (`App
               (_loc,
                 (`Id
                    (_loc,
                      (`Dot
                         (_loc, (`Uid (_loc, (gm ()))),
                           (`Lid (_loc, "iapp")))))), e)),
            (cstream gloc secl))
      else
        `App
          (_loc,
            (`App
               (_loc,
                 (`Id
                    (_loc,
                      (`Dot
                         (_loc, (`Uid (_loc, (gm ()))),
                           (`Lid (_loc, "lapp")))))), (slazy _loc e))),
            (cstream gloc secl))
