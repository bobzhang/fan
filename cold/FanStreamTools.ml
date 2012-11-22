module Ast = Camlp4Ast
open Lib
type spat_comp =  
  | SpTrm of FanLoc.t* Ast.patt* Ast.expr option
  | SpNtr of FanLoc.t* Ast.patt* Ast.expr
  | SpStr of FanLoc.t* Ast.patt 
type sexp_comp =  
  | SeTrm of FanLoc.t* Ast.expr
  | SeNtr of FanLoc.t* Ast.expr 
let grammar_module_name = ref "Stream"
let gm () = grammar_module_name.contents
let strm_n = "__strm"
let peek_fun _loc =
  Ast.ExId
    (_loc,
      (Ast.IdAcc
         (_loc, (Ast.IdUid (_loc, (gm ()))), (Ast.IdLid (_loc, "peek")))))
let junk_fun _loc =
  Ast.ExId
    (_loc,
      (Ast.IdAcc
         (_loc, (Ast.IdUid (_loc, (gm ()))), (Ast.IdLid (_loc, "junk")))))
let empty _loc =
  Ast.ExId
    (_loc,
      (Ast.IdAcc
         (_loc, (Ast.IdUid (_loc, (gm ()))), (Ast.IdLid (_loc, "sempty")))))
let is_raise =
  function
  | Ast.ExApp (_,Ast.ExId (_,Ast.IdLid (_,"raise")),_) -> true
  | _ -> false
let is_raise_failure =
  function
  | Ast.ExApp
      (_,Ast.ExId (_,Ast.IdLid (_,"raise")),Ast.ExId
       (_,Ast.IdAcc (_,Ast.IdUid (_,m),Ast.IdUid (_,"Failure"))))
      when m = (gm ()) -> true
  | _ -> false
let rec handle_failure e =
  match e with
  | Ast.ExTry
      (_,_,Ast.McArr
       (_,Ast.PaId
        (_,Ast.IdAcc (_,Ast.IdUid (_,m),Ast.IdUid (_,"Failure"))),Ast.ExNil
        _,e))
      when m = (gm ()) -> handle_failure e
  | Ast.ExMat (_,me,a) ->
      let rec match_case_handle_failure =
        function
        | Ast.McOr (_,a1,a2) ->
            (match_case_handle_failure a1) && (match_case_handle_failure a2)
        | Ast.McArr (_,_,Ast.ExNil _,e) -> handle_failure e
        | _ -> false in
      (handle_failure me) && (match_case_handle_failure a)
  | Ast.ExLet (_,Ast.ReNil ,bi,e) ->
      let rec binding_handle_failure =
        function
        | Ast.BiAnd (_,b1,b2) ->
            (binding_handle_failure b1) && (binding_handle_failure b2)
        | Ast.BiEq (_,_,e) -> handle_failure e
        | _ -> false in
      (binding_handle_failure bi) && (handle_failure e)
  | Ast.ExId (_,Ast.IdLid (_,_))|Ast.ExInt (_,_)|Ast.ExStr (_,_)|Ast.ExChr
      (_,_)|Ast.ExFun (_,_)|Ast.ExId (_,Ast.IdUid (_,_)) -> true
  | Ast.ExApp (_,Ast.ExId (_,Ast.IdLid (_,"raise")),e) ->
      (match e with
       | Ast.ExId (_,Ast.IdAcc (_,Ast.IdUid (_,m),Ast.IdUid (_,"Failure")))
           when m = (gm ()) -> false
       | _ -> true)
  | Ast.ExApp (_,f,x) ->
      (is_constr_apply f) && ((handle_failure f) && (handle_failure x))
  | _ -> false
and is_constr_apply =
  function
  | Ast.ExId (_,Ast.IdUid (_,_)) -> true
  | Ast.ExId (_,Ast.IdLid (_,_)) -> false
  | Ast.ExApp (_,x,_) -> is_constr_apply x
  | _ -> false
let rec subst v e =
  let _loc = Ast.loc_of_expr e in
  match e with
  | Ast.ExId (_,Ast.IdLid (_,x)) ->
      let x = if x = v then strm_n else x in
      Ast.ExId (_loc, (Ast.IdLid (_loc, x)))
  | Ast.ExId (_,Ast.IdUid (_,_))|Ast.ExInt (_,_)|Ast.ExChr (_,_)|Ast.ExStr
      (_,_)|Ast.ExAcc (_,_,_) -> e
  | Ast.ExLet (_,rf,bi,e) ->
      Ast.ExLet (_loc, rf, (subst_binding v bi), (subst v e))
  | Ast.ExApp (_,e1,e2) -> Ast.ExApp (_loc, (subst v e1), (subst v e2))
  | Ast.ExTup (_,e) -> Ast.ExTup (_loc, (subst v e))
  | Ast.ExCom (_,e1,e2) -> Ast.ExCom (_loc, (subst v e1), (subst v e2))
  | _ -> raise Not_found
and subst_binding v =
  function
  | Ast.BiAnd (_loc,b1,b2) ->
      Ast.BiAnd (_loc, (subst_binding v b1), (subst_binding v b2))
  | Ast.BiEq (_loc,Ast.PaId (_,Ast.IdLid (_,v')),e) ->
      Ast.BiEq
        (_loc, (Ast.PaId (_loc, (Ast.IdLid (_loc, v')))),
          (if v = v' then e else subst v e))
  | _ -> raise Not_found
let stream_pattern_component skont ckont =
  function
  | SpTrm (_loc,p,None ) ->
      Ast.ExMat
        (_loc,
          (Ast.ExApp
             (_loc, (peek_fun _loc),
               (Ast.ExId (_loc, (Ast.IdLid (_loc, strm_n)))))),
          (Ast.McOr
             (_loc,
               (Ast.McArr
                  (_loc,
                    (Ast.PaApp
                       (_loc, (Ast.PaId (_loc, (Ast.IdUid (_loc, "Some")))),
                         p)), (Ast.ExNil _loc),
                    (Ast.ExSeq
                       (_loc,
                         (Ast.ExSem
                            (_loc,
                              (Ast.ExApp
                                 (_loc, (junk_fun _loc),
                                   (Ast.ExId
                                      (_loc, (Ast.IdLid (_loc, strm_n)))))),
                              skont)))))),
               (Ast.McArr (_loc, (Ast.PaAny _loc), (Ast.ExNil _loc), ckont)))))
  | SpTrm (_loc,p,Some w) ->
      Ast.ExMat
        (_loc,
          (Ast.ExApp
             (_loc, (peek_fun _loc),
               (Ast.ExId (_loc, (Ast.IdLid (_loc, strm_n)))))),
          (Ast.McOr
             (_loc,
               (Ast.McArr
                  (_loc,
                    (Ast.PaApp
                       (_loc, (Ast.PaId (_loc, (Ast.IdUid (_loc, "Some")))),
                         p)), w,
                    (Ast.ExSeq
                       (_loc,
                         (Ast.ExSem
                            (_loc,
                              (Ast.ExApp
                                 (_loc, (junk_fun _loc),
                                   (Ast.ExId
                                      (_loc, (Ast.IdLid (_loc, strm_n)))))),
                              skont)))))),
               (Ast.McArr (_loc, (Ast.PaAny _loc), (Ast.ExNil _loc), ckont)))))
  | SpNtr (_loc,p,e) ->
      let e =
        match e with
        | Ast.ExFun
            (_,Ast.McArr
             (_,Ast.PaTyc
              (_,Ast.PaId (_,Ast.IdLid (_,v)),Ast.TyApp
               (_,Ast.TyId
                (_,Ast.IdAcc (_,Ast.IdUid (_,m),Ast.IdLid (_,"t"))),Ast.TyAny
                _)),Ast.ExNil
              _,e))
            when (v = strm_n) && (m = (gm ())) -> e
        | _ ->
            Ast.ExApp
              (_loc, e, (Ast.ExId (_loc, (Ast.IdLid (_loc, strm_n))))) in
      if Expr.pattern_eq_expression p skont
      then
        (if is_raise_failure ckont
         then e
         else
           if handle_failure e
           then e
           else
             Ast.ExTry
               (_loc, e,
                 (Ast.McArr
                    (_loc,
                      (Ast.PaId
                         (_loc,
                           (Ast.IdAcc
                              (_loc, (Ast.IdUid (_loc, (gm ()))),
                                (Ast.IdUid (_loc, "Failure")))))),
                      (Ast.ExNil _loc), ckont))))
      else
        if is_raise_failure ckont
        then Ast.ExLet (_loc, Ast.ReNil, (Ast.BiEq (_loc, p, e)), skont)
        else
          if
            Expr.pattern_eq_expression
              (Ast.PaApp
                 (_loc, (Ast.PaId (_loc, (Ast.IdUid (_loc, "Some")))), p))
              skont
          then
            Ast.ExTry
              (_loc,
                (Ast.ExApp
                   (_loc, (Ast.ExId (_loc, (Ast.IdUid (_loc, "Some")))), e)),
                (Ast.McArr
                   (_loc,
                     (Ast.PaId
                        (_loc,
                          (Ast.IdAcc
                             (_loc, (Ast.IdUid (_loc, (gm ()))),
                               (Ast.IdUid (_loc, "Failure")))))),
                     (Ast.ExNil _loc), ckont)))
          else
            if is_raise ckont
            then
              (let tst =
                 if handle_failure e
                 then e
                 else
                   Ast.ExTry
                     (_loc, e,
                       (Ast.McArr
                          (_loc,
                            (Ast.PaId
                               (_loc,
                                 (Ast.IdAcc
                                    (_loc, (Ast.IdUid (_loc, (gm ()))),
                                      (Ast.IdUid (_loc, "Failure")))))),
                            (Ast.ExNil _loc), ckont))) in
               Ast.ExLet (_loc, Ast.ReNil, (Ast.BiEq (_loc, p, tst)), skont))
            else
              Ast.ExMat
                (_loc,
                  (Ast.ExTry
                     (_loc,
                       (Ast.ExApp
                          (_loc,
                            (Ast.ExId (_loc, (Ast.IdUid (_loc, "Some")))), e)),
                       (Ast.McArr
                          (_loc,
                            (Ast.PaId
                               (_loc,
                                 (Ast.IdAcc
                                    (_loc, (Ast.IdUid (_loc, (gm ()))),
                                      (Ast.IdUid (_loc, "Failure")))))),
                            (Ast.ExNil _loc),
                            (Ast.ExId (_loc, (Ast.IdUid (_loc, "None")))))))),
                  (Ast.McOr
                     (_loc,
                       (Ast.McArr
                          (_loc,
                            (Ast.PaApp
                               (_loc,
                                 (Ast.PaId (_loc, (Ast.IdUid (_loc, "Some")))),
                                 p)), (Ast.ExNil _loc), skont)),
                       (Ast.McArr
                          (_loc, (Ast.PaAny _loc), (Ast.ExNil _loc), ckont)))))
  | SpStr (_loc,p) ->
      (try
         match p with
         | Ast.PaId (_,Ast.IdLid (_,v)) -> subst v skont
         | _ -> raise Not_found
       with
       | Not_found  ->
           Ast.ExLet
             (_loc, Ast.ReNil,
               (Ast.BiEq
                  (_loc, p, (Ast.ExId (_loc, (Ast.IdLid (_loc, strm_n)))))),
               skont))
let rec stream_pattern _loc epo e ekont =
  function
  | [] ->
      (match epo with
       | Some ep ->
           Ast.ExLet
             (_loc, Ast.ReNil,
               (Ast.BiEq
                  (_loc, ep,
                    (Ast.ExApp
                       (_loc,
                         (Ast.ExId
                            (_loc,
                              (Ast.IdAcc
                                 (_loc, (Ast.IdUid (_loc, (gm ()))),
                                   (Ast.IdLid (_loc, "count")))))),
                         (Ast.ExId (_loc, (Ast.IdLid (_loc, strm_n)))))))),
               e)
       | _ -> e)
  | (spc,err)::spcl ->
      let skont =
        let ekont err =
          let str =
            match err with | Some estr -> estr | _ -> Ast.ExStr (_loc, "") in
          Ast.ExApp
            (_loc, (Ast.ExId (_loc, (Ast.IdLid (_loc, "raise")))),
              (Ast.ExApp
                 (_loc,
                   (Ast.ExId
                      (_loc,
                        (Ast.IdAcc
                           (_loc, (Ast.IdUid (_loc, (gm ()))),
                             (Ast.IdUid (_loc, "Error")))))), str))) in
        stream_pattern _loc epo e ekont spcl in
      let ckont = ekont err in stream_pattern_component skont ckont spc
let stream_patterns_term _loc ekont tspel =
  let pel =
    List.fold_right
      (fun (p,w,_loc,spcl,epo,e)  acc  ->
         let p =
           Ast.PaApp (_loc, (Ast.PaId (_loc, (Ast.IdUid (_loc, "Some")))), p) in
         let e =
           let ekont err =
             let str =
               match err with | Some estr -> estr | _ -> Ast.ExStr (_loc, "") in
             Ast.ExApp
               (_loc, (Ast.ExId (_loc, (Ast.IdLid (_loc, "raise")))),
                 (Ast.ExApp
                    (_loc,
                      (Ast.ExId
                         (_loc,
                           (Ast.IdAcc
                              (_loc, (Ast.IdUid (_loc, (gm ()))),
                                (Ast.IdUid (_loc, "Error")))))), str))) in
           let skont = stream_pattern _loc epo e ekont spcl in
           Ast.ExSeq
             (_loc,
               (Ast.ExSem
                  (_loc,
                    (Ast.ExApp
                       (_loc, (junk_fun _loc),
                         (Ast.ExId (_loc, (Ast.IdLid (_loc, strm_n)))))),
                    skont))) in
         match w with
         | Some w -> Ast.McOr (_loc, (Ast.McArr (_loc, p, w, e)), acc)
         | None  ->
             Ast.McOr (_loc, (Ast.McArr (_loc, p, (Ast.ExNil _loc), e)), acc))
      tspel (Ast.McNil _loc) in
  Ast.ExMat
    (_loc,
      (Ast.ExApp
         (_loc, (peek_fun _loc),
           (Ast.ExId (_loc, (Ast.IdLid (_loc, strm_n)))))),
      (Ast.McOr
         (_loc, pel,
           (Ast.McArr (_loc, (Ast.PaAny _loc), (Ast.ExNil _loc), (ekont ()))))))
let rec group_terms =
  function
  | ((SpTrm (_loc,p,w),None )::spcl,epo,e)::spel ->
      let (tspel,spel) = group_terms spel in
      (((p, w, _loc, spcl, epo, e) :: tspel), spel)
  | spel -> ([], spel)
let rec parser_cases _loc =
  function
  | [] ->
      Ast.ExApp
        (_loc, (Ast.ExId (_loc, (Ast.IdLid (_loc, "raise")))),
          (Ast.ExId
             (_loc,
               (Ast.IdAcc
                  (_loc, (Ast.IdUid (_loc, (gm ()))),
                    (Ast.IdUid (_loc, "Failure")))))))
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
        Ast.ExLet
          (_loc, Ast.ReNil,
            (Ast.BiEq
               (_loc, bp,
                 (Ast.ExApp
                    (_loc,
                      (Ast.ExId
                         (_loc,
                           (Ast.IdAcc
                              (_loc, (Ast.IdUid (_loc, (gm ()))),
                                (Ast.IdLid (_loc, "count")))))),
                      (Ast.ExId (_loc, (Ast.IdLid (_loc, strm_n)))))))), e)
    | None  -> e in
  let p =
    Ast.PaTyc
      (_loc, (Ast.PaId (_loc, (Ast.IdLid (_loc, strm_n)))),
        (Ast.TyApp
           (_loc,
             (Ast.TyId
                (_loc,
                  (Ast.IdAcc
                     (_loc, (Ast.IdUid (_loc, (gm ()))),
                       (Ast.IdLid (_loc, "t")))))), (Ast.TyAny _loc)))) in
  Ast.ExFun (_loc, (Ast.McArr (_loc, p, (Ast.ExNil _loc), e)))
let cparser_match _loc me bpo pc =
  let pc = parser_cases _loc pc in
  let e =
    match bpo with
    | Some bp ->
        Ast.ExLet
          (_loc, Ast.ReNil,
            (Ast.BiEq
               (_loc, bp,
                 (Ast.ExApp
                    (_loc,
                      (Ast.ExId
                         (_loc,
                           (Ast.IdAcc
                              (_loc, (Ast.IdUid (_loc, (gm ()))),
                                (Ast.IdLid (_loc, "count")))))),
                      (Ast.ExId (_loc, (Ast.IdLid (_loc, strm_n)))))))), pc)
    | None  -> pc in
  match me with
  | Ast.ExId (_,Ast.IdLid (_,x)) when x = strm_n -> e
  | _ ->
      Ast.ExLet
        (_loc, Ast.ReNil,
          (Ast.BiEq
             (_loc,
               (Ast.PaTyc
                  (_loc, (Ast.PaId (_loc, (Ast.IdLid (_loc, strm_n)))),
                    (Ast.TyApp
                       (_loc,
                         (Ast.TyId
                            (_loc,
                              (Ast.IdAcc
                                 (_loc, (Ast.IdUid (_loc, (gm ()))),
                                   (Ast.IdLid (_loc, "t")))))),
                         (Ast.TyAny _loc))))), me)), e)
let rec not_computing =
  function
  | Ast.ExId (_,Ast.IdLid (_,_))|Ast.ExId (_,Ast.IdUid (_,_))|Ast.ExInt
      (_,_)|Ast.ExFlo (_,_)|Ast.ExChr (_,_)|Ast.ExStr (_,_) -> true
  | Ast.ExApp (_,x,y) -> (is_cons_apply_not_computing x) && (not_computing y)
  | _ -> false
and is_cons_apply_not_computing =
  function
  | Ast.ExId (_,Ast.IdUid (_,_)) -> true
  | Ast.ExId (_,Ast.IdLid (_,_)) -> false
  | Ast.ExApp (_,x,y) -> (is_cons_apply_not_computing x) && (not_computing y)
  | _ -> false
let slazy _loc e =
  match e with
  | Ast.ExApp (_,f,Ast.ExId (_,Ast.IdUid (_,"()"))) ->
      (match f with
       | Ast.ExId (_,Ast.IdLid (_,_)) -> f
       | _ ->
           Ast.ExFun
             (_loc,
               (Ast.McArr (_loc, (Ast.PaAny _loc), (Ast.ExNil _loc), e))))
  | _ ->
      Ast.ExFun
        (_loc, (Ast.McArr (_loc, (Ast.PaAny _loc), (Ast.ExNil _loc), e)))
let rec cstream gloc =
  function
  | [] ->
      let _loc = gloc in
      Ast.ExId
        (_loc,
          (Ast.IdAcc
             (_loc, (Ast.IdUid (_loc, "Stream")),
               (Ast.IdLid (_loc, "sempty")))))
  | (SeTrm (_loc,e))::[] ->
      if not_computing e
      then
        Ast.ExApp
          (_loc,
            (Ast.ExId
               (_loc,
                 (Ast.IdAcc
                    (_loc, (Ast.IdUid (_loc, (gm ()))),
                      (Ast.IdLid (_loc, "ising")))))), e)
      else
        Ast.ExApp
          (_loc,
            (Ast.ExId
               (_loc,
                 (Ast.IdAcc
                    (_loc, (Ast.IdUid (_loc, (gm ()))),
                      (Ast.IdLid (_loc, "lsing")))))), (slazy _loc e))
  | (SeTrm (_loc,e))::secl ->
      if not_computing e
      then
        Ast.ExApp
          (_loc,
            (Ast.ExApp
               (_loc,
                 (Ast.ExId
                    (_loc,
                      (Ast.IdAcc
                         (_loc, (Ast.IdUid (_loc, (gm ()))),
                           (Ast.IdLid (_loc, "icons")))))), e)),
            (cstream gloc secl))
      else
        Ast.ExApp
          (_loc,
            (Ast.ExApp
               (_loc,
                 (Ast.ExId
                    (_loc,
                      (Ast.IdAcc
                         (_loc, (Ast.IdUid (_loc, (gm ()))),
                           (Ast.IdLid (_loc, "lcons")))))), (slazy _loc e))),
            (cstream gloc secl))
  | (SeNtr (_loc,e))::[] ->
      if not_computing e
      then e
      else
        Ast.ExApp
          (_loc,
            (Ast.ExId
               (_loc,
                 (Ast.IdAcc
                    (_loc, (Ast.IdUid (_loc, (gm ()))),
                      (Ast.IdLid (_loc, "slazy")))))), (slazy _loc e))
  | (SeNtr (_loc,e))::secl ->
      if not_computing e
      then
        Ast.ExApp
          (_loc,
            (Ast.ExApp
               (_loc,
                 (Ast.ExId
                    (_loc,
                      (Ast.IdAcc
                         (_loc, (Ast.IdUid (_loc, (gm ()))),
                           (Ast.IdLid (_loc, "iapp")))))), e)),
            (cstream gloc secl))
      else
        Ast.ExApp
          (_loc,
            (Ast.ExApp
               (_loc,
                 (Ast.ExId
                    (_loc,
                      (Ast.IdAcc
                         (_loc, (Ast.IdUid (_loc, (gm ()))),
                           (Ast.IdLid (_loc, "lapp")))))), (slazy _loc e))),
            (cstream gloc secl))