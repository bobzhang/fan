open FAst
open AstLib
type spat_comp =  
  | SpWhen of loc* pat* exp option
  | SpMatch of loc* pat* exp
  | SpStr of loc* pat 
type stream_pat = (spat_comp* exp option) 
type stream_pats = stream_pat list 
type stream_case = (stream_pats* pat option* exp) 
type stream_cases = stream_case list 
let grammar_module_name = ref "Fstream"
let gm () = grammar_module_name.contents
let strm_n = "__strm"
let peek_fun _loc =
  (`Dot (_loc, (`Uid (_loc, (gm ()))), (`Lid (_loc, "peek"))) : FAst.exp )
let junk_fun _loc =
  (`Dot (_loc, (`Uid (_loc, (gm ()))), (`Lid (_loc, "junk"))) : FAst.exp )
let empty _loc =
  (`Dot (_loc, (`Uid (_loc, (gm ()))), (`Lid (_loc, "sempty"))) : FAst.exp )
let rec is_constr_apply a =
  match a with
  | (`Uid (_loc,_) : FAst.exp) -> true
  | (`App (_loc,x,_) : FAst.exp) -> is_constr_apply x
  | (`Lid (_loc,_) : FAst.exp) -> false
  | _ -> false
let rec handle_failure e =
  match e with
  | (`Try (_loc,_,`Case (_,`Dot (_,`Uid (_,m),`Uid (_,"NotConsumed")),e)) :
      FAst.exp) when m = (gm ()) -> handle_failure e
  | (`Match (_loc,me,a) : FAst.exp) ->
      let rec case_handle_failure =
        function
        | (`Bar (_loc,a1,a2) : FAst.case) ->
            (case_handle_failure a1) && (case_handle_failure a2)
        | (`Case (_loc,_,e) : FAst.case) -> handle_failure e
        | _ -> false in
      (handle_failure me) && (case_handle_failure a)
  | (`LetIn (_loc,`Negative _,bi,e) : FAst.exp) ->
      let rec bind_handle_failure =
        function
        | (`And (_loc,b1,b2) : FAst.bind) ->
            (bind_handle_failure b1) && (bind_handle_failure b2)
        | (`Bind (_loc,_,e) : FAst.bind) -> handle_failure e
        | _ -> false in
      (bind_handle_failure bi) && (handle_failure e)
  | #literal|#vid' -> true
  | (`Fun (_loc,_) : FAst.exp) -> true
  | (`App (_loc,`Lid (_,"raise"),`Dot (_,`Uid (_,m),`Uid (_,"NotConsumed")))
      : FAst.exp) -> m <> (gm ())
  | (`App (_loc,`Lid (_,"raise"),_) : FAst.exp) -> true
  | (`App (_loc,f,x) : FAst.exp) ->
      (is_constr_apply f) && ((handle_failure f) && (handle_failure x))
  | _ -> false
let stream_pattern_component (skont : exp) (ckont : exp) (x : spat_comp) =
  (match (x : spat_comp ) with
   | SpWhen (_loc,p,None ) ->
       (`Match
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
                 (`Case (_loc, (`Any _loc), ckont))))) : FAst.exp )
   | SpWhen (_loc,p,Some w) ->
       (`Match
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
                 (`Case (_loc, (`Any _loc), ckont))))) : FAst.exp )
   | SpMatch (_loc,p,e) ->
       let rec pat_eq_exp p e =
         match (p, e) with
         | ((`Lid (_loc,a) : FAst.pat),(`Lid (_,b) : FAst.exp))
           |((`Uid (_loc,a) : FAst.pat),(`Uid (_,b) : FAst.exp)) -> a = b
         | ((`App (_loc,p1,p2) : FAst.pat),(`App (_,e1,e2) : FAst.exp)) ->
             (pat_eq_exp p1 e1) && (pat_eq_exp p2 e2)
         | _ -> false in
       let e =
         match e with
         | (`Fun
              (_loc,`Case
                      (_,`Constraint
                           (_,`Lid (_,v),`App
                                           (_,`Dot
                                                (_,`Uid (_,m),`Lid (_,"t")),
                                            `Any _)),e))
             : FAst.exp) when (v = strm_n) && (m = (gm ())) -> e
         | _ -> (`App (_loc, e, (`Lid (_loc, strm_n))) : FAst.exp ) in
       if pat_eq_exp p skont
       then
         (if
            ((function
              | (`App
                   (_loc,`Lid (_,"raise"),`Dot
                                            (_,`Uid (_,m),`Uid
                                                            (_,"NotConsumed")))
                  : FAst.exp) when m = (gm ()) -> true
              | _ -> false) ckont) || (handle_failure e)
          then e
          else
            (`Try
               (_loc, e,
                 (`Case
                    (_loc,
                      (`Dot
                         (_loc, (`Uid (_loc, (gm ()))),
                           (`Uid (_loc, "NotConsumed")))), ckont))) : 
            FAst.exp ))
       else
         if
           ((function
             | (`App
                  (_loc,`Lid (_,"raise"),`Dot
                                           (_,`Uid (_,m),`Uid
                                                           (_,"NotConsumed")))
                 : FAst.exp) when m = (gm ()) -> true
             | _ -> false)) ckont
         then
           (`LetIn (_loc, (`Negative _loc), (`Bind (_loc, p, e)), skont) : 
           FAst.exp )
         else
           if
             pat_eq_exp (`App (_loc, (`Uid (_loc, "Some")), p) : FAst.pat )
               skont
           then
             (`Try
                (_loc, (`App (_loc, (`Uid (_loc, "Some")), e)),
                  (`Case
                     (_loc,
                       (`Dot
                          (_loc, (`Uid (_loc, (gm ()))),
                            (`Uid (_loc, "NotConsumed")))), ckont))) : 
             FAst.exp )
           else
             if
               ((function
                 | (`App (_loc,`Lid (_,"raise"),_) : FAst.exp) -> true
                 | _ -> false)) ckont
             then
               (let tst =
                  if handle_failure e
                  then e
                  else
                    (`Try
                       (_loc, e,
                         (`Case
                            (_loc,
                              (`Dot
                                 (_loc, (`Uid (_loc, (gm ()))),
                                   (`Uid (_loc, "NotConsumed")))), ckont))) : 
                    FAst.exp ) in
                (`LetIn
                   (_loc, (`Negative _loc), (`Bind (_loc, p, tst)), skont) : 
                  FAst.exp ))
             else
               (`Match
                  (_loc,
                    (`Try
                       (_loc, (`App (_loc, (`Uid (_loc, "Some")), e)),
                         (`Case
                            (_loc,
                              (`Dot
                                 (_loc, (`Uid (_loc, (gm ()))),
                                   (`Uid (_loc, "NotConsumed")))),
                              (`Uid (_loc, "None")))))),
                    (`Bar
                       (_loc,
                         (`Case
                            (_loc, (`App (_loc, (`Uid (_loc, "Some")), p)),
                              skont)), (`Case (_loc, (`Any _loc), ckont))))) : 
               FAst.exp )
   | SpStr (_loc,p) ->
       let rec subst (v : string) (e : exp) =
         let _loc = loc_of e in
         match e with
         | (`Lid (_loc,x) : FAst.exp) ->
             let x = if x = v then strm_n else x in
             (`Lid (_loc, x) : FAst.exp )
         | (`Uid (_loc,_) : FAst.exp)|(`Int (_loc,_) : FAst.exp)
           |(`Chr (_loc,_) : FAst.exp)|(`Str (_loc,_) : FAst.exp)
           |(`Field (_loc,_,_) : FAst.exp) -> e
         | (`LetIn (_loc,rf,bi,e) : FAst.exp) ->
             (`LetIn (_loc, rf, (subst_bind v bi), (subst v e)) : FAst.exp )
         | (`App (_loc,e1,e2) : FAst.exp) ->
             (`App (_loc, (subst v e1), (subst v e2)) : FAst.exp )
         | (`Par (_loc,e) : FAst.exp) ->
             (`Par (_loc, (subst v e)) : FAst.exp )
         | (`Com (_loc,e1,e2) : FAst.exp) ->
             (`Com (_loc, (subst v e1), (subst v e2)) : FAst.exp )
         | _ -> raise Not_found
       and subst_bind v =
         function
         | (`And (_loc,b1,b2) : FAst.bind) ->
             (`And (_loc, (subst_bind v b1), (subst_bind v b2)) : FAst.bind )
         | (`Bind (_loc,`Lid (_,v'),e) : FAst.bind) ->
             (`Bind
                (_loc, (`Lid (_loc, v')), (if v = v' then e else subst v e)) : 
             FAst.bind )
         | _ -> raise Not_found in
       (try
          match p with
          | (`Lid (_loc,v) : FAst.pat) -> subst v skont
          | _ -> raise Not_found
        with
        | Not_found  ->
            (`LetIn
               (_loc, (`Negative _loc),
                 (`Bind (_loc, p, (`Lid (_loc, strm_n)))), skont) : FAst.exp )) : 
  exp )
let rec stream_pattern _loc (x,epo,e) (ekont : exp option -> exp) =
  match x with
  | [] ->
      (match epo with
       | Some ep ->
           (`LetIn
              (_loc, (`Negative _loc),
                (`Bind
                   (_loc, ep,
                     (`App
                        (_loc,
                          (`Dot
                             (_loc, (`Uid (_loc, (gm ()))),
                               (`Lid (_loc, "count")))),
                          (`Lid (_loc, strm_n)))))), e) : FAst.exp )
       | _ -> e)
  | (spc,err)::spcl ->
      let skont =
        let ekont0 err =
          let str =
            match err with
            | Some estr -> estr
            | _ -> (`Str (_loc, "") : FAst.exp ) in
          (`App
             (_loc, (`Lid (_loc, "raise")),
               (`App
                  (_loc,
                    (`Dot
                       (_loc, (`Uid (_loc, (gm ()))), (`Uid (_loc, "Error")))),
                    str))) : FAst.exp ) in
        stream_pattern _loc (spcl, epo, e) ekont0 in
      let ckont = ekont err in stream_pattern_component skont ckont spc
let rec group_terms (xs : stream_cases) =
  match xs with
  | ((SpWhen (_loc,p,w),None )::spcl,epo,e)::spel ->
      let (tspel,spel) = group_terms spel in
      (((p, w, _loc, spcl, epo, e) :: tspel), spel)
  | spel -> ([], spel)
let stream_patterns_term _loc (ekont : unit -> exp) tspel =
  (let pel =
     List.fold_right
       (fun (p,w,_loc,spcl,epo,e)  acc  ->
          let p: FAst.pat = `App (_loc, (`Uid (_loc, "Some")), p) in
          let e =
            let ekont err =
              let str =
                match err with
                | Some estr -> estr
                | _ -> (`Str (_loc, "") : FAst.exp ) in
              (`App
                 (_loc, (`Lid (_loc, "raise")),
                   (`App
                      (_loc,
                        (`Dot
                           (_loc, (`Uid (_loc, (gm ()))),
                             (`Uid (_loc, "Error")))), str))) : FAst.exp ) in
            let skont = stream_pattern _loc (spcl, epo, e) ekont in
            (`Seq
               (_loc,
                 (`Sem
                    (_loc,
                      (`App (_loc, (junk_fun _loc), (`Lid (_loc, strm_n)))),
                      skont))) : FAst.exp ) in
          match w with
          | Some w ->
              (`Bar (_loc, (`CaseWhen (_loc, p, w, e)), acc) : FAst.case )
          | None  -> (`Bar (_loc, (`Case (_loc, p, e)), acc) : FAst.case ))
       tspel (`Case (_loc, (`Any _loc), (ekont ())) : FAst.case ) in
   (`Match (_loc, (`App (_loc, (peek_fun _loc), (`Lid (_loc, strm_n)))), pel) : 
     FAst.exp ) : exp )
let rec parser_cases _loc (x : stream_cases) =
  match x with
  | [] ->
      (`App
         (_loc, (`Lid (_loc, "raise")),
           (`Dot (_loc, (`Uid (_loc, (gm ()))), (`Uid (_loc, "NotConsumed"))))) : 
      FAst.exp )
  | spel ->
      (match group_terms spel with
       | ([],x::spel) ->
           stream_pattern _loc x (fun _  -> parser_cases _loc spel)
       | (tspel,spel) ->
           stream_patterns_term _loc (fun _  -> parser_cases _loc spel) tspel)
let cparser _loc bpo pc =
  let e = parser_cases _loc pc in
  let e =
    match bpo with
    | Some bp ->
        (`LetIn
           (_loc, (`Negative _loc),
             (`Bind
                (_loc, bp,
                  (`App
                     (_loc,
                       (`Dot
                          (_loc, (`Uid (_loc, (gm ()))),
                            (`Lid (_loc, "count")))), (`Lid (_loc, strm_n)))))),
             e) : FAst.exp )
    | None  -> e in
  let p: FAst.pat =
    `Constraint
      (_loc, (`Lid (_loc, strm_n)),
        (`App
           (_loc, (`Dot (_loc, (`Uid (_loc, (gm ()))), (`Lid (_loc, "t")))),
             (`Any _loc)))) in
  (`Fun (_loc, (`Case (_loc, p, e))) : FAst.exp )
let cparser_match _loc me bpo pc =
  let pc = parser_cases _loc pc in
  let e =
    match bpo with
    | Some bp ->
        (`LetIn
           (_loc, (`Negative _loc),
             (`Bind
                (_loc, bp,
                  (`App
                     (_loc,
                       (`Dot
                          (_loc, (`Uid (_loc, (gm ()))),
                            (`Lid (_loc, "count")))), (`Lid (_loc, strm_n)))))),
             pc) : FAst.exp )
    | None  -> pc in
  match me with
  | (`Lid (_loc,x) : FAst.exp) when x = strm_n -> e
  | _ ->
      (`LetIn
         (_loc, (`Negative _loc),
           (`Bind
              (_loc,
                (`Constraint
                   (_loc, (`Lid (_loc, strm_n)),
                     (`App
                        (_loc,
                          (`Dot
                             (_loc, (`Uid (_loc, (gm ()))),
                               (`Lid (_loc, "t")))), (`Any _loc))))), me)),
           e) : FAst.exp )
type sexp_comp =  
  | SeTrm of loc* exp
  | SeNtr of loc* exp 
let not_computing x =
  let rec aux x =
    match x with
    | #literal -> true
    | #vid' -> true
    | (`App (_loc,x,y) : FAst.exp) ->
        (is_cons_apply_not_computing x) && (aux y)
    | _ -> false
  and is_cons_apply_not_computing =
    function
    | (`Uid (_loc,_) : FAst.exp) -> true
    | (`Lid (_loc,_) : FAst.exp) -> false
    | (`App (_loc,x,y) : FAst.exp) ->
        (is_cons_apply_not_computing x) && (aux y)
    | _ -> false in
  aux x
let slazy _loc e =
  match e with
  | (`App (_loc,f,`Uid (_,"()")) : FAst.exp) ->
      (match f with
       | (`Lid (_loc,_) : FAst.exp) -> f
       | _ -> (`Fun (_loc, (`Case (_loc, (`Any _loc), e))) : FAst.exp ))
  | _ -> (`Fun (_loc, (`Case (_loc, (`Any _loc), e))) : FAst.exp )
let rec cstream gloc =
  function
  | [] ->
      let _loc = gloc in
      (`Dot (_loc, (`Uid (_loc, (gm ()))), (`Lid (_loc, "sempty"))) : 
        FAst.exp )
  | (SeTrm (_loc,e))::[] ->
      if not_computing e
      then
        (`App
           (_loc,
             (`Dot (_loc, (`Uid (_loc, (gm ()))), (`Lid (_loc, "ising")))),
             e) : FAst.exp )
      else
        (`App
           (_loc,
             (`Dot (_loc, (`Uid (_loc, (gm ()))), (`Lid (_loc, "lsing")))),
             (slazy _loc e)) : FAst.exp )
  | (SeTrm (_loc,e))::secl ->
      if not_computing e
      then
        (`App
           (_loc,
             (`App
                (_loc,
                  (`Dot
                     (_loc, (`Uid (_loc, (gm ()))), (`Lid (_loc, "icons")))),
                  e)), (cstream gloc secl)) : FAst.exp )
      else
        (`App
           (_loc,
             (`App
                (_loc,
                  (`Dot
                     (_loc, (`Uid (_loc, (gm ()))), (`Lid (_loc, "lcons")))),
                  (slazy _loc e))), (cstream gloc secl)) : FAst.exp )
  | (SeNtr (_loc,e))::[] ->
      if not_computing e
      then e
      else
        (`App
           (_loc,
             (`Dot (_loc, (`Uid (_loc, (gm ()))), (`Lid (_loc, "slazy")))),
             (slazy _loc e)) : FAst.exp )
  | (SeNtr (_loc,e))::secl ->
      if not_computing e
      then
        (`App
           (_loc,
             (`App
                (_loc,
                  (`Dot (_loc, (`Uid (_loc, (gm ()))), (`Lid (_loc, "iapp")))),
                  e)), (cstream gloc secl)) : FAst.exp )
      else
        (`App
           (_loc,
             (`App
                (_loc,
                  (`Dot (_loc, (`Uid (_loc, (gm ()))), (`Lid (_loc, "lapp")))),
                  (slazy _loc e))), (cstream gloc secl)) : FAst.exp )