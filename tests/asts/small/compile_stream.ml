open Astf
open Ast_gen
type spat_comp =
  | When of loc* pat* exp option
  | Match of loc* pat* exp
  | Str of loc* pat
type stream_pat = (spat_comp* exp option)
type stream_pats = stream_pat list
type stream_case = (stream_pats* pat option* exp)
type stream_cases = stream_case list
let grammar_module_name = ref "Streamf"
let gm = function | () -> !grammar_module_name
let strm_n = "__strm"
let peek_fun =
  function
  | _loc ->
      (`Dot (_loc, (`Uid (_loc, (gm ()))), (`Lid (_loc, "peek"))) :> 
      Astf.exp)
let junk_fun =
  function
  | _loc ->
      (`Dot (_loc, (`Uid (_loc, (gm ()))), (`Lid (_loc, "junk"))) :> 
      Astf.exp)
let empty =
  function
  | _loc ->
      (`Dot (_loc, (`Uid (_loc, (gm ()))), (`Lid (_loc, "sempty"))) :> 
      Astf.exp)
let rec is_constr_apply =
  function
  | a ->
      (match a with
       | (`Uid (_loc,_) : Astf.exp) -> true
       | (`App (_loc,x,_) : Astf.exp) -> is_constr_apply x
       | (`Lid (_loc,_) : Astf.exp) -> false
       | _ -> false)
let rec handle_failure =
  function
  | e ->
      (match e with
       | (`Try
            (_loc,_,`Case (_,`Dot (_,`Uid (_,m),`Uid (_,"NotConsumed")),e))
           : Astf.exp) when m = (gm ()) -> handle_failure e
       | (`Match (_loc,me,a) : Astf.exp) ->
           let rec case_handle_failure =
             function
             | (`Bar (_loc,a1,a2) : Astf.case) ->
                 (case_handle_failure a1) && (case_handle_failure a2)
             | (`Case (_loc,_,e) : Astf.case) -> handle_failure e
             | _ -> false in
           (handle_failure me) && (case_handle_failure a)
       | (`LetIn (_loc,`Negative _,bi,e) : Astf.exp) ->
           let rec bind_handle_failure =
             function
             | (`And (_loc,b1,b2) : Astf.bind) ->
                 (bind_handle_failure b1) && (bind_handle_failure b2)
             | (`Bind (_loc,_,e) : Astf.bind) -> handle_failure e
             | _ -> false in
           (bind_handle_failure bi) && (handle_failure e)
       | #literal|#vid' -> true
       | (`Fun (_loc,_) : Astf.exp) -> true
       | (`App
            (_loc,`Lid (_,"raise"),`Dot (_,`Uid (_,m),`Uid (_,"NotConsumed")))
           : Astf.exp) -> m <> (gm ())
       | (`App (_loc,`Lid (_,"raise"),_) : Astf.exp) -> true
       | (`App (_loc,f,x) : Astf.exp) ->
           (is_constr_apply f) && ((handle_failure f) && (handle_failure x))
       | _ -> false)
let stream_pattern_component =
  function
  | (skont : exp) ->
      (function
       | (ckont : exp) ->
           (function
            | (x : spat_comp) ->
                ((match (x : spat_comp) with
                  | When (_loc,p,None ) ->
                      (`Match
                         (_loc,
                           (`App
                              (_loc, (peek_fun _loc :> Astf.exp),
                                (`Lid (_loc, strm_n)))),
                           (`Bar
                              (_loc,
                                (`Case
                                   (_loc,
                                     (`App
                                        (_loc, (`Uid (_loc, "Some")),
                                          (p :> Astf.pat))),
                                     (`Seq
                                        (_loc,
                                          (`Sem
                                             (_loc,
                                               (`App
                                                  (_loc,
                                                    (junk_fun _loc :> 
                                                    Astf.exp),
                                                    (`Lid (_loc, strm_n)))),
                                               (skont :> Astf.exp))))))),
                                (`Case
                                   (_loc, (`Any _loc), (ckont :> Astf.exp)))))) :> 
                      Astf.exp)
                  | When (_loc,p,Some w) ->
                      (`Match
                         (_loc,
                           (`App
                              (_loc, (peek_fun _loc :> Astf.exp),
                                (`Lid (_loc, strm_n)))),
                           (`Bar
                              (_loc,
                                (`CaseWhen
                                   (_loc,
                                     (`App
                                        (_loc, (`Uid (_loc, "Some")),
                                          (p :> Astf.pat))), (w :> Astf.exp),
                                     (`Seq
                                        (_loc,
                                          (`Sem
                                             (_loc,
                                               (`App
                                                  (_loc,
                                                    (junk_fun _loc :> 
                                                    Astf.exp),
                                                    (`Lid (_loc, strm_n)))),
                                               (skont :> Astf.exp))))))),
                                (`Case
                                   (_loc, (`Any _loc), (ckont :> Astf.exp)))))) :> 
                      Astf.exp)
                  | Match (_loc,p,e) ->
                      let rec pat_eq_exp =
                        function
                        | p ->
                            (function
                             | e ->
                                 (match (p, e) with
                                  | ((`Lid (_loc,a) : Astf.pat),(`Lid (_,b) :
                                                                  Astf.exp))
                                    |((`Uid (_loc,a) : Astf.pat),(`Uid (_,b)
                                                                   :
                                                                   Astf.exp))
                                      -> a = b
                                  | ((`App (_loc,p1,p2) : Astf.pat),(`App
                                                                    (_,e1,e2)
                                                                    :
                                                                    Astf.exp))
                                      ->
                                      (pat_eq_exp p1 e1) &&
                                        (pat_eq_exp p2 e2)
                                  | _ -> false)) in
                      let e =
                        match e with
                        | (`Fun
                             (_loc,`Case
                                     (_,`Constraint
                                          (_,`Lid (_,v),`App
                                                          (_,`Dot
                                                               (_,`Uid (_,m),
                                                                `Lid 
                                                                  (_,"t")),
                                                           `Any _)),e))
                            : Astf.exp) when (v = strm_n) && (m = (gm ())) ->
                            e
                        | _ ->
                            (`App
                               (_loc, (e :> Astf.exp), (`Lid (_loc, strm_n))) :> 
                            Astf.exp) in
                      if pat_eq_exp p skont
                      then
                        (if
                           ((function
                             | (`App
                                  (_loc,`Lid (_,"raise"),`Dot
                                                           (_,`Uid (_,m),
                                                            `Uid
                                                              (_,"NotConsumed")))
                                 : Astf.exp) when m = (gm ()) -> true
                             | _ -> false) ckont) || (handle_failure e)
                         then e
                         else
                           (`Try
                              (_loc, (e :> Astf.exp),
                                (`Case
                                   (_loc,
                                     (`Dot
                                        (_loc, (`Uid (_loc, (gm ()))),
                                          (`Uid (_loc, "NotConsumed")))),
                                     (ckont :> Astf.exp)))) :> Astf.exp))
                      else
                        if
                          ((function
                            | (`App
                                 (_loc,`Lid (_,"raise"),`Dot
                                                          (_,`Uid (_,m),
                                                           `Uid
                                                             (_,"NotConsumed")))
                                : Astf.exp) when m = (gm ()) -> true
                            | _ -> false)) ckont
                        then
                          (`LetIn
                             (_loc, (`Negative _loc),
                               (`Bind
                                  (_loc, (p :> Astf.pat), (e :> Astf.exp))),
                               (skont :> Astf.exp)) :> Astf.exp)
                        else
                          if
                            pat_eq_exp
                              (`App
                                 (_loc, (`Uid (_loc, "Some")),
                                   (p :> Astf.pat)) :> Astf.pat) skont
                          then
                            (`Try
                               (_loc,
                                 (`App
                                    (_loc, (`Uid (_loc, "Some")),
                                      (e :> Astf.exp))),
                                 (`Case
                                    (_loc,
                                      (`Dot
                                         (_loc, (`Uid (_loc, (gm ()))),
                                           (`Uid (_loc, "NotConsumed")))),
                                      (ckont :> Astf.exp)))) :> Astf.exp)
                          else
                            if
                              ((function
                                | (`App (_loc,`Lid (_,"raise"),_) : Astf.exp)
                                    -> true
                                | _ -> false)) ckont
                            then
                              (let tst =
                                 if handle_failure e
                                 then e
                                 else
                                   (`Try
                                      (_loc, (e :> Astf.exp),
                                        (`Case
                                           (_loc,
                                             (`Dot
                                                (_loc,
                                                  (`Uid (_loc, (gm ()))),
                                                  (`Uid (_loc, "NotConsumed")))),
                                             (ckont :> Astf.exp)))) :> 
                                   Astf.exp) in
                               (`LetIn
                                  (_loc, (`Negative _loc),
                                    (`Bind
                                       (_loc, (p :> Astf.pat),
                                         (tst :> Astf.exp))),
                                    (skont :> Astf.exp)) :> Astf.exp))
                            else
                              (`Match
                                 (_loc,
                                   (`Try
                                      (_loc,
                                        (`App
                                           (_loc, (`Uid (_loc, "Some")),
                                             (e :> Astf.exp))),
                                        (`Case
                                           (_loc,
                                             (`Dot
                                                (_loc,
                                                  (`Uid (_loc, (gm ()))),
                                                  (`Uid (_loc, "NotConsumed")))),
                                             (`Uid (_loc, "None")))))),
                                   (`Bar
                                      (_loc,
                                        (`Case
                                           (_loc,
                                             (`App
                                                (_loc, (`Uid (_loc, "Some")),
                                                  (p :> Astf.pat))),
                                             (skont :> Astf.exp))),
                                        (`Case
                                           (_loc, (`Any _loc),
                                             (ckont :> Astf.exp)))))) :> 
                              Astf.exp)
                  | Str (_loc,p) ->
                      let rec subst =
                        function
                        | (v : string) ->
                            (function
                             | (e : exp) ->
                                 let _loc = loc_of e in
                                 (match e with
                                  | (`Lid (_loc,x) : Astf.exp) ->
                                      let x = if x = v then strm_n else x in
                                      (`Lid (_loc, x) :> Astf.exp)
                                  | (`Uid (_loc,_) : Astf.exp)
                                    |(`Int (_loc,_) : Astf.exp)
                                    |(`Chr (_loc,_) : Astf.exp)
                                    |(`Str (_loc,_) : Astf.exp)
                                    |(`Field (_loc,_,_) : Astf.exp) -> e
                                  | (`LetIn (_loc,rf,bi,e) : Astf.exp) ->
                                      (`LetIn
                                         (_loc, (rf :> Astf.flag),
                                           (subst_bind v bi :> Astf.bind),
                                           (subst v e :> Astf.exp)) :> 
                                      Astf.exp)
                                  | (`App (_loc,e1,e2) : Astf.exp) ->
                                      (`App
                                         (_loc, (subst v e1 :> Astf.exp),
                                           (subst v e2 :> Astf.exp)) :> 
                                      Astf.exp)
                                  | (`Par (_loc,e) : Astf.exp) ->
                                      (`Par (_loc, (subst v e)) :> Astf.exp)
                                  | (`Com (_loc,e1,e2) : Astf.exp) ->
                                      (`Com
                                         (_loc, (subst v e1 :> Astf.exp),
                                           (subst v e2 :> Astf.exp)) :> 
                                      Astf.exp)
                                  | _ -> raise Not_found))
                      and subst_bind =
                        function
                        | v ->
                            (function
                             | (`And (_loc,b1,b2) : Astf.bind) ->
                                 (`And
                                    (_loc, (subst_bind v b1 :> Astf.bind),
                                      (subst_bind v b2 :> Astf.bind)) :> 
                                 Astf.bind)
                             | (`Bind (_loc,`Lid (_,v'),e) : Astf.bind) ->
                                 (`Bind
                                    (_loc, (`Lid (_loc, v')),
                                      (if v = v' then e else subst v e :> 
                                      Astf.exp)) :> Astf.bind)
                             | _ -> raise Not_found) in
                      (try
                         match p with
                         | (`Lid (_loc,v) : Astf.pat) -> subst v skont
                         | _ -> raise Not_found
                       with
                       | Not_found  ->
                           (`LetIn
                              (_loc, (`Negative _loc),
                                (`Bind
                                   (_loc, (p :> Astf.pat),
                                     (`Lid (_loc, strm_n)))),
                                (skont :> Astf.exp)) :> Astf.exp))) : 
                exp)))
let rec stream_pattern =
  function
  | _loc ->
      (function
       | (x,epo,e) ->
           (function
            | (ekont : exp option -> exp) ->
                (match x with
                 | [] ->
                     (match epo with
                      | Some ep ->
                          (`LetIn
                             (_loc, (`Negative _loc),
                               (`Bind
                                  (_loc, (ep :> Astf.pat),
                                    (`App
                                       (_loc,
                                         (`Dot
                                            (_loc, (`Uid (_loc, (gm ()))),
                                              (`Lid (_loc, "count")))),
                                         (`Lid (_loc, strm_n)))))),
                               (e :> Astf.exp)) :> Astf.exp)
                      | _ -> e)
                 | (spc,err)::spcl ->
                     let skont =
                       let ekont0 =
                         function
                         | err ->
                             let str =
                               match err with
                               | Some estr -> estr
                               | _ -> (`Str (_loc, "") :> Astf.exp) in
                             (`App
                                (_loc, (`Lid (_loc, "raise")),
                                  (`App
                                     (_loc,
                                       (`Dot
                                          (_loc, (`Uid (_loc, (gm ()))),
                                            (`Uid (_loc, "Error")))),
                                       (str :> Astf.exp)))) :> Astf.exp) in
                       stream_pattern _loc (spcl, epo, e) ekont0 in
                     let ckont = ekont err in
                     stream_pattern_component skont ckont spc)))
let rec group_terms =
  function
  | (xs : stream_cases) ->
      (match xs with
       | ((When (_loc,p,w),None )::spcl,epo,e)::spel ->
           let (tspel,spel) = group_terms spel in
           (((p, w, _loc, spcl, epo, e) :: tspel), spel)
       | spel -> ([], spel))
let stream_patterns_term =
  function
  | _loc ->
      (function
       | (ekont : unit -> exp) ->
           (function
            | tspel ->
                (let pel =
                   List.fold_right
                     (function
                      | (p,w,_loc,spcl,epo,e) ->
                          (function
                           | acc ->
                               let p =
                                 (`App
                                    (_loc, (`Uid (_loc, "Some")),
                                      (p :> Astf.pat)) :> Astf.pat) in
                               let e =
                                 let ekont =
                                   function
                                   | err ->
                                       let str =
                                         match err with
                                         | Some estr -> estr
                                         | _ -> (`Str (_loc, "") :> Astf.exp) in
                                       (`App
                                          (_loc, (`Lid (_loc, "raise")),
                                            (`App
                                               (_loc,
                                                 (`Dot
                                                    (_loc,
                                                      (`Uid (_loc, (gm ()))),
                                                      (`Uid (_loc, "Error")))),
                                                 (str :> Astf.exp)))) :> 
                                         Astf.exp) in
                                 let skont =
                                   stream_pattern _loc (spcl, epo, e) ekont in
                                 (`Seq
                                    (_loc,
                                      (`Sem
                                         (_loc,
                                           (`App
                                              (_loc,
                                                (junk_fun _loc :> Astf.exp),
                                                (`Lid (_loc, strm_n)))),
                                           (skont :> Astf.exp)))) :> 
                                   Astf.exp) in
                               (match w with
                                | Some w ->
                                    (`Bar
                                       (_loc,
                                         (`CaseWhen
                                            (_loc, (p :> Astf.pat),
                                              (w :> Astf.exp),
                                              (e :> Astf.exp))), acc) :> 
                                    Astf.case)
                                | None  ->
                                    (`Bar
                                       (_loc,
                                         (`Case
                                            (_loc, (p :> Astf.pat),
                                              (e :> Astf.exp))), acc) :> 
                                    Astf.case)))) tspel
                     (`Case (_loc, (`Any _loc), (ekont () :> Astf.exp)) :> 
                     Astf.case) in
                 (`Match
                    (_loc,
                      (`App
                         (_loc, (peek_fun _loc :> Astf.exp),
                           (`Lid (_loc, strm_n)))), pel) :> Astf.exp) : 
                exp)))
let rec parser_cases =
  function
  | _loc ->
      (function
       | (x : stream_cases) ->
           (match x with
            | [] ->
                (`App
                   (_loc, (`Lid (_loc, "raise")),
                     (`Dot
                        (_loc, (`Uid (_loc, (gm ()))),
                          (`Uid (_loc, "NotConsumed"))))) :> Astf.exp)
            | spel ->
                (match group_terms spel with
                 | ([],x::spel) ->
                     stream_pattern _loc x
                       (function | _ -> parser_cases _loc spel)
                 | (tspel,spel) ->
                     stream_patterns_term _loc
                       (function | _ -> parser_cases _loc spel) tspel)))
let cparser =
  function
  | _loc ->
      (function
       | pc ->
           let e = parser_cases _loc pc in
           let p =
             (`Constraint
                (_loc, (`Lid (_loc, strm_n)),
                  (`App
                     (_loc,
                       (`Dot
                          (_loc, (`Uid (_loc, (gm ()))), (`Lid (_loc, "t")))),
                       (`Any _loc)))) :> Astf.pat) in
           (`Fun (_loc, (`Case (_loc, (p :> Astf.pat), (e :> Astf.exp)))) :> 
             Astf.exp))
type sexp_comp =
  | Trm of loc* exp
  | Ntr of loc* exp
let not_computing =
  function
  | x ->
      let rec aux =
        function
        | x ->
            (match x with
             | #literal -> true
             | #vid' -> true
             | (`App (_loc,x,y) : Astf.exp) ->
                 (is_cons_apply_not_computing x) && (aux y)
             | _ -> false)
      and is_cons_apply_not_computing =
        function
        | (`Uid (_loc,_) : Astf.exp) -> true
        | (`Lid (_loc,_) : Astf.exp) -> false
        | (`App (_loc,x,y) : Astf.exp) ->
            (is_cons_apply_not_computing x) && (aux y)
        | _ -> false in
      aux x
let slazy =
  function
  | _loc ->
      (function
       | e ->
           (match e with
            | (`App (_loc,f,`Unit _) : Astf.exp) ->
                (match f with
                 | (`Lid (_loc,_) : Astf.exp) -> f
                 | _ ->
                     (`Fun
                        (_loc, (`Case (_loc, (`Any _loc), (e :> Astf.exp)))) :> 
                     Astf.exp))
            | _ ->
                (`Fun (_loc, (`Case (_loc, (`Any _loc), (e :> Astf.exp)))) :> 
                Astf.exp)))
let rec cstream =
  function
  | gloc ->
      (function
       | [] ->
           let _loc = gloc in
           (`Dot (_loc, (`Uid (_loc, (gm ()))), (`Lid (_loc, "sempty"))) :> 
             Astf.exp)
       | (Trm (_loc,e))::[] ->
           if not_computing e
           then
             (`App
                (_loc,
                  (`Dot
                     (_loc, (`Uid (_loc, (gm ()))), (`Lid (_loc, "ising")))),
                  (e :> Astf.exp)) :> Astf.exp)
           else
             (`App
                (_loc,
                  (`Dot
                     (_loc, (`Uid (_loc, (gm ()))), (`Lid (_loc, "lsing")))),
                  (slazy _loc e :> Astf.exp)) :> Astf.exp)
       | (Trm (_loc,e))::secl ->
           if not_computing e
           then
             (`App
                (_loc,
                  (`App
                     (_loc,
                       (`Dot
                          (_loc, (`Uid (_loc, (gm ()))),
                            (`Lid (_loc, "icons")))), (e :> Astf.exp))),
                  (cstream gloc secl :> Astf.exp)) :> Astf.exp)
           else
             (`App
                (_loc,
                  (`App
                     (_loc,
                       (`Dot
                          (_loc, (`Uid (_loc, (gm ()))),
                            (`Lid (_loc, "lcons")))),
                       (slazy _loc e :> Astf.exp))),
                  (cstream gloc secl :> Astf.exp)) :> Astf.exp)
       | (Ntr (_loc,e))::[] ->
           if not_computing e
           then e
           else
             (`App
                (_loc,
                  (`Dot
                     (_loc, (`Uid (_loc, (gm ()))), (`Lid (_loc, "slazy")))),
                  (slazy _loc e :> Astf.exp)) :> Astf.exp)
       | (Ntr (_loc,e))::secl ->
           if not_computing e
           then
             (`App
                (_loc,
                  (`App
                     (_loc,
                       (`Dot
                          (_loc, (`Uid (_loc, (gm ()))),
                            (`Lid (_loc, "iapp")))), (e :> Astf.exp))),
                  (cstream gloc secl :> Astf.exp)) :> Astf.exp)
           else
             (`App
                (_loc,
                  (`App
                     (_loc,
                       (`Dot
                          (_loc, (`Uid (_loc, (gm ()))),
                            (`Lid (_loc, "lapp")))),
                       (slazy _loc e :> Astf.exp))),
                  (cstream gloc secl :> Astf.exp)) :> Astf.exp))
