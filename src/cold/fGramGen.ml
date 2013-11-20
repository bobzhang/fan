let eprintf = Format.eprintf
let list_of_list = FanOps.list_of_list
let is_irrefut_pat = FanOps.is_irrefut_pat
open Astf
open Ast_gen
open Util
let print_warning = eprintf "%a:\n%s@." Locf.print
let prefix = "__fan_"
let ghost = Locf.ghost
let grammar_module_name = ref (`Uid (ghost, "Gramf"))
let gm () =
  match FConfig.compilation_unit.contents with
  | Some "Gramf" -> `Uid (ghost, "")
  | Some _|None  -> grammar_module_name.contents
let mk_entry ~local  ~name  ~pos  ~levels  =
  { Gram_def.name = name; pos; levels; local }
let mk_level ~label  ~assoc  ~rules  =
  { Gram_def.label = label; assoc; rules }
let mk_rule ~prod  ~action  = ({ prod; action } : Gram_def.rule )
let mk_symbol ?(pattern= None)  ~text  ~styp  =
  ({ text; styp; pattern } : Gram_def.symbol )
let check_not_tok s =
  match (s : Gram_def.symbol ) with
  | { text = `Stok (_loc,_,_);_} ->
      Locf.raise _loc
        (Fstream.Error
           ("Deprecated syntax, use a sub rule. " ^
              "L0 STRING becomes L0 [ x = STRING -> x ]"))
  | _ -> ()
let new_type_var =
  let i = ref 0 in fun ()  -> incr i; "e__" ^ (string_of_int i.contents)
let gensym = let i = ref 0 in fun ()  -> incr i; i
let gen_lid () = prefix ^ (string_of_int (gensym ()).contents)
let retype_rule_list_without_patterns _loc rl =
  try
    List.map
      (fun (x : Gram_def.rule)  ->
         match x with
         | { prod = ({ pattern = None ; styp = `Tok _;_} as s)::[];
             action = None  } ->
             ({
                prod =
                  [{ s with pattern = (Some (`Lid (_loc, "x") : Astf.pat )) }];
                action =
                  (Some
                     (`App
                        (_loc,
                          (`Dot
                             (_loc, (gm ()),
                               (`Lid (_loc, "string_of_token")))),
                          (`Lid (_loc, "x"))) : Astf.exp ))
              } : Gram_def.rule )
         | { prod = ({ pattern = None ;_} as s)::[]; action = None  } ->
             ({
                prod =
                  [{ s with pattern = (Some (`Lid (_loc, "x") : Astf.pat )) }];
                action = (Some (`Lid (_loc, "x") : Astf.exp ))
              } : Gram_def.rule )
         | { prod = []; action = Some _ } as r -> r
         | _ -> raise Exit) rl
  with | Exit  -> rl
let make_ctyp (styp : Gram_def.styp) tvar =
  (let rec aux v =
     match (v : Gram_def.styp ) with
     | #vid' as x -> (x : vid'  :>ctyp)
     | `Quote _ as x -> x
     | `App (_loc,t1,t2) -> (`App (_loc, (aux t1), (aux t2)) : Astf.ctyp )
     | `Self _loc ->
         if tvar = ""
         then
           Locf.raise _loc
             (Fstream.Error "S: illegal in anonymous entry level")
         else
           (`Quote (_loc, (`Normal _loc), (`Lid (_loc, tvar))) : Astf.ctyp )
     | `Tok _loc ->
         (`PolySup
            (_loc,
              (`Ctyp
                 (_loc,
                   (`Dot (_loc, (`Uid (_loc, "Tokenf")), (`Lid (_loc, "t"))))))) : 
         Astf.ctyp )
     | `Type t -> t in
   aux styp : ctyp )
let rec make_exp (tvar : string) (x : Gram_def.text) =
  let rec aux tvar (x : Gram_def.text) =
    match x with
    | `Slist (_loc,min,t,ts) ->
        let txt = aux "" t.text in
        (match ts with
         | None  ->
             if min
             then (`App (_loc, (`Vrn (_loc, "Slist1")), txt) : Astf.exp )
             else (`App (_loc, (`Vrn (_loc, "Slist0")), txt) : Astf.exp )
         | Some s ->
             let x = aux tvar s.text in
             if min
             then
               (`App
                  (_loc, (`Vrn (_loc, "Slist1sep")),
                    (`Par (_loc, (`Com (_loc, txt, x))))) : Astf.exp )
             else
               (`App
                  (_loc, (`Vrn (_loc, "Slist0sep")),
                    (`Par (_loc, (`Com (_loc, txt, x))))) : Astf.exp ))
    | `Sself _loc -> (`Vrn (_loc, "Sself") : Astf.exp )
    | `Skeyword (_loc,kwd) ->
        (`App (_loc, (`Vrn (_loc, "Skeyword")), (`Str (_loc, kwd))) : 
        Astf.exp )
    | `Snterm (_loc,n,lev) ->
        let obj: Astf.exp =
          `App
            (_loc, (`Dot (_loc, (gm ()), (`Lid (_loc, "obj")))),
              (`Constraint
                 (_loc, (n.exp),
                   (`App
                      (_loc,
                        (`Dot
                           (_loc, (gm () : vid  :>ident), (`Lid (_loc, "t")))),
                        (`Quote
                           (_loc, (`Normal _loc), (`Lid (_loc, (n.tvar)))))))))) in
        (match lev with
         | Some lab ->
             (`App
                (_loc, (`Vrn (_loc, "Snterml")),
                  (`Par (_loc, (`Com (_loc, obj, (`Str (_loc, lab))))))) : 
             Astf.exp )
         | None  ->
             if n.tvar = tvar
             then (`Vrn (_loc, "Sself") : Astf.exp )
             else (`App (_loc, (`Vrn (_loc, "Snterm")), obj) : Astf.exp ))
    | `Sopt (_loc,t) ->
        (`App (_loc, (`Vrn (_loc, "Sopt")), (aux "" t)) : Astf.exp )
    | `Stry (_loc,t) ->
        (`App (_loc, (`Vrn (_loc, "Stry")), (aux "" t)) : Astf.exp )
    | `Speek (_loc,t) ->
        (`App (_loc, (`Vrn (_loc, "Speek")), (aux "" t)) : Astf.exp )
    | `Stok (_loc,match_fun,descr) ->
        let v =
          object 
            inherit  FanAstN.meta
            method! ant _loc x =
              match x with
              | `Ant (_loc,{ FanUtil.content = x;_}) ->
                  (`App (_loc, (`Vrn (_loc, "Str")), (`Lid (_loc, x))) : 
                  Astf.ep )
          end in
        let descr' = Objs.strip_pat (descr :>pat) in
        let mdescr = (v#pat _loc descr' :>exp) in
        let mstr = Gram_def.string_of_simple_pat descr in
        (`App
           (_loc, (`Vrn (_loc, "Stoken")),
             (`Par
                (_loc,
                  (`Com
                     (_loc, match_fun,
                       (`Com (_loc, mdescr, (`Str (_loc, mstr))))))))) : 
          Astf.exp ) in
  aux tvar x
and make_exp_rules (_loc : loc)
  (rl : (Gram_def.text list* exp* exp option) list) (tvar : string) =
  list_of_list _loc
    (List.map
       (fun (sl,action,raw)  ->
          let action_string =
            match raw with | None  -> "" | Some e -> Ast2pt.to_string_exp e in
          let sl =
            list_of_list _loc (List.map (fun t  -> make_exp tvar t) sl) in
          (`Par
             (_loc,
               (`Com
                  (_loc, sl,
                    (`Par
                       (_loc,
                         (`Com (_loc, (`Str (_loc, action_string)), action))))))) : 
            Astf.exp )) rl)
let text_of_action (_loc : loc) (psl : Gram_def.symbol list)
  ?action:(act : exp option)  (rtvar : string) (tvar : string) =
  (let locid: Astf.pat = `Lid (_loc, (Locf.name.contents)) in
   let act = Option.default (`Uid (_loc, "()") : Astf.exp ) act in
   let (_,tok_match_pl) =
     Listf.fold_lefti
       (fun i  ((oe,op) as ep)  x  ->
          match (x : Gram_def.symbol ) with
          | { pattern = Some p; text = `Stok _;_} when not (is_irrefut_pat p)
              ->
              let id = prefix ^ (string_of_int i) in
              (((`Lid (_loc, id) : Astf.exp ) :: oe), (p :: op))
          | _ -> ep) ([], []) psl in
   let e =
     let e1: Astf.exp =
       `Constraint
         (_loc, act, (`Quote (_loc, (`Normal _loc), (`Lid (_loc, rtvar))))) in
     match tok_match_pl with
     | ([],_) ->
         (`Fun
            (_loc,
              (`Case
                 (_loc,
                   (`Constraint
                      (_loc, locid,
                        (`Dot
                           (_loc, (`Uid (_loc, "Locf")), (`Lid (_loc, "t")))))),
                   e1))) : Astf.exp )
     | (e,p) ->
         let (exp,pat) =
           match (e, p) with
           | (x::[],y::[]) -> (x, y)
           | _ -> ((tuple_com e), (tuple_com p)) in
         let action_string = Ast2pt.to_string_exp act in
         (`Fun
            (_loc,
              (`Case
                 (_loc,
                   (`Constraint
                      (_loc, locid,
                        (`Dot
                           (_loc, (`Uid (_loc, "Locf")), (`Lid (_loc, "t")))))),
                   (`Match
                      (_loc, exp,
                        (`Bar
                           (_loc, (`Case (_loc, pat, e1)),
                             (`Case
                                (_loc, (`Any _loc),
                                  (`App
                                     (_loc, (`Lid (_loc, "failwith")),
                                       (`Str
                                          (_loc,
                                            (String.escaped action_string)))))))))))))) : 
           Astf.exp ) in
   let (_,txt) =
     Listf.fold_lefti
       (fun i  txt  (s : Gram_def.symbol)  ->
          match s.pattern with
          | Some (`Alias (_loc,`App (_,_,`Par (_,(`Any _ : Astf.pat))),p)) ->
              let p = typing (p : alident  :>pat) (make_ctyp s.styp tvar) in
              (`Fun (_loc, (`Case (_loc, p, txt))) : Astf.exp )
          | Some p when is_irrefut_pat p ->
              let p = typing p (make_ctyp s.styp tvar) in
              (`Fun (_loc, (`Case (_loc, p, txt))) : Astf.exp )
          | Some _ ->
              let p =
                typing
                  (`Lid (_loc, (prefix ^ (string_of_int i))) : Astf.pat )
                  (make_ctyp s.styp tvar) in
              (`Fun (_loc, (`Case (_loc, p, txt))) : Astf.exp )
          | None  ->
              (`Fun (_loc, (`Case (_loc, (`Any _loc), txt))) : Astf.exp )) e
       psl in
   (`App (_loc, (`Dot (_loc, (gm ()), (`Lid (_loc, "mk_action")))), txt) : 
     Astf.exp ) : exp )
let exp_delete_rule _loc n (symbolss : Gram_def.symbol list list) =
  let f _loc (n : Gram_def.name) sl =
    let sl =
      list_of_list _loc
        (List.map (fun (s : Gram_def.symbol)  -> make_exp "" s.text) sl) in
    ((n.exp : Astf.exp ), sl) in
  let rest =
    List.map
      (fun sl  ->
         let (e,b) = f _loc n sl in
         (`App
            (_loc,
              (`App
                 (_loc, (`Dot (_loc, (gm ()), (`Lid (_loc, "delete_rule")))),
                   e)), b) : Astf.exp )) symbolss in
  match symbolss with
  | [] -> (`Uid (_loc, "()") : Astf.exp )
  | _ -> seq_sem rest
let mk_name _loc (i : vid) =
  let rec aux: vid -> string =
    function
    | `Lid (_,x)|`Uid (_,x) -> x
    | `Dot (_,`Uid (_,x),xs) -> x ^ ("__" ^ (aux xs))
    | _ -> failwith "internal error in the Grammar extension" in
  ({ exp = (i :>exp); tvar = (aux i); loc = _loc } : Gram_def.name )
let mk_slist loc min sep symb = `Slist (loc, min, symb, sep)
let text_of_entry ?(safe= true)  (e : Gram_def.entry) =
  (let _loc = (e.name).loc in
   let ent: Astf.exp =
     `Constraint
       (_loc, ((e.name).exp),
         (`App
            (_loc, (`Dot (_loc, (gm () : vid  :>ident), (`Lid (_loc, "t")))),
              (`Quote (_loc, (`Normal _loc), (`Lid (_loc, ((e.name).tvar)))))))) in
   let pos =
     match e.pos with
     | Some pos -> (`App (_loc, (`Uid (_loc, "Some")), pos) : Astf.exp )
     | None  -> (`Uid (_loc, "None") : Astf.exp ) in
   let apply (level : Gram_def.level) =
     let lab =
       match level.label with
       | Some lab ->
           (`App (_loc, (`Uid (_loc, "Some")), (`Str (_loc, lab))) : 
           Astf.exp )
       | None  -> (`Uid (_loc, "None") : Astf.exp ) in
     let ass =
       match level.assoc with
       | Some ass -> (`App (_loc, (`Uid (_loc, "Some")), ass) : Astf.exp )
       | None  -> (`Uid (_loc, "None") : Astf.exp ) in
     let mk_srule loc (t : string) (tvar : string) (r : Gram_def.rule) =
       (let sl = List.map (fun (s : Gram_def.symbol)  -> s.text) r.prod in
        let ac = text_of_action loc r.prod t ?action:(r.action) tvar in
        (sl, ac, (r.action)) : (Gram_def.text list* exp* exp option) ) in
     let mk_srules loc (t : string) (rl : Gram_def.rule list) (tvar : string)
       = List.map (mk_srule loc t tvar) rl in
     let rl = mk_srules _loc (e.name).tvar level.rules (e.name).tvar in
     let prod = make_exp_rules _loc rl (e.name).tvar in
     (`Par (_loc, (`Com (_loc, lab, (`Com (_loc, ass, prod))))) : Astf.exp ) in
   match e.levels with
   | `Single l ->
       if safe
       then
         (`App
            (_loc,
              (`App
                 (_loc,
                   (`Dot (_loc, (gm ()), (`Lid (_loc, "extend_single")))),
                   ent)), (`Par (_loc, (`Com (_loc, pos, (apply l)))))) : 
         Astf.exp )
       else
         (`App
            (_loc,
              (`App
                 (_loc,
                   (`Dot
                      (_loc, (gm ()), (`Lid (_loc, "unsafe_extend_single")))),
                   ent)), (`Par (_loc, (`Com (_loc, pos, (apply l)))))) : 
         Astf.exp )
   | `Group ls ->
       let txt = list_of_list _loc (List.map apply ls) in
       if safe
       then
         (`App
            (_loc,
              (`App
                 (_loc, (`Dot (_loc, (gm ()), (`Lid (_loc, "extend")))), ent)),
              (`Par (_loc, (`Com (_loc, pos, txt))))) : Astf.exp )
       else
         (`App
            (_loc,
              (`App
                 (_loc,
                   (`Dot (_loc, (gm ()), (`Lid (_loc, "unsafe_extend")))),
                   ent)), (`Par (_loc, (`Com (_loc, pos, txt))))) : Astf.exp ) : 
  exp )
let let_in_of_extend _loc (gram : vid option) locals default =
  let entry_mk =
    match gram with
    | Some g ->
        let g = (g : vid  :>exp) in
        (`App (_loc, (`Dot (_loc, (gm ()), (`Lid (_loc, "mk_dynamic")))), g) : 
          Astf.exp )
    | None  -> (`Dot (_loc, (gm ()), (`Lid (_loc, "mk"))) : Astf.exp ) in
  let local_bind_of_name x =
    match (x : Gram_def.name ) with
    | { exp = (`Lid (_,i) : Astf.exp); tvar = x; loc = _loc } ->
        (`Bind
           (_loc, (`Lid (_loc, i)),
             (`Constraint
                (_loc,
                  (`App
                     (_loc, (`Lid (_loc, "grammar_entry_create")),
                       (`Str (_loc, i)))),
                  (`App
                     (_loc,
                       (`Dot
                          (_loc, (gm () : vid  :>ident), (`Lid (_loc, "t")))),
                       (`Quote (_loc, (`Normal _loc), (`Lid (_loc, x))))))))) : 
        Astf.bind )
    | { exp;_} ->
        failwithf "internal error in the Grammar extension %s"
          (Objs.dump_exp exp) in
  match locals with
  | [] -> default
  | ll ->
      let locals = and_of_list (List.map local_bind_of_name ll) in
      (`LetIn
         (_loc, (`Negative _loc),
           (`Bind
              (_loc, (`Lid (_loc, "grammar_entry_create")),
                (`Fun
                   (_loc,
                     (`Case
                        (_loc, (`Lid (_loc, "x")),
                          (`App (_loc, entry_mk, (`Lid (_loc, "x")))))))))),
           (`LetIn (_loc, (`Negative _loc), locals, default))) : Astf.exp )
let capture_antiquot =
  object 
    inherit  Objs.map as super
    val mutable constraints = ([] : (exp* exp) list )
    method! pat =
      function
      | `Ant (_loc,s) ->
          (match s with
           | { FanUtil.content = code;_} ->
               let cons: Astf.exp = `Lid (_loc, code) in
               let code' = "__fan__" ^ code in
               let cons': Astf.exp = `Lid (_loc, code') in
               let () = constraints <- (cons, cons') :: constraints in
               (`Lid (_loc, code') : Astf.pat ))
      | p -> super#pat p
    method get_captured_variables = constraints
    method clear_captured_variables = constraints <- []
  end
let filter_pat_with_captured_variables pat =
  capture_antiquot#clear_captured_variables;
  (let pat = capture_antiquot#pat pat in
   let constraints = capture_antiquot#get_captured_variables in
   (pat, constraints))
let text_of_functorial_extend ?safe  _loc gram el =
  let args =
    let el = List.map (text_of_entry ?safe) el in
    match el with | [] -> (`Uid (_loc, "()") : Astf.exp ) | _ -> seq_sem el in
  let locals =
    Listf.filter_map
      (fun (x : Gram_def.entry)  -> if x.local then Some (x.name) else None)
      el in
  let_in_of_extend _loc gram locals args
let token_of_simple_pat _loc (p : Gram_def.simple_pat) =
  let p_pat = (p : Gram_def.simple_pat  :>pat) in
  let (po,ls) = filter_pat_with_captured_variables p_pat in
  match ls with
  | [] ->
      let no_variable = Gram_def.wildcarder#simple_pat p in
      let match_fun =
        let v = (no_variable :>pat) in
        if is_irrefut_pat v
        then
          (`Fun (_loc, (`Case (_loc, v, (`Lid (_loc, "true"))))) : Astf.exp )
        else
          (`Fun
             (_loc,
               (`Bar
                  (_loc, (`Case (_loc, v, (`Lid (_loc, "true")))),
                    (`Case (_loc, (`Any _loc), (`Lid (_loc, "false"))))))) : 
          Astf.exp ) in
      let descr = no_variable in
      let text = `Stok (_loc, match_fun, descr) in
      ({ text; styp = (`Tok _loc); pattern = (Some p_pat) } : Gram_def.symbol )
  | (x,y)::ys ->
      let guard =
        List.fold_left
          (fun acc  (x,y)  ->
             (`App
                (_loc, (`App (_loc, (`Lid (_loc, "&&")), acc)),
                  (`App (_loc, (`App (_loc, (`Lid (_loc, "=")), x)), y))) : 
             Astf.exp ))
          (`App (_loc, (`App (_loc, (`Lid (_loc, "=")), x)), y) : Astf.exp )
          ys in
      let match_fun: Astf.exp =
        `Fun
          (_loc,
            (`Bar
               (_loc, (`CaseWhen (_loc, po, guard, (`Lid (_loc, "true")))),
                 (`Case (_loc, (`Any _loc), (`Lid (_loc, "false"))))))) in
      let descr = Gram_def.wildcarder#simple_pat p in
      let text = `Stok (_loc, match_fun, descr) in
      { text; styp = (`Tok _loc); pattern = (Some (Objs.wildcarder#pat po)) }
