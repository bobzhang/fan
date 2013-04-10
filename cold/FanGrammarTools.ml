open Ast

open FanOps

open Format

open AstLoc

open LibUtil

open FanGrammar

let print_warning = eprintf "%a:\n%s@." FanLoc.print

let prefix = "__fan_"

let ghost = FanLoc.ghost

let grammar_module_name = ref (`Uid (ghost, "Gram"))

let gm () =
  (match FanConfig.compilation_unit.contents with
   | Some "Gram" -> `Uid (ghost, "")
   | Some _|None  -> grammar_module_name.contents : vid )

let mk_entry ~name  ~pos  ~levels  = { name; pos; levels }

let mk_level ~label  ~assoc  ~rules  = { label; assoc; rules }

let mk_rule ~prod  ~action  = { prod; action }

let mk_symbol ?(pattern= None)  ~text  ~styp  = { text; styp; pattern }

let string_of_pat pat =
  let buf = Buffer.create 42 in
  let () =
    Format.bprintf buf "%a@?"
      (fun fmt  p  -> AstPrint.pattern fmt (Ast2pt.pat p)) pat in
  let str = Buffer.contents buf in if str = "" then assert false else str

let check_not_tok s =
  match s with
  | { text = `Stok (_loc,_,_,_);_} ->
      FanLoc.raise _loc
        (XStream.Error
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
      (function
       | { prod = ({ pattern = None ; styp = `Tok _;_} as s)::[];
           action = None  } ->
           {
             prod = [{ s with pattern = (Some (`Lid (_loc, "x"))) }];
             action =
               (Some
                  (`App
                     (_loc,
                       (`Field
                          (_loc, (gm () : vid  :>exp),
                            (`Lid (_loc, "string_of_token")))),
                       (`Lid (_loc, "x"))) : Ast.exp ))
           }
       | { prod = ({ pattern = None ;_} as s)::[]; action = None  } ->
           {
             prod = [{ s with pattern = (Some (`Lid (_loc, "x"))) }];
             action = (Some (`Lid (_loc, "x") : Ast.exp ))
           }
       | { prod = []; action = Some _ } as r -> r
       | _ -> raise Exit) rl
  with | Exit  -> rl

let make_ctyp (styp : styp) tvar =
  (let rec aux =
     function
     | `Id _|`Quote _ as x -> x
     | `App (_loc,t1,t2) -> `App (_loc, (aux t1), (aux t2))
     | `Self (_loc,x) ->
         if tvar = ""
         then
           FanLoc.raise _loc
             (XStream.Error
                ("'" ^ (x ^ "' illegal in anonymous entry level")))
         else `Quote (_loc, (`Normal _loc), (`Lid (_loc, tvar)))
     | `Tok _loc ->
         `PolySup
           (_loc,
             (`Ctyp
                (_loc,
                  (`Id
                     (_loc,
                       (`Dot
                          (_loc, (`Uid (_loc, "FanToken")),
                            (`Lid (_loc, "t")))))))))
     | `Type t -> t in
   aux styp : ctyp )

let rec make_exp (tvar : string) (x : text) =
  let rec aux tvar x =
    match x with
    | `Smeta (_loc,n,tl,e,t) ->
        let el = list_of_list _loc (List.map (fun t  -> aux "" t) tl) in
        let ns =
          list_of_list _loc
            (List.map (fun n  -> (`Str (_loc, n) : Ast.exp )) n) in
        let act = typing e (make_ctyp t tvar) in
        (`App
           (_loc, (`Vrn (_loc, "Smeta")),
             (`Par
                (_loc,
                  (`Com
                     (_loc, ns,
                       (`Com
                          (_loc, el,
                            (`App
                               (_loc,
                                 (`Field
                                    (_loc, (gm () : vid  :>exp),
                                      (`Dot
                                         (_loc, (`Uid (_loc, "Action")),
                                           (`Lid (_loc, "mk")))))), act))))))))) : 
          Ast.exp )
    | `Slist (_loc,min,t,ts) ->
        let txt = aux "" t.text in
        (match ts with
         | None  ->
             if min
             then (`App (_loc, (`Vrn (_loc, "Slist1")), txt) : Ast.exp )
             else (`App (_loc, (`Vrn (_loc, "Slist0")), txt) : Ast.exp )
         | Some s ->
             let x = aux tvar s.text in
             if min
             then
               (`App
                  (_loc, (`Vrn (_loc, "Slist1sep")),
                    (`Par (_loc, (`Com (_loc, txt, x))))) : Ast.exp )
             else
               (`App
                  (_loc, (`Vrn (_loc, "Slist0sep")),
                    (`Par (_loc, (`Com (_loc, txt, x))))) : Ast.exp ))
    | `Snext _loc -> (`Vrn (_loc, "Snext") : Ast.exp )
    | `Sself _loc -> (`Vrn (_loc, "Sself") : Ast.exp )
    | `Skeyword (_loc,kwd) ->
        (`App (_loc, (`Vrn (_loc, "Skeyword")), (`Str (_loc, kwd))) : 
        Ast.exp )
    | `Snterm (_loc,n,lev) ->
        let obj: Ast.exp =
          `App
            (_loc,
              (`Field (_loc, (gm () : vid  :>exp), (`Lid (_loc, "obj")))),
              (`Constraint
                 (_loc, (n.exp),
                   (`App
                      (_loc,
                        (`Id
                           (_loc,
                             (`Dot
                                (_loc, (gm () : vid  :>ident),
                                  (`Lid (_loc, "t")))))),
                        (`Quote
                           (_loc, (`Normal _loc), (`Lid (_loc, (n.tvar)))))))))) in
        (match lev with
         | Some lab ->
             (`App
                (_loc, (`Vrn (_loc, "Snterml")),
                  (`Par (_loc, (`Com (_loc, obj, (`Str (_loc, lab))))))) : 
             Ast.exp )
         | None  ->
             if n.tvar = tvar
             then (`Vrn (_loc, "Sself") : Ast.exp )
             else (`App (_loc, (`Vrn (_loc, "Snterm")), obj) : Ast.exp ))
    | `Sopt (_loc,t) ->
        (`App (_loc, (`Vrn (_loc, "Sopt")), (aux "" t)) : Ast.exp )
    | `Stry (_loc,t) ->
        (`App (_loc, (`Vrn (_loc, "Stry")), (aux "" t)) : Ast.exp )
    | `Speek (_loc,t) ->
        (`App (_loc, (`Vrn (_loc, "Speek")), (aux "" t)) : Ast.exp )
    | `Srules (_loc,rl) ->
        (`App
           (_loc,
             (`Field (_loc, (gm () : vid  :>exp), (`Lid (_loc, "srules")))),
             (make_exp_rules _loc rl "")) : Ast.exp )
    | `Stok (_loc,match_fun,attr,descr) ->
        (`App
           (_loc, (`Vrn (_loc, "Stoken")),
             (`Par
                (_loc,
                  (`Com
                     (_loc, match_fun,
                       (`Par
                          (_loc,
                            (`Com
                               (_loc, (`Vrn (_loc, attr)),
                                 (`Str (_loc, (String.escaped descr)))))))))))) : 
        Ast.exp ) in
  aux tvar x
and make_exp_rules (_loc : loc) (rl : (text list * exp) list) (tvar : string)
  =
  list_of_list _loc
    (List.map
       (fun (sl,action)  ->
          let action_string = Ast2pt.to_string_exp action in
          let sl =
            list_of_list _loc (List.map (fun t  -> make_exp tvar t) sl) in
          (`Par
             (_loc,
               (`Com
                  (_loc, sl,
                    (`Par
                       (_loc,
                         (`Com (_loc, (`Str (_loc, action_string)), action))))))) : 
            Ast.exp )) rl)

let text_of_action (_loc : loc) (psl : symbol list)
  ?action:(act : exp option)  (rtvar : string) (tvar : string) =
  (let locid = `Lid (_loc, (FanLoc.name.contents)) in
   let act =
     match act with
     | Some act -> act
     | None  -> (`Id (_loc, (`Uid (_loc, "()"))) : Ast.exp ) in
   let (_,tok_match_pl) =
     List.fold_lefti
       (fun i  ((oe,op) as ep)  x  ->
          match x with
          | { pattern = Some p; text = `Stok _;_} ->
              let id = prefix ^ (string_of_int i) in
              (((`Lid (_loc, id) : Ast.exp ) :: oe), (p :: op))
          | _ -> ep) ([], []) psl in
   let e =
     let e1: Ast.exp =
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
                        (`Id
                           (_loc,
                             (`Dot
                                (_loc, (`Uid (_loc, "FanLoc")),
                                  (`Lid (_loc, "t")))))))), e1))) : Ast.exp )
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
                        (`Id
                           (_loc,
                             (`Dot
                                (_loc, (`Uid (_loc, "FanLoc")),
                                  (`Lid (_loc, "t")))))))),
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
           Ast.exp ) in
   let (_,txt) =
     List.fold_lefti
       (fun i  txt  s  ->
          match s.pattern with
          | Some (`Alias (_loc,`App (_,_,`Par (_,`Any _)),p) : Ast.pat) ->
              let p = typing (p : alident  :>pat) (make_ctyp s.styp tvar) in
              (`Fun (_loc, (`Case (_loc, p, txt))) : Ast.exp )
          | Some p when is_irrefut_pat p ->
              let p = typing p (make_ctyp s.styp tvar) in
              (`Fun (_loc, (`Case (_loc, p, txt))) : Ast.exp )
          | None  ->
              (`Fun (_loc, (`Case (_loc, (`Any _loc), txt))) : Ast.exp )
          | Some _ ->
              let p =
                typing (`Lid (_loc, (prefix ^ (string_of_int i))))
                  (make_ctyp s.styp tvar) in
              (`Fun (_loc, (`Case (_loc, p, txt))) : Ast.exp )) e psl in
   (`App
      (_loc,
        (`Field (_loc, (gm () : vid  :>exp), (`Lid (_loc, "mk_action")))),
        txt) : Ast.exp ) : exp )

let mk_srule loc (t : string) (tvar : string) (r : rule) =
  (let sl = List.map (fun s  -> s.text) r.prod in
   let ac = text_of_action loc r.prod t ?action:(r.action) tvar in (sl, ac) : 
  (text list * exp) )

let mk_srules loc (t : string) (rl : rule list) (tvar : string) =
  (List.map (mk_srule loc t tvar) rl : (text list * exp) list )

let exp_delete_rule _loc n (symbolss : symbol list list) =
  let f _loc n sl =
    let sl = list_of_list _loc (List.map (fun s  -> make_exp "" s.text) sl) in
    ((n.exp), sl) in
  let rest =
    List.map
      (fun sl  ->
         let (e,b) = f _loc n sl in
         (`App
            (_loc,
              (`App
                 (_loc,
                   (`Field
                      (_loc, (gm () : vid  :>exp),
                        (`Lid (_loc, "delete_rule")))), e)), b) : Ast.exp ))
      symbolss in
  match symbolss with
  | [] -> `Id (_loc, (`Uid (_loc, "()")))
  | _ -> seq_sem rest

let mk_name _loc (i : vid) =
  { exp = (i : vid  :>exp); tvar = (Id.tvar_of_ident i); loc = _loc }

let mk_slist loc min sep symb = `Slist (loc, min, symb, sep)

let text_of_entry (e : entry) =
  (let _loc = (e.name).loc in
   let ent =
     let x = e.name in
     (`Constraint
        (_loc, (x.exp),
          (`App
             (_loc,
               (`Id
                  (_loc,
                    (`Dot (_loc, (gm () : vid  :>ident), (`Lid (_loc, "t")))))),
               (`Quote (_loc, (`Normal _loc), (`Lid (_loc, (x.tvar)))))))) : 
       Ast.exp ) in
   let pos =
     match e.pos with
     | Some pos -> (`App (_loc, (`Uid (_loc, "Some")), pos) : Ast.exp )
     | None  -> (`Uid (_loc, "None") : Ast.exp ) in
   let apply level =
     let lab =
       match level.label with
       | Some lab ->
           (`App (_loc, (`Uid (_loc, "Some")), (`Str (_loc, lab))) : 
           Ast.exp )
       | None  -> (`Uid (_loc, "None") : Ast.exp ) in
     let ass =
       match level.assoc with
       | Some ass -> (`App (_loc, (`Uid (_loc, "Some")), ass) : Ast.exp )
       | None  -> (`Uid (_loc, "None") : Ast.exp ) in
     let rl = mk_srules _loc (e.name).tvar level.rules (e.name).tvar in
     let prod = make_exp_rules _loc rl (e.name).tvar in
     (`Par (_loc, (`Com (_loc, lab, (`Com (_loc, ass, prod))))) : Ast.exp ) in
   match e.levels with
   | `Single l ->
       (`App
          (_loc,
            (`App
               (_loc,
                 (`Field
                    (_loc, (gm () : vid  :>exp),
                      (`Lid (_loc, "extend_single")))), ent)),
            (`Par (_loc, (`Com (_loc, pos, (apply l)))))) : Ast.exp )
   | `Group ls ->
       let txt = list_of_list _loc (List.map apply ls) in
       (`App
          (_loc,
            (`App
               (_loc,
                 (`Field
                    (_loc, (gm () : vid  :>exp), (`Lid (_loc, "extend")))),
                 ent)), (`Par (_loc, (`Com (_loc, pos, txt))))) : Ast.exp ) : 
  exp )

let let_in_of_extend _loc (gram : vid option) locals default =
  let entry_mk =
    match gram with
    | Some g ->
        let g = (g : vid  :>exp) in
        (`App
           (_loc,
             (`Field
                (_loc, (gm () : vid  :>exp), (`Lid (_loc, "mk_dynamic")))),
             g) : Ast.exp )
    | None  ->
        (`Field (_loc, (gm () : vid  :>exp), (`Lid (_loc, "mk"))) : Ast.exp ) in
  let local_binding_of_name =
    function
    | { exp = (`Lid (_,i)|`Id (_,`Lid (_,i))|`Lid (_,i)); tvar = x;
        loc = _loc } ->
        (`Bind
           (_loc, (`Lid (_loc, i)),
             (`Constraint
                (_loc,
                  (`App
                     (_loc, (`Lid (_loc, "grammar_entry_create")),
                       (`Str (_loc, i)))),
                  (`App
                     (_loc,
                       (`Id
                          (_loc,
                            (`Dot
                               (_loc, (gm () : vid  :>ident),
                                 (`Lid (_loc, "t")))))),
                       (`Quote (_loc, (`Normal _loc), (`Lid (_loc, x))))))))) : 
        Ast.binding )
    | { exp;_} ->
        failwithf "internal error in the Grammar extension %s"
          (Objs.dump_exp exp) in
  match locals with
  | None |Some [] -> default
  | Some ll ->
      let locals = and_of_list (List.map local_binding_of_name ll) in
      (`LetIn
         (_loc, (`ReNil _loc),
           (`Bind (_loc, (`Lid (_loc, "grammar_entry_create")), entry_mk)),
           (`LetIn (_loc, (`ReNil _loc), locals, default))) : Ast.exp )

let text_of_functorial_extend _loc gram locals el =
  let args =
    let el = List.map text_of_entry el in
    match el with | [] -> `Id (_loc, (`Uid (_loc, "()"))) | _ -> seq_sem el in
  let_in_of_extend _loc gram locals args

let mk_tok _loc ?restrict  ~pattern  styp =
  match restrict with
  | None  ->
      let no_variable = Objs.wildcarder#pat pattern in
      let match_fun =
        if is_irrefut_pat no_variable
        then `Fun (_loc, (`Case (_loc, no_variable, (`Lid (_loc, "true")))))
        else
          `Fun
            (_loc,
              (`Bar
                 (_loc, (`Case (_loc, no_variable, (`Lid (_loc, "true")))),
                   (`Case (_loc, (`Any _loc), (`Lid (_loc, "false"))))))) in
      let descr = string_of_pat no_variable in
      let text = `Stok (_loc, match_fun, "Normal", descr) in
      { text; styp; pattern = (Some pattern) }
  | Some restrict ->
      let p' = Objs.wildcarder#pat pattern in
      let match_fun =
        `Fun
          (_loc,
            (`Bar
               (_loc,
                 (`CaseWhen (_loc, pattern, restrict, (`Lid (_loc, "true")))),
                 (`Case (_loc, (`Any _loc), (`Lid (_loc, "false"))))))) in
      let descr = string_of_pat pattern in
      let text = `Stok (_loc, match_fun, "Antiquot", descr) in
      { text; styp; pattern = (Some p') }

let sfold ?sep  _loc (ns : string list) f e s =
  let fs = [("FOLD0", "sfold0"); ("FOLD1", "sfold1")] in
  let suffix = match sep with | None  -> "" | Some _ -> "sep" in
  let n = List.hd ns in
  let foldfun =
    try (List.assoc n fs) ^ suffix with | Not_found  -> invalid_arg "sfold" in
  let styp = `Quote (_loc, (`Normal _loc), (`Lid (_loc, (new_type_var ())))) in
  let e: Ast.exp =
    `App
      (_loc,
        (`App
           (_loc,
             (`Field (_loc, (gm () : vid  :>exp), (`Lid (_loc, foldfun)))),
             f)), e) in
  let (t :styp)=
    `App
      (_loc,
        (`App
           (_loc,
             (`Type
                (`App
                   (_loc,
                     (`Id
                        (_loc,
                          (`Dot
                             (_loc, (gm () : vid  :>ident),
                               (`Lid (_loc, ("fold" ^ suffix))))))),
                     (`Any _loc)))), (s.styp))), styp) in
  let text =
    `Smeta
      (_loc, ns,
        (match sep with | None  -> [s.text] | Some sep -> [s.text; sep.text]),
        e, t) in
  { text; styp; pattern = None }