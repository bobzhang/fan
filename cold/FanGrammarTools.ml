open FanOps
open Format
open Lib
open AstLoc
open LibUtil
module MetaAst = FanAst.Make(Lib.Meta.MetaGhostLoc)
open FanGrammar
let print_warning = eprintf "%a:\n%s@." FanLoc.print
let prefix = "__fan_"
let ghost = FanLoc.ghost
let grammar_module_name = ref (`Uid (ghost, "Gram"))
let gm () =
  match FanConfig.compilation_unit.contents with
  | Some "Gram" -> `Uid (ghost, "")
  | Some _|None  -> grammar_module_name.contents
let mk_entry ~name  ~pos  ~levels  = { name; pos; levels }
let mk_level ~label  ~assoc  ~rules  = { label; assoc; rules }
let mk_rule ~prod  ~action  = { prod; action }
let mk_symbol ?(pattern= None)  ~text  ~styp  = { text; styp; pattern }
let string_of_patt patt =
  let buf = Buffer.create 42 in
  let () =
    Format.bprintf buf "%a@?"
      (fun fmt  p  -> AstPrint.pattern fmt (Ast2pt.patt p)) patt in
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
             prod =
               [{ s with pattern = (Some (`Id (_loc, (`Lid (_loc, "x"))))) }];
             action =
               (Some
                  (`App
                     (_loc,
                       (`Id
                          (_loc,
                            (`Dot
                               (_loc, (gm ()),
                                 (`Lid (_loc, "string_of_token")))))),
                       (`Id (_loc, (`Lid (_loc, "x")))))))
           }
       | { prod = ({ pattern = None ;_} as s)::[]; action = None  } ->
           {
             prod =
               [{ s with pattern = (Some (`Id (_loc, (`Lid (_loc, "x"))))) }];
             action = (Some (`Id (_loc, (`Lid (_loc, "x")))))
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
let rec make_expr (tvar : string) (x : text) =
  let rec aux tvar x =
    match x with
    | `Smeta (_loc,n,tl,e,t) ->
        let el = list_of_list _loc (List.map (fun t  -> aux "" t) tl) in
        let ns = list_of_list _loc (List.map (fun n  -> `Str (_loc, n)) n) in
        let act = typing e (make_ctyp t tvar) in
        `App
          (_loc, (`Vrn (_loc, "Smeta")),
            (`Tup
               (_loc,
                 (`Com
                    (_loc, ns,
                      (`Com
                         (_loc, el,
                           (`App
                              (_loc,
                                (`Id
                                   (_loc,
                                     (`Dot
                                        (_loc, (gm ()),
                                          (`Dot
                                             (_loc, (`Uid (_loc, "Action")),
                                               (`Lid (_loc, "mk")))))))),
                                act)))))))))
    | `Slist (_loc,min,t,ts) ->
        let txt = aux "" t.text in
        (match ts with
         | None  ->
             if min
             then `App (_loc, (`Vrn (_loc, "Slist1")), txt)
             else `App (_loc, (`Vrn (_loc, "Slist0")), txt)
         | Some s ->
             let x = aux tvar s.text in
             if min
             then
               `App
                 (_loc, (`Vrn (_loc, "Slist1sep")),
                   (`Tup (_loc, (`Com (_loc, txt, x)))))
             else
               `App
                 (_loc, (`Vrn (_loc, "Slist0sep")),
                   (`Tup (_loc, (`Com (_loc, txt, x))))))
    | `Snext _loc -> `Vrn (_loc, "Snext")
    | `Sself _loc -> `Vrn (_loc, "Sself")
    | `Skeyword (_loc,kwd) ->
        `App (_loc, (`Vrn (_loc, "Skeyword")), (`Str (_loc, kwd)))
    | `Snterm (_loc,n,lev) ->
        let obj =
          `App
            (_loc,
              (`Id (_loc, (`Dot (_loc, (gm ()), (`Lid (_loc, "obj")))))),
              (`Constraint
                 (_loc, (n.expr),
                   (`App
                      (_loc,
                        (`Id
                           (_loc, (`Dot (_loc, (gm ()), (`Lid (_loc, "t")))))),
                        (`Quote
                           (_loc, (`Normal _loc), (`Lid (_loc, (n.tvar)))))))))) in
        (match lev with
         | Some lab ->
             `App
               (_loc, (`Vrn (_loc, "Snterml")),
                 (`Tup (_loc, (`Com (_loc, obj, (`Str (_loc, lab)))))))
         | None  ->
             if n.tvar = tvar
             then `Vrn (_loc, "Sself")
             else `App (_loc, (`Vrn (_loc, "Snterm")), obj))
    | `Sopt (_loc,t) -> `App (_loc, (`Vrn (_loc, "Sopt")), (aux "" t))
    | `Stry (_loc,t) -> `App (_loc, (`Vrn (_loc, "Stry")), (aux "" t))
    | `Speek (_loc,t) -> `App (_loc, (`Vrn (_loc, "Speek")), (aux "" t))
    | `Srules (_loc,rl) ->
        `App
          (_loc,
            (`Id (_loc, (`Dot (_loc, (gm ()), (`Lid (_loc, "srules")))))),
            (make_expr_rules _loc rl ""))
    | `Stok (_loc,match_fun,attr,descr) ->
        `App
          (_loc, (`Vrn (_loc, "Stoken")),
            (`Tup
               (_loc,
                 (`Com
                    (_loc, match_fun,
                      (`Tup
                         (_loc,
                           (`Com
                              (_loc, (`Vrn (_loc, attr)),
                                (`Str (_loc, (String.escaped descr)))))))))))) in
  aux tvar x
and make_expr_rules (_loc : loc) (rl : (text list* expr) list)
  (tvar : string) =
  (list_of_list _loc
     (List.map
        (fun (sl,action)  ->
           let action_string = Ast2pt.to_string_expr action in
           let sl =
             list_of_list _loc (List.map (fun t  -> make_expr tvar t) sl) in
           `Tup
             (_loc,
               (`Com
                  (_loc, sl,
                    (`Tup
                       (_loc,
                         (`Com (_loc, (`Str (_loc, action_string)), action))))))))
        rl) : expr )
let text_of_action (_loc : loc) (psl : symbol list)
  ?action:(act : expr option)  (rtvar : string) (tvar : string) =
  (let locid = `Id (_loc, (`Lid (_loc, (FanLoc.name.contents)))) in
   let act =
     match act with
     | Some act -> act
     | None  -> `Id (_loc, (`Uid (_loc, "()"))) in
   let (_,tok_match_pl) =
     List.fold_lefti
       (fun i  ((oe,op) as ep)  x  ->
          match x with
          | { pattern = Some p; text = `Stok _;_} ->
              let id = prefix ^ (string_of_int i) in
              (((`Id (_loc, (`Lid (_loc, id)))) :: oe), (p :: op))
          | _ -> ep) ([], []) psl in
   let e =
     let e1 =
       `Constraint
         (_loc, act, (`Quote (_loc, (`Normal _loc), (`Lid (_loc, rtvar))))) in
     match tok_match_pl with
     | ([],_) ->
         `Fun
           (_loc,
             (`Case
                (_loc,
                  (`Constraint
                     (_loc, locid,
                       (`Id
                          (_loc,
                            (`Dot
                               (_loc, (`Uid (_loc, "FanLoc")),
                                 (`Lid (_loc, "t")))))))), e1)))
     | (e,p) ->
         let (expr,patt) =
           match (e, p) with
           | (x::[],y::[]) -> (x, y)
           | _ -> ((tuple_com e), (tuple_com p)) in
         let action_string = Ast2pt.to_string_expr act in
         `Fun
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
                     (_loc, expr,
                       (`Or
                          (_loc, (`Case (_loc, patt,  e1)),
                            (`Case
                               (_loc, (`Any _loc), 
                                 (`App
                                    (_loc,
                                      (`Id (_loc, (`Lid (_loc, "failwith")))),
                                      (`Str
                                         (_loc,
                                           (String.escaped action_string)))))))))))))) in
   let (_,txt) =
     List.fold_lefti
       (fun i  txt  s  ->
          match s.pattern with
          | Some (`Alias (_loc,`App (_,_,`Tup (_,`Any _)),p)) ->
              let p =
                typing (`Id (_loc, (p :>ident))) (make_ctyp s.styp tvar) in
              `Fun (_loc, (`Case (_loc, p,  txt)))
          | Some p when is_irrefut_patt p ->
              let p = typing p (make_ctyp s.styp tvar) in
              `Fun (_loc, (`Case (_loc, p,  txt)))
          | None  ->
              `Fun (_loc, (`Case (_loc, (`Any _loc),  txt)))
          | Some _ ->
              let p =
                typing
                  (`Id (_loc, (`Lid (_loc, (prefix ^ (string_of_int i))))))
                  (make_ctyp s.styp tvar) in
              `Fun (_loc, (`Case (_loc, p,  txt)))) e psl in
   `App
     (_loc, (`Id (_loc, (`Dot (_loc, (gm ()), (`Lid (_loc, "mk_action")))))),
       txt) : expr )
let mk_srule loc (t : string) (tvar : string) (r : rule) =
  (let sl = List.map (fun s  -> s.text) r.prod in
   let ac = text_of_action loc r.prod t ?action:(r.action) tvar in (sl, ac) : 
  (text list* expr) )
let mk_srules loc (t : string) (rl : rule list) (tvar : string) =
  (List.map (mk_srule loc t tvar) rl : (text list* expr) list )
let expr_delete_rule _loc n (symbolss : symbol list list) =
  let f _loc n sl =
    let sl = list_of_list _loc (List.map (fun s  -> make_expr "" s.text) sl) in
    ((n.expr), sl) in
  let rest =
    List.map
      (fun sl  ->
         let (e,b) = f _loc n sl in
         `App
           (_loc,
             (`App
                (_loc,
                  (`Id
                     (_loc,
                       (`Dot (_loc, (gm ()), (`Lid (_loc, "delete_rule")))))),
                  e)), b)) symbolss in
  seq (sem_of_list rest)
let mk_name _loc i =
  { expr = (`Id (_loc, i)); tvar = (Ident.tvar_of_ident i); loc = _loc }
let mk_slist loc min sep symb = `Slist (loc, min, symb, sep)
let text_of_entry (e : entry) =
  (let _loc = (e.name).loc in
   let ent =
     let x = e.name in
     `Constraint
       (_loc, (x.expr),
         (`App
            (_loc, (`Id (_loc, (`Dot (_loc, (gm ()), (`Lid (_loc, "t")))))),
              (`Quote (_loc, (`Normal _loc), (`Lid (_loc, (x.tvar)))))))) in
   let pos =
     match e.pos with
     | Some pos -> `App (_loc, (`Id (_loc, (`Uid (_loc, "Some")))), pos)
     | None  -> `Id (_loc, (`Uid (_loc, "None"))) in
   let apply level =
     let lab =
       match level.label with
       | Some lab ->
           `App
             (_loc, (`Id (_loc, (`Uid (_loc, "Some")))), (`Str (_loc, lab)))
       | None  -> `Id (_loc, (`Uid (_loc, "None"))) in
     let ass =
       match level.assoc with
       | Some ass -> `App (_loc, (`Id (_loc, (`Uid (_loc, "Some")))), ass)
       | None  -> `Id (_loc, (`Uid (_loc, "None"))) in
     let rl = mk_srules _loc (e.name).tvar level.rules (e.name).tvar in
     let prod = make_expr_rules _loc rl (e.name).tvar in
     `Tup (_loc, (`Com (_loc, lab, (`Com (_loc, ass, prod))))) in
   match e.levels with
   | `Single l ->
       `App
         (_loc,
           (`App
              (_loc,
                (`Id
                   (_loc,
                     (`Dot (_loc, (gm ()), (`Lid (_loc, "extend_single")))))),
                ent)), (`Tup (_loc, (`Com (_loc, pos, (apply l))))))
   | `Group ls ->
       let txt = list_of_list _loc (List.map apply ls) in
       `App
         (_loc,
           (`App
              (_loc,
                (`Id (_loc, (`Dot (_loc, (gm ()), (`Lid (_loc, "extend")))))),
                ent)), (`Tup (_loc, (`Com (_loc, pos, txt))))) : expr )
let let_in_of_extend _loc gram locals default =
  let entry_mk =
    match gram with
    | Some g ->
        `App
          (_loc,
            (`Id (_loc, (`Dot (_loc, (gm ()), (`Lid (_loc, "mk_dynamic")))))),
            (`Id (_loc, g)))
    | None  -> `Id (_loc, (`Dot (_loc, (gm ()), (`Lid (_loc, "mk"))))) in
  let local_binding_of_name =
    function
    | { expr = `Id (_,`Lid (_,i)); tvar = x; loc = _loc } ->
        `Bind
          (_loc, (`Id (_loc, (`Lid (_loc, i)))),
            (`Constraint
               (_loc,
                 (`App
                    (_loc,
                      (`Id (_loc, (`Lid (_loc, "grammar_entry_create")))),
                      (`Str (_loc, i)))),
                 (`App
                    (_loc,
                      (`Id (_loc, (`Dot (_loc, (gm ()), (`Lid (_loc, "t")))))),
                      (`Quote (_loc, (`Normal _loc), (`Lid (_loc, x)))))))))
    | _ -> failwith "internal error in the Grammar extension" in
  match locals with
  | None |Some [] -> default
  | Some ll ->
      let locals = and_of_list1 (List.map local_binding_of_name ll) in
      `LetIn
        (_loc, (`ReNil _loc),
          (`Bind
             (_loc, (`Id (_loc, (`Lid (_loc, "grammar_entry_create")))),
               entry_mk)), (`LetIn (_loc, (`ReNil _loc), locals, default)))
let text_of_functorial_extend _loc gram locals el =
  let args =
    let el = List.map text_of_entry el in
    match el with | [] -> `Id (_loc, (`Uid (_loc, "()"))) | _ -> seq_sem el in
  let_in_of_extend _loc gram locals args
let mk_tok _loc ?restrict  ~pattern  styp =
  match restrict with
  | None  ->
      let no_variable = FanObjs.wildcarder#patt pattern in
      let match_fun =
        if is_irrefut_patt no_variable
        then
          `Fun
            (_loc,
              (`Case
                 (_loc, no_variable, 
                   (`Id (_loc, (`Lid (_loc, "true")))))))
        else
          `Fun
            (_loc,
              (`Or
                 (_loc,
                   (`Case
                      (_loc, no_variable, 
                        (`Id (_loc, (`Lid (_loc, "true")))))),
                   (`Case
                      (_loc, (`Any _loc), 
                        (`Id (_loc, (`Lid (_loc, "false"))))))))) in
      let descr = string_of_patt no_variable in
      let text = `Stok (_loc, match_fun, "Normal", descr) in
      { text; styp; pattern = (Some pattern) }
  | Some restrict ->
      let p' = FanObjs.wildcarder#patt pattern in
      let match_fun =
        `Fun
          (_loc,
            (`Or
               (_loc,
                 (`CaseWhen
                    (_loc, pattern, restrict,
                      (`Id (_loc, (`Lid (_loc, "true")))))),
                 (`Case
                    (_loc, (`Any _loc), 
                      (`Id (_loc, (`Lid (_loc, "false"))))))))) in
      let descr = string_of_patt pattern in
      let text = `Stok (_loc, match_fun, "Antiquot", descr) in
      { text; styp; pattern = (Some p') }
let sfold ?sep  _loc (ns : string list) f e s =
  let fs = [("FOLD0", "sfold0"); ("FOLD1", "sfold1")] in
  let suffix = match sep with | None  -> "" | Some _ -> "sep" in
  let n = List.hd ns in
  let foldfun =
    try (List.assoc n fs) ^ suffix with | Not_found  -> invalid_arg "sfold" in
  let styp = `Quote (_loc, (`Normal _loc), (`Lid (_loc, (new_type_var ())))) in
  let e =
    `App
      (_loc,
        (`App
           (_loc,
             (`Id (_loc, (`Dot (_loc, (gm ()), (`Lid (_loc, foldfun)))))), f)),
        e) in
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
                             (_loc, (gm ()),
                               (`Lid (_loc, ("fold" ^ suffix))))))),
                     (`Any _loc)))), (s.styp))), styp) in
  let text =
    `Smeta
      (_loc, ns,
        (match sep with | None  -> [s.text] | Some sep -> [s.text; sep.text]),
        e, t) in
  { text; styp; pattern = None }
