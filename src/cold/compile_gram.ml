let eprintf = Format.eprintf
let list_of_list = Fan_ops.list_of_list
let is_irrefut_pat = Fan_ops.is_irrefut_pat
open FAst
open Ast_gen
open Util
let print_warning = eprintf "%a:\n%s@." Locf.print
let prefix = "__fan_"
let ghost = Locf.ghost
let module_name = ref (`Uid (ghost, "Gramf"))
let gm () =
  match Configf.compilation_unit.contents with
  | Some "Gramf" -> `Uid (ghost, "")
  | Some _|None  -> module_name.contents
let mk_entry ~local  ~name  ~pos  ~levels  =
  { Gram_def.name = name; pos; levels; local }
let mk_level ~label  ~assoc  ~rules  =
  ({ label; assoc; rules } : Gram_def.level )
let mk_rule ~prod  ~action  = ({ prod; action } : Gram_def.rule )
let mk_symbol ?(pattern= None)  ~text  ~styp  =
  ({ text; styp; pattern } : Gram_def.symbol )
let mk_slist loc min sep symb = `Slist (loc, min, symb, sep)
let new_type_var =
  let i = ref 0 in fun ()  -> incr i; "e__" ^ (string_of_int i.contents)
let gensym = let i = ref 0 in fun ()  -> incr i; i
let gen_lid () = prefix ^ (string_of_int (gensym ()).contents)
let make_ctyp (styp : Gram_def.styp) tvar =
  (let rec aux v =
     match (v : Gram_def.styp ) with
     | #vid' as x -> (x : vid'  :>ctyp)
     | `Quote _ as x -> x
     | `App (_loc,t1,t2) -> (`App (_loc, (aux t1), (aux t2)) : FAst.ctyp )
     | `Self _loc ->
         if tvar = ""
         then
           Locf.raise _loc
             (Streamf.Error "S: illegal in anonymous entry level")
         else
           (`Quote (_loc, (`Normal _loc), (`Lid (_loc, tvar))) : FAst.ctyp )
     | `Tok _loc ->
         (`Dot (_loc, (`Uid (_loc, "Tokenf")), (`Lid (_loc, "t"))) : 
         FAst.ctyp )
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
             then (`App (_loc, (`Vrn (_loc, "Slist1")), txt) : FAst.exp )
             else (`App (_loc, (`Vrn (_loc, "Slist0")), txt) : FAst.exp )
         | Some s ->
             let x = aux tvar s.text in
             if min
             then
               (`App
                  (_loc, (`Vrn (_loc, "Slist1sep")),
                    (`Par (_loc, (`Com (_loc, txt, x))))) : FAst.exp )
             else
               (`App
                  (_loc, (`Vrn (_loc, "Slist0sep")),
                    (`Par (_loc, (`Com (_loc, txt, x))))) : FAst.exp ))
    | `Sself _loc -> (`Vrn (_loc, "Sself") : FAst.exp )
    | `Skeyword (_loc,kwd) ->
        (`App (_loc, (`Vrn (_loc, "Skeyword")), (`Str (_loc, kwd))) : 
        FAst.exp )
    | `Snterm (_loc,n,lev) ->
        let obj: FAst.exp =
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
             FAst.exp )
         | None  ->
             if n.tvar = tvar
             then (`Vrn (_loc, "Sself") : FAst.exp )
             else (`App (_loc, (`Vrn (_loc, "Snterm")), obj) : FAst.exp ))
    | `Sopt (_loc,t) ->
        (`App (_loc, (`Vrn (_loc, "Sopt")), (aux "" t)) : FAst.exp )
    | `Stry (_loc,t) ->
        (`App (_loc, (`Vrn (_loc, "Stry")), (aux "" t)) : FAst.exp )
    | `Speek (_loc,t) ->
        (`App (_loc, (`Vrn (_loc, "Speek")), (aux "" t)) : FAst.exp )
    | `Stoken (_loc,match_fun,mdescr,mstr) ->
        (`App
           (_loc, (`Vrn (_loc, "Stoken")),
             (`Par
                (_loc,
                  (`Com
                     (_loc, match_fun,
                       (`Com (_loc, mdescr, (`Str (_loc, mstr))))))))) : 
        FAst.exp ) in
  aux tvar x
and make_exp_rules (_loc : loc)
  (rl : (Gram_def.text list* exp* exp option) list) (tvar : string) =
  (rl |>
     (List.map
        (fun (sl,action,raw)  ->
           let action_string =
             match raw with | None  -> "" | Some e -> Ast2pt.to_string_exp e in
           let sl = (sl |> (List.map (make_exp tvar))) |> (list_of_list _loc) in
           (`Par
              (_loc,
                (`Com
                   (_loc, sl,
                     (`Par
                        (_loc,
                          (`Com (_loc, (`Str (_loc, action_string)), action))))))) : 
             FAst.exp ))))
    |> (list_of_list _loc)
let make_action (_loc : loc) (x : Gram_def.rule) (rtvar : string) =
  (let locid: FAst.pat = `Lid (_loc, (Locf.name.contents)) in
   let act = Option.default (`Uid (_loc, "()") : FAst.exp ) x.action in
   let (_,tok_match_pl) =
     Listf.fold_lefti
       (fun i  ((oe,op) as ep)  x  ->
          match (x : Gram_def.symbol ) with
          | { pattern = Some p; text = `Stoken _;_} when
              not (is_irrefut_pat p) ->
              let id = prefix ^ (string_of_int i) in
              (((`Lid (_loc, id) : FAst.exp ) :: oe), (p :: op))
          | { pattern = Some p; text = `Skeyword _;_} ->
              let id = prefix ^ (string_of_int i) in
              (((`Lid (_loc, id) : FAst.exp ) :: oe), (p :: op))
          | _ -> ep) ([], []) x.prod in
   let e =
     let e1: FAst.exp =
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
                   e1))) : FAst.exp )
     | (e,p) ->
         let (exp,pat) =
           match (e, p) with
           | (x::[],y::[]) -> (x, y)
           | _ -> ((tuple_com e), (tuple_com p)) in
         let len = List.length e in
         let error_fmt = String.concat " " (Listf.init len (fun _  -> "%s")) in
         let es =
           List.map
             (fun x  ->
                (`App
                   (_loc,
                     (`Dot
                        (_loc, (`Uid (_loc, "Tokenf")),
                          (`Lid (_loc, "token_to_string")))), x) : FAst.exp ))
             e in
         let error =
           Ast_gen.appl_of_list
             ([(`Dot
                  (_loc, (`Uid (_loc, "Printf")), (`Lid (_loc, "sprintf"))) : 
              FAst.exp );
              (`Str (_loc, (String.escaped error_fmt)) : FAst.exp )] @ es) in
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
                                     (_loc, (`Lid (_loc, "failwith")), error))))))))))) : 
           FAst.exp ) in
   let (_,txt) =
     Listf.fold_lefti
       (fun i  txt  (s : Gram_def.symbol)  ->
          match s.pattern with
          | Some (`Alias (_loc,`App (_,_,`Par (_,(`Any _ : FAst.pat))),p)) ->
              let p = typing (p : alident  :>pat) (make_ctyp s.styp rtvar) in
              (`Fun (_loc, (`Case (_loc, p, txt))) : FAst.exp )
          | Some p when is_irrefut_pat p ->
              let p = typing p (make_ctyp s.styp rtvar) in
              (`Fun (_loc, (`Case (_loc, p, txt))) : FAst.exp )
          | Some _ ->
              let p =
                typing
                  (`Lid (_loc, (prefix ^ (string_of_int i))) : FAst.pat )
                  (make_ctyp s.styp rtvar) in
              (`Fun (_loc, (`Case (_loc, p, txt))) : FAst.exp )
          | None  ->
              (`Fun (_loc, (`Case (_loc, (`Any _loc), txt))) : FAst.exp )) e
       x.prod in
   (`App (_loc, (`Dot (_loc, (gm ()), (`Lid (_loc, "mk_action")))), txt) : 
     FAst.exp ) : exp )
let make_extend safe (e : Gram_def.entry) =
  (let _loc = (e.name).loc in
   let ent: FAst.exp =
     `Constraint
       (_loc, ((e.name).exp),
         (`App
            (_loc, (`Dot (_loc, (gm () : vid  :>ident), (`Lid (_loc, "t")))),
              (`Quote (_loc, (`Normal _loc), (`Lid (_loc, ((e.name).tvar)))))))) in
   let pos =
     match e.pos with
     | Some pos -> (`App (_loc, (`Uid (_loc, "Some")), pos) : FAst.exp )
     | None  -> (`Uid (_loc, "None") : FAst.exp ) in
   let apply (level : Gram_def.level) =
     let lab =
       match level.label with
       | Some lab ->
           (`App (_loc, (`Uid (_loc, "Some")), (`Str (_loc, lab))) : 
           FAst.exp )
       | None  -> (`Uid (_loc, "None") : FAst.exp ) in
     let ass =
       match level.assoc with
       | Some ass -> (`App (_loc, (`Uid (_loc, "Some")), ass) : FAst.exp )
       | None  -> (`Uid (_loc, "None") : FAst.exp ) in
     let rl =
       level.rules |>
         (List.map
            (fun (r : Gram_def.rule)  ->
               let sl =
                 r.prod |> (List.map (fun (s : Gram_def.symbol)  -> s.text)) in
               let ac = make_action _loc r (e.name).tvar in
               (sl, ac, (r.action)))) in
     let prod = make_exp_rules _loc rl (e.name).tvar in
     (`Par (_loc, (`Com (_loc, lab, (`Com (_loc, ass, prod))))) : FAst.exp ) in
   match e.levels with
   | `Single l ->
       let f =
         if safe
         then
           (`Dot (_loc, (gm ()), (`Lid (_loc, "extend_single"))) : FAst.exp )
         else
           (`Dot (_loc, (gm ()), (`Lid (_loc, "unsafe_extend_single"))) : 
           FAst.exp ) in
       (`App
          (_loc, (`App (_loc, f, ent)),
            (`Par (_loc, (`Com (_loc, pos, (apply l)))))) : FAst.exp )
   | `Group ls ->
       let txt = list_of_list _loc (List.map apply ls) in
       let f =
         if safe
         then (`Dot (_loc, (gm ()), (`Lid (_loc, "extend"))) : FAst.exp )
         else
           (`Dot (_loc, (gm ()), (`Lid (_loc, "unsafe_extend"))) : FAst.exp ) in
       (`App
          (_loc, (`App (_loc, f, ent)),
            (`Par (_loc, (`Com (_loc, pos, txt))))) : FAst.exp ) : exp )
let capture_antiquot =
  object 
    inherit  Objs.map as super
    val mutable constraints = ([] : (exp* exp) list )
    method! pat =
      function
      | `Ant (_loc,s) ->
          (match s with
           | { FanUtil.content = code;_} ->
               let cons: FAst.exp = `Lid (_loc, code) in
               let code' = "__fan__" ^ code in
               let cons': FAst.exp = `Lid (_loc, code') in
               let () = constraints <- (cons, cons') :: constraints in
               (`Lid (_loc, code') : FAst.pat ))
      | p -> super#pat p
    method get_captured_variables = constraints
    method clear_captured_variables = constraints <- []
  end
let filter_pat_with_captured_variables pat =
  capture_antiquot#clear_captured_variables;
  (let pat = capture_antiquot#pat pat in
   let constraints = capture_antiquot#get_captured_variables in
   (pat, constraints))
let combine _loc (gram : vid option) locals extends =
  let entry_mk =
    match gram with
    | Some g ->
        let g = (g : vid  :>exp) in
        (`App (_loc, (`Dot (_loc, (gm ()), (`Lid (_loc, "mk_dynamic")))), g) : 
          FAst.exp )
    | None  -> (`Dot (_loc, (gm ()), (`Lid (_loc, "mk"))) : FAst.exp ) in
  let local_bind_of_name (x : Gram_def.name) =
    match (x : Gram_def.name ) with
    | { exp = (`Lid (_,i) : FAst.exp); tvar = x; loc = _loc } ->
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
        FAst.bind )
    | { exp;_} ->
        failwithf "internal error in the Grammar extension %s"
          (Objs.dump_exp exp) in
  match locals with
  | [] -> extends
  | ll ->
      let locals = (ll |> (List.map local_bind_of_name)) |> and_of_list in
      (`LetIn
         (_loc, (`Negative _loc),
           (`Bind
              (_loc, (`Lid (_loc, "grammar_entry_create")),
                (`Fun
                   (_loc,
                     (`Case
                        (_loc, (`Lid (_loc, "x")),
                          (`App (_loc, entry_mk, (`Lid (_loc, "x")))))))))),
           (`LetIn (_loc, (`Negative _loc), locals, extends))) : FAst.exp )
let make _loc (x : Gram_def.entries) =
  let extends =
    let el = x.items |> (List.map (make_extend x.safe)) in
    match el with | [] -> (`Uid (_loc, "()") : FAst.exp ) | _ -> seq_sem el in
  let locals =
    x.items |>
      (Listf.filter_map
         (fun (x : Gram_def.entry)  ->
            if x.local then Some (x.name) else None)) in
  combine _loc x.gram locals extends