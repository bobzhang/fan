let eprintf = Format.eprintf
let list_of_list = Fan_ops.list_of_list
let tuple_com = Ast_gen.tuple_com
let typing = Ast_gen.typing
let and_of_list = Ast_gen.and_of_list
let seq_sem = Ast_gen.seq_sem
open FAst
open Util
let print_warning = eprintf "%a:\n%s@." Locf.print
let prefix = "__fan_"
let ghost = Locf.ghost
let module_name = ref (`Uid (ghost, "Gramf"))
let gm () =
  match !Configf.compilation_unit with
  | Some "Gramf" -> `Uid (ghost, "")
  | Some _|None  -> !module_name
let check_add ((loc,id),v) env =
  if List.exists (fun ((_,i),_)  -> i = id) (!env)
  then Locf.failf loc "This variable %s is bound several times" id
  else env := (((loc, id), v) :: (!env))
let mk_prule ~prod  ~action  =
  let env = ref [] in
  let inner_env = ref [] in
  let i = ref 0 in
  let prod =
    Listf.filter_map
      (fun (p : Gram_def.osymbol Gram_def.decorate)  ->
         match p with
         | { kind = KSome ;
             txt = ({ outer_pattern = None ; bounds;_} as symbol) } ->
             (List.iter
                (fun ((xloc,id) as z)  ->
                   check_add
                     (z,
                       (`App (xloc, (`Uid (xloc, "Some")), (`Lid (xloc, id))) : 
                       FAst.exp )) inner_env) bounds;
              incr i;
              Some symbol)
         | { kind = KNormal ; txt = symbol } -> (incr i; Some symbol)
         | { kind = KSome ;
             txt = ({ outer_pattern = Some ((xloc,id) as z); bounds;_} as s)
             } ->
             (check_add
                (z,
                  (`App (xloc, (`Uid (xloc, "Some")), (`Lid (xloc, id))) : 
                  FAst.exp )) env;
              List.iter
                (fun ((xloc,id) as z)  ->
                   check_add
                     (z,
                       (`App (xloc, (`Uid (xloc, "Some")), (`Lid (xloc, id))) : 
                       FAst.exp )) inner_env) bounds;
              incr i;
              Some s)
         | { kind = KNone ; txt = { outer_pattern = None ; bounds;_} } ->
             (List.iter
                (fun ((xloc,_) as z)  ->
                   check_add (z, (`Uid (xloc, "None") : FAst.exp )) inner_env)
                bounds;
              None)
         | { kind = KNone ;
             txt = { outer_pattern = Some ((xloc,_) as z); bounds;_} } ->
             (check_add (z, (`Uid (xloc, "None") : FAst.exp )) env;
              List.iter
                (fun ((xloc,_) as z)  ->
                   check_add (z, (`Uid (xloc, "None") : FAst.exp )) inner_env)
                bounds;
              None)) prod in
  ({
     prod;
     action;
     inner_env = (List.rev (!inner_env));
     env = (List.rev (!env))
   } : Gram_def.rule )
let gen_lid () =
  let gensym = let i = ref 0 in fun ()  -> incr i; i in
  prefix ^ (string_of_int (!(gensym ())))
let rec make_exp (tvar : string) (x : Gram_def.text) =
  let rec aux tvar (x : Gram_def.text) =
    match x with
    | `List (_loc,min,t,ts) ->
        let txt = aux "" t.text in
        (match ts with
         | None  ->
             if min
             then (`App (_loc, (`Vrn (_loc, "List1")), txt) : FAst.exp )
             else (`App (_loc, (`Vrn (_loc, "List0")), txt) : FAst.exp )
         | Some s ->
             let x = aux tvar s.text in
             if min
             then
               (`App
                  (_loc, (`Vrn (_loc, "List1sep")),
                    (`Par (_loc, (`Com (_loc, txt, x))))) : FAst.exp )
             else
               (`App
                  (_loc, (`Vrn (_loc, "List0sep")),
                    (`Par (_loc, (`Com (_loc, txt, x))))) : FAst.exp ))
    | `Self _loc -> (`Vrn (_loc, "Self") : FAst.exp )
    | `Keyword (_loc,kwd) ->
        (`App (_loc, (`Vrn (_loc, "Keyword")), (`Str (_loc, kwd))) : 
        FAst.exp )
    | `Nterm (_loc,n,lev) ->
        let obj: FAst.exp =
          `App
            (_loc, (`Dot (_loc, (gm ()), (`Lid (_loc, "obj")))),
              (`Constraint
                 (_loc, (n.id :>exp),
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
             then (`Vrn (_loc, "Self") : FAst.exp )
             else (`App (_loc, (`Vrn (_loc, "Nterm")), obj) : FAst.exp ))
    | `Try (_loc,t) ->
        (`App (_loc, (`Vrn (_loc, "Try")), (aux "" t)) : FAst.exp )
    | `Peek (_loc,t) ->
        (`App (_loc, (`Vrn (_loc, "Peek")), (aux "" t)) : FAst.exp )
    | `Token (_loc,meta) ->
        (`App (_loc, (`Vrn (_loc, "Token")), meta) : FAst.exp ) in
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
  (let locid: FAst.pat = `Lid (_loc, (!Locf.name)) in
   let act = Option.default (`Uid (_loc, "()") : FAst.exp ) x.action in
   let tok_match_pl =
     snd @@
       (Listf.fold_lefti
          (fun i  ((oe,op) as acc)  x  ->
             match (x : Gram_def.osymbol ) with
             | { pattern = Some p; text = `Token _; outer_pattern = None ;_}
                 ->
                 let id = prefix ^ (string_of_int i) in
                 (((`Lid (_loc, id) : FAst.exp ) :: oe), (p :: op))
             | { pattern = Some p; text = `Token _;
                 outer_pattern = Some (xloc,id);_} ->
                 (((`Lid (xloc, id) : FAst.exp ) :: oe), (p :: op))
             | { pattern = Some p; text = `Keyword _;
                 outer_pattern = None ;_} ->
                 let id = prefix ^ (string_of_int i) in
                 (((`Lid (_loc, id) : FAst.exp ) :: oe), (p :: op))
             | { pattern = Some p; text = `Keyword _;
                 outer_pattern = Some (xloc,id);_} ->
                 (((`Lid (xloc, id) : FAst.exp ) :: oe), (p :: op))
             | _ -> acc) ([], []) x.prod) in
   let e =
     let make_env env =
       env |>
         (List.map
            (fun ((loc,id),e)  ->
               (`Bind (_loc, (`Lid (loc, id) : FAst.pat ), e) : FAst.bind ))) in
     let binds = make_env x.env in
     let inner_binds = make_env x.inner_env in
     let e1: FAst.exp =
       `Constraint
         (_loc, act, (`Quote (_loc, (`Normal _loc), (`Lid (_loc, rtvar))))) in
     let e1 = Ast_gen.binds inner_binds e1 in
     let e1 = Ast_gen.binds binds e1 in
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
         let e =
           if Fan_ops.is_irrefut_pat pat
           then (`Match (_loc, exp, (`Case (_loc, pat, e1))) : FAst.exp )
           else
             (`Match
                (_loc, exp,
                  (`Bar
                     (_loc, (`Case (_loc, pat, e1)),
                       (`Case
                          (_loc, (`Any _loc),
                            (`Assert (_loc, (`Lid (_loc, "false"))))))))) : 
             FAst.exp ) in
         (`Fun
            (_loc,
              (`Case
                 (_loc,
                   (`Constraint
                      (_loc, locid,
                        (`Dot
                           (_loc, (`Uid (_loc, "Locf")), (`Lid (_loc, "t")))))),
                   e))) : FAst.exp ) in
   let make_ctyp (styp : Gram_def.styp) tvar =
     (let rec aux v =
        match (v : Gram_def.styp ) with
        | #vid' as x -> (x : vid'  :>ctyp)
        | `Quote _ as x -> x
        | `App (_loc,t1,t2) -> (`App (_loc, (aux t1), (aux t2)) : FAst.ctyp )
        | `Self _loc ->
            if tvar = ""
            then
              (Locf.raise _loc) @@
                (Streamf.Error "S: illegal in anonymous entry level")
            else
              (`Quote (_loc, (`Normal _loc), (`Lid (_loc, tvar))) : FAst.ctyp )
        | `Type t -> t in
      aux styp : ctyp ) in
   let (+:) = typing in
   let txt =
     snd @@
       (Listf.fold_lefti
          (fun i  txt  (s : Gram_def.osymbol)  ->
             let mk_arg p =
               (`Label (_loc, (`Lid (_loc, (prefix ^ (string_of_int i)))), p) : 
               FAst.pat ) in
             match ((s.outer_pattern), (s.pattern)) with
             | (Some (xloc,id),_) ->
                 let p =
                   (`Lid (xloc, id) : FAst.pat ) +: (make_ctyp s.styp rtvar) in
                 (`Fun (_loc, (`Case (_loc, (mk_arg p), txt))) : FAst.exp )
             | (None ,Some _) ->
                 let p =
                   (`Lid (_loc, (prefix ^ (string_of_int i))) : FAst.pat ) +:
                     (make_ctyp s.styp rtvar) in
                 (`Fun (_loc, (`Case (_loc, (mk_arg p), txt))) : FAst.exp )
             | (None ,None ) ->
                 (`Fun
                    (_loc,
                      (`Case (_loc, (mk_arg (`Any _loc : FAst.pat )), txt))) : 
                 FAst.exp )) e x.prod) in
   (`App (_loc, (`Dot (_loc, (gm ()), (`Lid (_loc, "mk_action")))), txt) : 
     FAst.exp ) : exp )
let make_extend safe (e : Gram_def.entry) =
  (let _loc = (e.name).loc in
   let gmid = (gm () : vid  :>ident) in
   let ent: FAst.exp =
     `Constraint
       (_loc, ((e.name).id :>exp),
         (`App
            (_loc, (`Dot (_loc, gmid, (`Lid (_loc, "t")))),
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
                 r.prod |> (List.map (fun (s : Gram_def.osymbol)  -> s.text)) in
               (sl, (make_action _loc r (e.name).tvar), (r.action)))) in
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
            (`Par
               (_loc,
                 (`Com
                    (_loc, pos,
                      (`Constraint
                         (_loc, (apply l),
                           (`Dot
                              (_loc, (gm () : vid  :>ident),
                                (`Lid (_loc, "olevel"))))))))))) : FAst.exp )
   | `Group ls ->
       let txt = list_of_list _loc (List.map apply ls) in
       let f =
         if safe
         then (`Dot (_loc, (gm ()), (`Lid (_loc, "extend"))) : FAst.exp )
         else
           (`Dot (_loc, (gm ()), (`Lid (_loc, "unsafe_extend"))) : FAst.exp ) in
       (`App
          (_loc, (`App (_loc, f, ent)),
            (`Par
               (_loc,
                 (`Com
                    (_loc, pos,
                      (`Constraint
                         (_loc, txt,
                           (`App
                              (_loc, (`Lid (_loc, "list")),
                                (`Dot
                                   (_loc, (gm () : vid  :>ident),
                                     (`Lid (_loc, "olevel"))))))))))))) : 
         FAst.exp ) : exp )
let capture_antiquot =
  object 
    inherit  Objs.map as super
    val mutable constraints = ([] : (exp* exp) list )
    method! pat =
      function
      | `Ant (_loc,s) ->
          let code = s.txt in
          let cons: FAst.exp = `Lid (_loc, code) in
          let code' = "__fan__" ^ code in
          let cons': FAst.exp = `Lid (_loc, code') in
          let () = constraints <- (cons, cons') :: constraints in
          (`Lid (_loc, code') : FAst.pat )
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
    | { id = `Lid (_,i); tvar = x; loc = _loc } ->
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
    | _ ->
        failwithf "internal error in the Grammar extension %s"
          (Objs.dump_vid x.id) in
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
