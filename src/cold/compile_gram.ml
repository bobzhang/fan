let eprintf = Format.eprintf
let list_of_list = Fan_ops.list_of_list
let tuple_com = Ast_gen.tuple_com
let typing = Ast_gen.typing
let and_of_list = Ast_gen.and_of_list
let seq_sem = Ast_gen.seq_sem
open Astf
open Util
let print_warning = eprintf "%a:\n%s@." Locf.print
let prefix = "__fan_"
let ghost = Locf.ghost
let module_name = ref (`Uid (ghost, "Gramf"))
let gm () =
  match !Configf.compilation_unit with
  | Some "Gramf" -> `Uid (ghost, "")
  | Some _|None  -> !module_name
let add ?(check= true)  ((loc,id),v) env =
  if check && (List.exists (fun ((_,i),_)  -> i = id) (!env))
  then Locf.failf loc "This variable %s is bound several times" id
  else env := (((loc, id), v) :: (!env))
let enhance_env (s : string) xs env =
  xs |>
    (List.iter
       (fun (((loc,_) as v),opt)  ->
          match opt with
          | None  -> add (v, (`Lid (loc, s) : Astf.exp )) env
          | Some l ->
              add
                (v,
                  (`Field (loc, (`Lid (loc, s)), (`Lid (loc, l))) : Astf.exp ))
                env))
let mk_prule ~prod  ~action  =
  let env = ref [] in
  let i = ref 0 in
  let prod =
    Listf.filter_map
      (fun (p : Gram_def.osymbol list Gram_def.decorate)  ->
         match p with
         | { kind = KSome ; txt } ->
             Some
               (txt |>
                  (List.map
                     (fun (symbol : Gram_def.osymbol)  ->
                        match symbol with
                        | { outer_pattern = None ; bounds;_} ->
                            let id = prefix ^ (string_of_int (!i)) in
                            (enhance_env id bounds env;
                             List.iter
                               (fun (((xloc,id) as z),_)  ->
                                  add ~check:false
                                    (z,
                                      (`App
                                         (xloc, (`Uid (xloc, "Some")),
                                           (`Lid (xloc, id))) : Astf.exp ))
                                    env) bounds;
                             incr i;
                             symbol)
                        | { outer_pattern = Some ((xloc,id) as z); bounds;_}
                            as s ->
                            (enhance_env id bounds env;
                             add ~check:false
                               (z,
                                 (`App
                                    (xloc, (`Uid (xloc, "Some")),
                                      (`Lid (xloc, id))) : Astf.exp )) env;
                             List.iter
                               (fun (((xloc,id) as z),_)  ->
                                  add ~check:false
                                    (z,
                                      (`App
                                         (xloc, (`Uid (xloc, "Some")),
                                           (`Lid (xloc, id))) : Astf.exp ))
                                    env) bounds;
                             incr i;
                             s))))
         | { kind = KNormal ; txt } ->
             Some
               (List.map
                  (fun (symbol : Gram_def.osymbol)  ->
                     match symbol with
                     | { outer_pattern = None ; bounds;_} ->
                         let id = prefix ^ (string_of_int (!i)) in
                         (enhance_env id bounds env; incr i; symbol)
                     | { outer_pattern = Some (_,id); bounds;_} as symbol ->
                         (enhance_env id bounds env; incr i; symbol)) txt)
         | { kind = KNone ; txt } ->
             (List.iter
                (fun (symbol : Gram_def.osymbol)  ->
                   match symbol with
                   | { outer_pattern = None ; bounds;_} ->
                       List.iter
                         (fun (((xloc,_) as z),_)  ->
                            add (z, (`Uid (xloc, "None") : Astf.exp )) env)
                         bounds
                   | { outer_pattern = Some ((xloc,_) as z); bounds;_} ->
                       (add (z, (`Uid (xloc, "None") : Astf.exp )) env;
                        List.iter
                          (fun (((xloc,_) as z),_)  ->
                             add (z, (`Uid (xloc, "None") : Astf.exp )) env)
                          bounds)) txt;
              None)) prod in
  ({ prod = (List.concat prod); action; env = (List.rev (!env)) } : Gram_def.rule )
let rec make_exp (tvar : string) (x : Gram_def.text) =
  let rec aux tvar (x : Gram_def.text) =
    match x with
    | List (_loc,min,t,ts) ->
        let txt = aux "" t.text in
        (match ts with
         | None  ->
             if min
             then
               (`App (_loc, (`Uid (_loc, "List1")), (txt :>Astf.exp)) : 
               Astf.exp )
             else
               (`App (_loc, (`Uid (_loc, "List0")), (txt :>Astf.exp)) : 
               Astf.exp )
         | Some s ->
             let x = aux tvar s.text in
             if min
             then
               (`App
                  (_loc, (`Uid (_loc, "List1sep")),
                    (`Par
                       (_loc,
                         (`Com (_loc, (txt :>Astf.exp), (x :>Astf.exp)))))) : 
               Astf.exp )
             else
               (`App
                  (_loc, (`Uid (_loc, "List0sep")),
                    (`Par
                       (_loc,
                         (`Com (_loc, (txt :>Astf.exp), (x :>Astf.exp)))))) : 
               Astf.exp ))
    | Self _loc -> (`Uid (_loc, "Self") : Astf.exp )
    | Keyword (_loc,kwd) ->
        (`App (_loc, (`Vrn (_loc, "Keyword")), (`Str (_loc, kwd))) : 
        Astf.exp )
    | Nterm (_loc,n,lev) ->
        let obj: Astf.exp =
          `App
            (_loc, (`Dot (_loc, (gm () :>Astf.vid), (`Lid (_loc, "obj")))),
              (`Constraint
                 (_loc, (n.id :>Astf.exp),
                   (`App
                      (_loc,
                        (`Dot
                           (_loc, ((gm () : vid  :>ident) :>Astf.ident),
                             (`Lid (_loc, "t")))),
                        (`Quote
                           (_loc, (`Normal _loc), (`Lid (_loc, (n.tvar)))))))))) in
        (match lev with
         | Some lab ->
             (`App
                (_loc, (`Uid (_loc, "Snterml")),
                  (`Par
                     (_loc,
                       (`Com
                          (_loc, (obj :>Astf.exp),
                            (`Int (_loc, (string_of_int lab)))))))) : 
             Astf.exp )
         | None  ->
             if n.tvar = tvar
             then (`Uid (_loc, "Self") : Astf.exp )
             else
               (`App (_loc, (`Uid (_loc, "Nterm")), (obj :>Astf.exp)) : 
               Astf.exp ))
    | Try (_loc,t) ->
        (`App (_loc, (`Uid (_loc, "Try")), (aux "" t :>Astf.exp)) : Astf.exp )
    | Peek (_loc,t) ->
        (`App (_loc, (`Uid (_loc, "Peek")), (aux "" t :>Astf.exp)) : 
        Astf.exp )
    | Token (_loc,meta) ->
        (`App (_loc, (`Uid (_loc, "Token")), (meta :>Astf.exp)) : Astf.exp ) in
  aux tvar x
and make_exp_rules (rl : (Gram_def.text list* exp* Gram_def.action) list)
  (tvar : string) =
  (rl |>
     (List.map
        (fun (sl,action,(raw : Gram_def.action))  ->
           let action_string =
             match raw with
             | E (None ) -> ""
             | E (Some e) -> Ast2pt.to_string_exp e
             | Ant _ -> "" in
           let sl = (sl |> (List.map (make_exp tvar))) |> list_of_list in
           let _loc = Ast_loc.loc_of sl in
           (`Record
              (_loc,
                (`Sem
                   (_loc,
                     (`RecBind
                        (_loc, (`Lid (_loc, "symbols")), (sl :>Astf.exp))),
                     (`Sem
                        (_loc,
                          (`RecBind
                             (_loc, (`Lid (_loc, "annot")),
                               (`Str (_loc, action_string)))),
                          (`RecBind
                             (_loc, (`Lid (_loc, "fn")), (action :>Astf.exp)))))))) : 
             Astf.exp ))))
    |> list_of_list
let make_action (_loc : loc) (x : Gram_def.rule) (rtvar : string) =
  (let locid: Astf.pat = `Lid (_loc, (!Locf.name)) in
   let make_ctyp (styp : Gram_def.styp) tvar =
     (let rec aux v =
        match (v : Gram_def.styp ) with
        | #vid' as x -> (x : vid'  :>ctyp)
        | `Quote _ as x -> x
        | `App (_loc,t1,t2) ->
            (`App (_loc, (aux t1 :>Astf.ctyp), (aux t2 :>Astf.ctyp)) : 
            Astf.ctyp )
        | `Self _loc ->
            if tvar = ""
            then
              (Locf.raise _loc) @@
                (Streamf.Error "S: illegal in anonymous entry level")
            else
              (`Quote (_loc, (`Normal _loc), (`Lid (_loc, tvar))) : Astf.ctyp )
        | `Type t -> t in
      aux styp : ctyp ) in
   let (+:) = typing in
   match x.action with
   | Ant v ->
       let e = Tokenf.ant_expand Parsef.exp v in
       let ty =
         List.fold_left
           (fun ty  (s : Gram_def.osymbol)  ->
              let t = make_ctyp s.styp rtvar in
              (`Arrow (_loc, (t :>Astf.ctyp), (ty :>Astf.ctyp)) : Astf.ctyp ))
           (`Arrow
              (_loc,
                (`Dot (_loc, (`Uid (_loc, "Locf")), (`Lid (_loc, "t")))),
                (`Quote (_loc, (`Normal _loc), (`Lid (_loc, rtvar))))) : 
           Astf.ctyp ) x.prod in
       (`App
          (_loc,
            (`Dot (_loc, (gm () :>Astf.vid), (`Lid (_loc, "mk_action")))),
            (`Constraint (_loc, (e :>Astf.exp), (ty :>Astf.ctyp)))) : 
         Astf.exp )
   | E v ->
       let e =
         let act = Option.default (`Unit _loc : Astf.exp ) v in
         let make_env env =
           env |>
             (List.map
                (fun ((loc,id),e)  ->
                   (`Bind
                      (_loc, ((`Lid (loc, id) : Astf.pat ) :>Astf.pat),
                        (e :>Astf.exp)) : Astf.bind ))) in
         let binds = make_env x.env in
         let e1: Astf.exp =
           `Constraint
             (_loc, (act :>Astf.exp),
               (`Quote (_loc, (`Normal _loc), (`Lid (_loc, rtvar))))) in
         let e1 = Ast_gen.seq_binds binds e1 in
         (`Fun
            (_loc,
              (`Case
                 (_loc,
                   (`Constraint
                      (_loc, (locid :>Astf.pat),
                        (`Dot
                           (_loc, (`Uid (_loc, "Locf")), (`Lid (_loc, "t")))))),
                   (e1 :>Astf.exp)))) : Astf.exp ) in
       let (ty,txt) =
         snd @@
           (Listf.fold_lefti
              (fun i  (ty,txt)  (s : Gram_def.osymbol)  ->
                 match ((s.outer_pattern), (s.bounds)) with
                 | (Some (xloc,id),_) ->
                     let t = make_ctyp s.styp rtvar in
                     let p = (`Lid (xloc, id) : Astf.pat ) +: t in
                     ((`Arrow (_loc, (t :>Astf.ctyp), (ty :>Astf.ctyp)) : 
                       Astf.ctyp ),
                       (`Fun
                          (_loc,
                            (`Case (_loc, (p :>Astf.pat), (txt :>Astf.exp)))) : 
                       Astf.exp ))
                 | (None ,[]) ->
                     let t = make_ctyp s.styp rtvar in
                     ((`Arrow (_loc, (t :>Astf.ctyp), (ty :>Astf.ctyp)) : 
                       Astf.ctyp ),
                       (`Fun
                          (_loc,
                            (`Case (_loc, (`Any _loc), (txt :>Astf.exp)))) : 
                       Astf.exp ))
                 | (None ,_) ->
                     let t = make_ctyp s.styp rtvar in
                     let p =
                       (`Lid (_loc, (prefix ^ (string_of_int i))) : Astf.pat )
                         +: t in
                     ((`Arrow (_loc, (t :>Astf.ctyp), (ty :>Astf.ctyp)) : 
                       Astf.ctyp ),
                       (`Fun
                          (_loc,
                            (`Case (_loc, (p :>Astf.pat), (txt :>Astf.exp)))) : 
                       Astf.exp )))
              ((`Arrow
                  (_loc,
                    (`Dot (_loc, (`Uid (_loc, "Locf")), (`Lid (_loc, "t")))),
                    (`Quote (_loc, (`Normal _loc), (`Lid (_loc, rtvar))))) : 
                Astf.ctyp ), e) x.prod) in
       (`App
          (_loc,
            (`Dot (_loc, (gm () :>Astf.vid), (`Lid (_loc, "mk_action")))),
            (`Constraint (_loc, (txt :>Astf.exp), (ty :>Astf.ctyp)))) : 
         Astf.exp ) : exp )
let make_single_extend_statement (e : Gram_def.entry) =
  (let _loc = (e.name).loc in
   let gmid = (gm () : vid  :>ident) in
   let ent: Astf.exp =
     `Constraint
       (_loc, ((e.name).id :>Astf.exp),
         (`App
            (_loc, (`Dot (_loc, (gmid :>Astf.ident), (`Lid (_loc, "t")))),
              (`Quote (_loc, (`Normal _loc), (`Lid (_loc, ((e.name).tvar)))))))) in
   let pos =
     match e.pos with
     | Some pos ->
         (`App (_loc, (`Uid (_loc, "Some")), (pos :>Astf.exp)) : Astf.exp )
     | None  -> (`Uid (_loc, "None") : Astf.exp ) in
   let apply (level : Gram_def.level) =
     let ass =
       match level.assoc with
       | Some ass -> ass
       | None  -> (`Bool (_loc, true) : Astf.exp ) in
     let rl =
       level.rules |>
         (List.map
            (fun (r : Gram_def.rule)  ->
               let sl =
                 r.prod |> (List.map (fun (s : Gram_def.osymbol)  -> s.text)) in
               (sl, (make_action _loc r (e.name).tvar), (r.action)))) in
     let prod = make_exp_rules rl (e.name).tvar in
     (`Constraint
        (_loc,
          (`Record
             (_loc,
               (`Sem
                  (_loc,
                    (`RecBind
                       (_loc, (`Lid (_loc, "label")), (pos :>Astf.exp))),
                    (`Sem
                       (_loc,
                         (`RecBind
                            (_loc, (`Lid (_loc, "lassoc")), (ass :>Astf.exp))),
                         (`RecBind
                            (_loc, (`Lid (_loc, "productions")),
                              (prod :>Astf.exp))))))))),
          (`Dot
             (_loc, ((gm () : vid  :>ident) :>Astf.ident),
               (`Lid (_loc, "olevel"))))) : Astf.exp ) in
   let l = e.level in
   (`Constraint
      (_loc,
        (`Record
           (_loc,
             (`Sem
                (_loc,
                  (`RecBind (_loc, (`Lid (_loc, "entry")), (ent :>Astf.exp))),
                  (`RecBind
                     (_loc, (`Lid (_loc, "olevel")), (apply l :>Astf.exp))))))),
        (`App
           (_loc,
             (`Dot
                (_loc, (`Uid (_loc, "Gramf")),
                  (`Lid (_loc, "single_extend_statement")))), (`Any _loc)))) : 
     Astf.exp ) : exp )
let make_extend safe (e : Gram_def.entry) =
  (let _loc = (e.name).loc in
   let f =
     if safe
     then
       (`Dot (_loc, (gm () :>Astf.vid), (`Lid (_loc, "extend_single"))) : 
       Astf.exp )
     else
       (`Dot
          (_loc, (gm () :>Astf.vid), (`Lid (_loc, "unsafe_extend_single"))) : 
       Astf.exp ) in
   (`App (_loc, (f :>Astf.exp), (make_single_extend_statement e :>Astf.exp)) : 
     Astf.exp ) : exp )
let make_localbinds _loc locals =
  let local_bind_of_name (x : Gram_def.name) =
    match (x : Gram_def.name ) with
    | { id = `Lid (_,i); tvar = x; loc = _loc } ->
        (`Bind
           (_loc, (`Lid (_loc, i)),
             (`Constraint
                (_loc,
                  (`App
                     (_loc,
                       (`Dot
                          (_loc, (`Uid (_loc, "Gramf")), (`Lid (_loc, "mk")))),
                       (`Str (_loc, i)))),
                  (`App
                     (_loc,
                       (`Dot
                          (_loc, ((gm () : vid  :>ident) :>Astf.ident),
                            (`Lid (_loc, "t")))),
                       (`Quote (_loc, (`Normal _loc), (`Lid (_loc, x))))))))) : 
        Astf.bind )
    | _ ->
        failwithf "internal error in the Grammar extension %s"
          (Objs.dump_vid x.id) in
  List.map local_bind_of_name locals
let make _loc (x : Gram_def.entries) =
  let extends =
    let el = x.items |> (List.map (make_extend x.safe)) in
    match el with | [] -> (`Unit _loc : Astf.exp ) | _ -> seq_sem el in
  let locals =
    x.items |>
      (Listf.filter_map
         (fun (x : Gram_def.entry)  ->
            if x.local then Some (x.name) else None)) in
  Ast_gen.binds (make_localbinds _loc locals) extends
let make_protects _loc (x : Gram_def.entries) action =
  let (_,globals) =
    List.partition (fun (x : Gram_def.entry)  -> x.local) x.items in
  let binds =
    List.map
      (fun (x : Gram_def.entry)  ->
         ((Gensym.fresh ~prefix:"tmp_entry" ()),
           (((x.name).id :>Astf.exp) : Astf.exp ))) globals in
  let save_binds =
    List.map
      (fun (tmp,e)  ->
         (`Bind
            (_loc, (`Lid (_loc, tmp)),
              (`App
                 (_loc,
                   (`Dot
                      (_loc, (`Uid (_loc, "Gramf")),
                        (`Lid (_loc, "get_levels")))), (e :>Astf.exp)))) : 
         Astf.bind )) binds in
  let pop_actions =
    seq_sem @@
      (Listf.map
         (fun (tmp,e)  ->
            (`App
               (_loc,
                 (`App
                    (_loc,
                      (`Dot
                         (_loc, (`Uid (_loc, "Gramf")),
                           (`Lid (_loc, "fresh_with_levels")))),
                      (e :>Astf.exp))), (`Lid (_loc, tmp))) : Astf.exp ))
         binds) in
  let e = make _loc x in
  Ast_gen.binds save_binds
    (`Try
       (_loc,
         (`Seq
            (_loc,
              (`Sem
                 (_loc, (e :>Astf.exp),
                   (`LetIn
                      (_loc, (`Negative _loc),
                        (`Bind
                           (_loc, (`Lid (_loc, "result")),
                             (action :>Astf.exp))),
                        (`Seq
                           (_loc,
                             (`Sem
                                (_loc, (pop_actions :>Astf.exp),
                                  (`Lid (_loc, "result")))))))))))),
         (`Case
            (_loc, (`Lid (_loc, "x")),
              (`Seq
                 (_loc,
                   (`Sem
                      (_loc, (pop_actions :>Astf.exp),
                        (`App
                           (_loc, (`Lid (_loc, "raise")), (`Lid (_loc, "x"))))))))))) : 
    Astf.exp )
