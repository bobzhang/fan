open Astf
open AstLib
open LibUtil
type 'a item_or_def =  
  | Str of 'a
  | Def of string* (string list* exp) option
  | Und of string 
let defined = ref []
let substp loc (env : (string* pat) list) =
  let bad_pat _loc =
    Locf.errorf _loc
      "this macro cannot be used in a pattern (see its definition)" in
  let rec loop (x : exp) =
    match x with
    | (`App (_loc,e1,e2) : Astf.exp) ->
        (`App (loc, (loop e1), (loop e2)) : Astf.pat )
    | (`Lid (_loc,x) : Astf.exp) ->
        (try List.assoc x env
         with | Not_found  -> (`Lid (loc, x) : Astf.pat ))
    | (`Uid (_loc,x) : Astf.exp) ->
        (try List.assoc x env
         with | Not_found  -> (`Uid (loc, x) : Astf.pat ))
    | (`Int (_loc,x) : Astf.exp) -> (`Int (loc, x) : Astf.pat )
    | (`Str (_loc,s) : Astf.exp) -> (`Str (loc, s) : Astf.pat )
    | (`Par (_loc,x) : Astf.exp) -> (`Par (loc, (loop x)) : Astf.pat )
    | (`Com (_loc,x1,x2) : Astf.exp) ->
        (`Com (loc, (loop x1), (loop x2)) : Astf.pat )
    | (`Record (_loc,bi) : Astf.exp) ->
        let rec substbi =
          function
          | (`Sem (_loc,b1,b2) : Astf.rec_exp) ->
              `Sem (_loc, (substbi b1), (substbi b2))
          | (`RecBind (_loc,i,e) : Astf.rec_exp) ->
              `RecBind (loc, i, (loop e))
          | _ -> bad_pat _loc in
        (`Record (loc, (substbi bi)) : Astf.pat )
    | _ -> bad_pat loc in
  loop
class subst loc env =
  object 
    inherit  (Objs.reloc loc) as super
    method! exp =
      function
      | (`Lid (_loc,x) : Astf.exp)|(`Uid (_loc,x) : Astf.exp) as e ->
          (try List.assoc x env with | Not_found  -> super#exp e)
      | (`App (_loc,`Uid (_,"LOCATION_OF"),`Lid (_,x)) : Astf.exp)
        |(`App (_loc,`Uid (_,"LOCATION_OF"),`Uid (_,x)) : Astf.exp) as e ->
          (try
             let loc = loc_of (List.assoc x env) in
             let (a,b,c,d,e,f,g,h) = Locf.to_tuple loc in
             (`App
                (_loc,
                  (`Dot
                     (_loc, (`Uid (_loc, "Locf")), (`Lid (_loc, "of_tuple")))),
                  (`Par
                     (_loc,
                       (`Com
                          (_loc, (`Str (_loc, (String.escaped a))),
                            (`Com
                               (_loc,
                                 (`Com
                                    (_loc,
                                      (`Com
                                         (_loc,
                                           (`Com
                                              (_loc,
                                                (`Com
                                                   (_loc,
                                                     (`Com
                                                        (_loc,
                                                          (`Int
                                                             (_loc,
                                                               (string_of_int
                                                                  b))),
                                                          (`Int
                                                             (_loc,
                                                               (string_of_int
                                                                  c))))),
                                                     (`Int
                                                        (_loc,
                                                          (string_of_int d))))),
                                                (`Int
                                                   (_loc, (string_of_int e))))),
                                           (`Int (_loc, (string_of_int f))))),
                                      (`Int (_loc, (string_of_int g))))),
                                 (if h
                                  then (`Lid (_loc, "true") : Astf.exp )
                                  else (`Lid (_loc, "false") : Astf.exp ))))))))) : 
               Astf.exp )
           with | Not_found  -> super#exp e)
      | e -> super#exp e
    method! pat =
      function
      | (`Lid (_loc,x) : Astf.pat)|(`Uid (_loc,x) : Astf.pat) as p ->
          (try substp loc [] (List.assoc x env)
           with | Not_found  -> super#pat p)
      | p -> super#pat p
  end
let define ~exp  ~pat  eo y =
  let incorrect_number loc l1 l2 =
    Locf.raise loc
      (Failure
         (Printf.sprintf "expected %d parameters; found %d" (List.length l2)
            (List.length l1))) in
  (match eo with
   | Some ([],e) ->
       (Gramf.extend_single (exp : 'exp Gramf.t )
          ((Some (`Level "simple")),
            (None, None,
              [([`Stoken
                   (((function
                      | `Uid __fan__y when y = __fan__y -> true
                      | _ -> false)), (`App ((`Vrn "Uid"), (`Str y))),
                     "`Uid $y")],
                 ("((new Objs.reloc) _loc)#exp e\n",
                   (Gramf.mk_action
                      (fun (__fan_0 : [> Tokenf.t])  (_loc : Locf.t)  ->
                         match __fan_0 with
                         | `Uid _ -> (((new Objs.reloc) _loc)#exp e : 'exp )
                         | _ -> failwith "((new Objs.reloc) _loc)#exp e\n"))))]));
        Gramf.extend_single (pat : 'pat Gramf.t )
          ((Some (`Level "simple")),
            (None, None,
              [([`Stoken
                   (((function
                      | `Uid __fan__y when y = __fan__y -> true
                      | _ -> false)), (`App ((`Vrn "Uid"), (`Str y))),
                     "`Uid $y")],
                 ("let p = substp _loc [] e in ((new Objs.reloc) _loc)#pat p\n",
                   (Gramf.mk_action
                      (fun (__fan_0 : [> Tokenf.t])  (_loc : Locf.t)  ->
                         match __fan_0 with
                         | `Uid _ ->
                             (let p = substp _loc [] e in
                              ((new Objs.reloc) _loc)#pat p : 'pat )
                         | _ ->
                             failwith
                               "let p = substp _loc [] e in ((new Objs.reloc) _loc)#pat p\n"))))])))
   | Some (sl,e) ->
       let grammar_entry_create x = Gramf.mk x in
       let params: 'params Gramf.t = grammar_entry_create "params"
       and param: 'param Gramf.t = grammar_entry_create "param" in
       (Gramf.extend_single (exp : 'exp Gramf.t )
          ((Some (`Level "apply")),
            (None, None,
              [([`Stoken
                   (((function
                      | `Uid __fan__y when y = __fan__y -> true
                      | _ -> false)), (`App ((`Vrn "Uid"), (`Str y))),
                     "`Uid $y");
                `Sself],
                 ("let el =\n  match param with\n  | (`Par (_loc,e) : Astf.exp) -> list_of_com e []\n  | e -> [e] in\nif (List.length el) = (List.length sl)\nthen let env = List.combine sl el in ((new subst) _loc env)#exp e\nelse incorrect_number _loc el sl\n",
                   (Gramf.mk_action
                      (fun (param : 'exp)  (__fan_0 : [> Tokenf.t]) 
                         (_loc : Locf.t)  ->
                         match __fan_0 with
                         | `Uid _ ->
                             (let el =
                                match param with
                                | (`Par (_loc,e) : Astf.exp) ->
                                    list_of_com e []
                                | e -> [e] in
                              if (List.length el) = (List.length sl)
                              then
                                let env = List.combine sl el in
                                ((new subst) _loc env)#exp e
                              else incorrect_number _loc el sl : 'exp )
                         | _ ->
                             failwith
                               "let el =\n  match param with\n  | (`Par (_loc,e) : Astf.exp) -> list_of_com e []\n  | e -> [e] in\nif (List.length el) = (List.length sl)\nthen let env = List.combine sl el in ((new subst) _loc env)#exp e\nelse incorrect_number _loc el sl\n"))))]));
        Gramf.extend_single (params : 'params Gramf.t )
          (None,
            (None, None,
              [([`Slist1sep
                   ((`Snterm (Gramf.obj (param : 'param Gramf.t ))),
                     (`Skeyword ","))],
                 ("xs\n",
                   (Gramf.mk_action
                      (fun (xs : 'param list)  (_loc : Locf.t)  ->
                         (xs : 'params )))))]));
        Gramf.extend_single (param : 'param Gramf.t )
          (None,
            (None, None,
              [([`Stoken
                   (((function | `Lid _ -> true | _ -> false)),
                     (`App ((`Vrn "Lid"), `Any)), "`Lid _")],
                 ("x\n",
                   (Gramf.mk_action
                      (fun (__fan_0 : [> Tokenf.t])  (_loc : Locf.t)  ->
                         match __fan_0 with
                         | `Lid x -> (x : 'param )
                         | _ -> failwith "x\n"))))]));
        Gramf.extend_single (pat : 'pat Gramf.t )
          ((Some (`Level "simple")),
            (None, None,
              [([`Stoken
                   (((function
                      | `Uid __fan__y when y = __fan__y -> true
                      | _ -> false)), (`App ((`Vrn "Uid"), (`Str y))),
                     "`Uid $y");
                `Sself],
                 ("let pl =\n  match param with\n  | (`Par (_loc,p) : Astf.pat) -> list_of_com p []\n  | p -> [p] in\nif (List.length pl) = (List.length sl)\nthen\n  let env = List.combine sl pl in\n  let p = substp _loc env e in ((new Objs.reloc) _loc)#pat p\nelse incorrect_number _loc pl sl\n",
                   (Gramf.mk_action
                      (fun (param : 'pat)  (__fan_0 : [> Tokenf.t]) 
                         (_loc : Locf.t)  ->
                         match __fan_0 with
                         | `Uid _ ->
                             (let pl =
                                match param with
                                | (`Par (_loc,p) : Astf.pat) ->
                                    list_of_com p []
                                | p -> [p] in
                              if (List.length pl) = (List.length sl)
                              then
                                let env = List.combine sl pl in
                                let p = substp _loc env e in
                                ((new Objs.reloc) _loc)#pat p
                              else incorrect_number _loc pl sl : 'pat )
                         | _ ->
                             failwith
                               "let pl =\n  match param with\n  | (`Par (_loc,p) : Astf.pat) -> list_of_com p []\n  | p -> [p] in\nif (List.length pl) = (List.length sl)\nthen\n  let env = List.combine sl pl in\n  let p = substp _loc env e in ((new Objs.reloc) _loc)#pat p\nelse incorrect_number _loc pl sl\n"))))])))
   | None  -> ());
  defined := ((y, eo) :: (defined.contents))
let undef ~exp  ~pat  x =
  try
    (let eo = List.assoc x defined.contents in
     match eo with
     | Some ([],_) ->
         (Gramf.delete_rule exp
            [`Stoken
               (((function
                  | `Uid __fan__x when x = __fan__x -> true
                  | _ -> false)), (`App ((`Vrn "Uid"), (`Str x))), "`Uid $x")];
          Gramf.delete_rule pat
            [`Stoken
               (((function
                  | `Uid __fan__x when x = __fan__x -> true
                  | _ -> false)), (`App ((`Vrn "Uid"), (`Str x))), "`Uid $x")])
     | Some (_,_) ->
         (Gramf.delete_rule exp
            [`Stoken
               (((function
                  | `Uid __fan__x when x = __fan__x -> true
                  | _ -> false)), (`App ((`Vrn "Uid"), (`Str x))), "`Uid $x");
            `Sself];
          Gramf.delete_rule pat
            [`Stoken
               (((function
                  | `Uid __fan__x when x = __fan__x -> true
                  | _ -> false)), (`App ((`Vrn "Uid"), (`Str x))), "`Uid $x");
            `Sself])
     | None  -> ());
    defined := (List.remove x defined.contents)
  with | Not_found  -> ()
let execute_macro ~exp  ~pat  nil =
  function
  | Str i -> i
  | Def (x,eo) -> (define ~exp ~pat eo x; nil)
  | Und x -> (undef ~exp ~pat x; nil)
