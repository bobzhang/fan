open FAst

open AstLib

open LibUtil

type 'a item_or_def =  
  | Str of 'a
  | Def of string* (string list * exp) option
  | Und of string 

let defined = ref []

let substp loc (env : (string * pat) list) =
  let bad_pat _loc =
    FLoc.errorf _loc
      "this macro cannot be used in a pattern (see its definition)" in
  let rec loop (x : exp) =
    match x with
    | (`App (_loc,e1,e2) : FAst.exp) ->
        (`App (loc, (loop e1), (loop e2)) : FAst.pat )
    | (`Lid (_loc,x) : FAst.exp) ->
        (try List.assoc x env
         with | Not_found  -> (`Lid (loc, x) : FAst.pat ))
    | (`Uid (_loc,x) : FAst.exp) ->
        (try List.assoc x env
         with | Not_found  -> (`Uid (loc, x) : FAst.pat ))
    | (`Int (_loc,x) : FAst.exp) -> (`Int (loc, x) : FAst.pat )
    | (`Str (_loc,s) : FAst.exp) -> (`Str (loc, s) : FAst.pat )
    | (`Par (_loc,x) : FAst.exp) -> (`Par (loc, (loop x)) : FAst.pat )
    | (`Com (_loc,x1,x2) : FAst.exp) ->
        (`Com (loc, (loop x1), (loop x2)) : FAst.pat )
    | (`Record (_loc,bi) : FAst.exp) ->
        let rec substbi =
          function
          | (`Sem (_loc,b1,b2) : FAst.rec_exp) ->
              `Sem (_loc, (substbi b1), (substbi b2))
          | (`RecBind (_loc,i,e) : FAst.rec_exp) ->
              `RecBind (loc, i, (loop e))
          | _ -> bad_pat _loc in
        (`Record (loc, (substbi bi)) : FAst.pat )
    | _ -> bad_pat loc in
  loop

class subst loc env =
  object 
    inherit  (Objs.reloc loc) as super
    method! exp =
      function
      | (`Lid (_loc,x) : FAst.exp)|(`Uid (_loc,x) : FAst.exp) as e ->
          (try List.assoc x env with | Not_found  -> super#exp e)
      | (`App (_loc,`Uid (_,"LOCATION_OF"),`Lid (_,x)) : FAst.exp)
        |(`App (_loc,`Uid (_,"LOCATION_OF"),`Uid (_,x)) : FAst.exp) as e ->
          (try
             let loc = loc_of (List.assoc x env) in
             let (a,b,c,d,e,f,g,h) = FLoc.to_tuple loc in
             (`App
                (_loc,
                  (`Dot
                     (_loc, (`Uid (_loc, "FLoc")), (`Lid (_loc, "of_tuple")))),
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
                                  then (`Lid (_loc, "true") : FAst.exp )
                                  else (`Lid (_loc, "false") : FAst.exp ))))))))) : 
               FAst.exp )
           with | Not_found  -> super#exp e)
      | e -> super#exp e
    method! pat =
      function
      | (`Lid (_loc,x) : FAst.pat)|(`Uid (_loc,x) : FAst.pat) as p ->
          (try substp loc [] (List.assoc x env)
           with | Not_found  -> super#pat p)
      | p -> super#pat p
  end

let define ~exp  ~pat  eo y =
  let incorrect_number loc l1 l2 =
    FLoc.raise loc
      (Failure
         (Printf.sprintf "expected %d parameters; found %d" (List.length l2)
            (List.length l1))) in
  begin
    (match eo with
     | Some ([],e) ->
         begin
           Fgram.extend_single (exp : 'exp Fgram.t )
             ((Some (`Level "simple")),
               (None, None,
                 [([`Stoken
                      (((function
                         | `Uid __fan__y when y = __fan__y -> true
                         | _ -> false)), (`App ((`Vrn "Uid"), (`Str y))),
                        "`Uid $y")],
                    ("((new Objs.reloc) _loc)#exp e\n",
                      (Fgram.mk_action
                         (fun (__fan_0 : [> FToken.t])  (_loc : FLoc.t)  ->
                            match __fan_0 with
                            | `Uid _ ->
                                (((new Objs.reloc) _loc)#exp e : 'exp )
                            | _ -> failwith "((new Objs.reloc) _loc)#exp e\n"))))]));
           Fgram.extend_single (pat : 'pat Fgram.t )
             ((Some (`Level "simple")),
               (None, None,
                 [([`Stoken
                      (((function
                         | `Uid __fan__y when y = __fan__y -> true
                         | _ -> false)), (`App ((`Vrn "Uid"), (`Str y))),
                        "`Uid $y")],
                    ("let p = substp _loc [] e in ((new Objs.reloc) _loc)#pat p\n",
                      (Fgram.mk_action
                         (fun (__fan_0 : [> FToken.t])  (_loc : FLoc.t)  ->
                            match __fan_0 with
                            | `Uid _ ->
                                (let p = substp _loc [] e in
                                 ((new Objs.reloc) _loc)#pat p : 'pat )
                            | _ ->
                                failwith
                                  "let p = substp _loc [] e in ((new Objs.reloc) _loc)#pat p\n"))))]))
         end
     | Some (sl,e) ->
         let grammar_entry_create x = Fgram.mk x in
         let params: 'params Fgram.t = grammar_entry_create "params"
         and param: 'param Fgram.t = grammar_entry_create "param" in
         begin
           Fgram.extend_single (exp : 'exp Fgram.t )
             ((Some (`Level "apply")),
               (None, None,
                 [([`Stoken
                      (((function
                         | `Uid __fan__y when y = __fan__y -> true
                         | _ -> false)), (`App ((`Vrn "Uid"), (`Str y))),
                        "`Uid $y");
                   `Sself],
                    ("let el =\n  match param with\n  | (`Par (_loc,e) : FAst.exp) -> list_of_com e []\n  | e -> [e] in\nif (List.length el) = (List.length sl)\nthen let env = List.combine sl el in ((new subst) _loc env)#exp e\nelse incorrect_number _loc el sl\n",
                      (Fgram.mk_action
                         (fun (param : 'exp)  (__fan_0 : [> FToken.t]) 
                            (_loc : FLoc.t)  ->
                            match __fan_0 with
                            | `Uid _ ->
                                (let el =
                                   match param with
                                   | (`Par (_loc,e) : FAst.exp) ->
                                       list_of_com e []
                                   | e -> [e] in
                                 if (List.length el) = (List.length sl)
                                 then
                                   let env = List.combine sl el in
                                   ((new subst) _loc env)#exp e
                                 else incorrect_number _loc el sl : 'exp )
                            | _ ->
                                failwith
                                  "let el =\n  match param with\n  | (`Par (_loc,e) : FAst.exp) -> list_of_com e []\n  | e -> [e] in\nif (List.length el) = (List.length sl)\nthen let env = List.combine sl el in ((new subst) _loc env)#exp e\nelse incorrect_number _loc el sl\n"))))]));
           Fgram.extend_single (params : 'params Fgram.t )
             (None,
               (None, None,
                 [([`Slist1sep
                      ((`Snterm (Fgram.obj (param : 'param Fgram.t ))),
                        (`Skeyword ","))],
                    ("xs\n",
                      (Fgram.mk_action
                         (fun (xs : 'param list)  (_loc : FLoc.t)  ->
                            (xs : 'params )))))]));
           Fgram.extend_single (param : 'param Fgram.t )
             (None,
               (None, None,
                 [([`Stoken
                      (((function | `Lid _ -> true | _ -> false)),
                        (`App ((`Vrn "Lid"), `Any)), "`Lid _")],
                    ("x\n",
                      (Fgram.mk_action
                         (fun (__fan_0 : [> FToken.t])  (_loc : FLoc.t)  ->
                            match __fan_0 with
                            | `Lid x -> (x : 'param )
                            | _ -> failwith "x\n"))))]));
           Fgram.extend_single (pat : 'pat Fgram.t )
             ((Some (`Level "simple")),
               (None, None,
                 [([`Stoken
                      (((function
                         | `Uid __fan__y when y = __fan__y -> true
                         | _ -> false)), (`App ((`Vrn "Uid"), (`Str y))),
                        "`Uid $y");
                   `Sself],
                    ("let pl =\n  match param with\n  | (`Par (_loc,p) : FAst.pat) -> list_of_com p []\n  | p -> [p] in\nif (List.length pl) = (List.length sl)\nthen\n  let env = List.combine sl pl in\n  let p = substp _loc env e in ((new Objs.reloc) _loc)#pat p\nelse incorrect_number _loc pl sl\n",
                      (Fgram.mk_action
                         (fun (param : 'pat)  (__fan_0 : [> FToken.t]) 
                            (_loc : FLoc.t)  ->
                            match __fan_0 with
                            | `Uid _ ->
                                (let pl =
                                   match param with
                                   | (`Par (_loc,p) : FAst.pat) ->
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
                                  "let pl =\n  match param with\n  | (`Par (_loc,p) : FAst.pat) -> list_of_com p []\n  | p -> [p] in\nif (List.length pl) = (List.length sl)\nthen\n  let env = List.combine sl pl in\n  let p = substp _loc env e in ((new Objs.reloc) _loc)#pat p\nelse incorrect_number _loc pl sl\n"))))]))
         end
     | None  -> ());
    defined := ((y, eo) :: (defined.contents))
  end

let undef ~exp  ~pat  x =
  try
    begin
      (let eo = List.assoc x defined.contents in
       match eo with
       | Some ([],_) ->
           begin
             Fgram.delete_rule exp
               [`Stoken
                  (((function
                     | `Uid __fan__x when x = __fan__x -> true
                     | _ -> false)), (`App ((`Vrn "Uid"), (`Str x))),
                    "`Uid $x")];
             Fgram.delete_rule pat
               [`Stoken
                  (((function
                     | `Uid __fan__x when x = __fan__x -> true
                     | _ -> false)), (`App ((`Vrn "Uid"), (`Str x))),
                    "`Uid $x")]
           end
       | Some (_,_) ->
           begin
             Fgram.delete_rule exp
               [`Stoken
                  (((function
                     | `Uid __fan__x when x = __fan__x -> true
                     | _ -> false)), (`App ((`Vrn "Uid"), (`Str x))),
                    "`Uid $x");
               `Sself];
             Fgram.delete_rule pat
               [`Stoken
                  (((function
                     | `Uid __fan__x when x = __fan__x -> true
                     | _ -> false)), (`App ((`Vrn "Uid"), (`Str x))),
                    "`Uid $x");
               `Sself]
           end
       | None  -> ());
      defined := (List.remove x defined.contents)
    end
  with | Not_found  -> ()

let execute_macro ~exp  ~pat  nil =
  function
  | Str i -> i
  | Def (x,eo) -> begin define ~exp ~pat eo x; nil end
  | Und x -> begin undef ~exp ~pat x; nil end