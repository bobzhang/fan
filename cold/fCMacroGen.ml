open FAst

open AstLib

open LibUtil

type 'a item_or_def =  
  | Str of 'a
  | Def of string* (string list * exp) option
  | Und of string 

let defined = ref []

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
                    ("let p = Exp.substp _loc [] e in ((new Objs.reloc) _loc)#pat p\n",
                      (Fgram.mk_action
                         (fun (__fan_0 : [> FToken.t])  (_loc : FLoc.t)  ->
                            match __fan_0 with
                            | `Uid _ ->
                                (let p = Exp.substp _loc [] e in
                                 ((new Objs.reloc) _loc)#pat p : 'pat )
                            | _ ->
                                failwith
                                  "let p = Exp.substp _loc [] e in ((new Objs.reloc) _loc)#pat p\n"))))]))
         end
     | Some (sl,e) ->
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
                    ("let el =\n  match param with\n  | (`Par (_loc,e) : FAst.exp) -> list_of_com e []\n  | e -> [e] in\nif (List.length el) = (List.length sl)\nthen let env = List.combine sl el in ((new Exp.subst) _loc env)#exp e\nelse incorrect_number _loc el sl\n",
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
                                   ((new Exp.subst) _loc env)#exp e
                                 else incorrect_number _loc el sl : 'exp )
                            | _ ->
                                failwith
                                  "let el =\n  match param with\n  | (`Par (_loc,e) : FAst.exp) -> list_of_com e []\n  | e -> [e] in\nif (List.length el) = (List.length sl)\nthen let env = List.combine sl el in ((new Exp.subst) _loc env)#exp e\nelse incorrect_number _loc el sl\n"))))]));
           Fgram.extend_single (pat : 'pat Fgram.t )
             ((Some (`Level "simple")),
               (None, None,
                 [([`Stoken
                      (((function
                         | `Uid __fan__y when y = __fan__y -> true
                         | _ -> false)), (`App ((`Vrn "Uid"), (`Str y))),
                        "`Uid $y");
                   `Sself],
                    ("let pl =\n  match param with\n  | (`Par (_loc,p) : FAst.pat) -> list_of_com p []\n  | p -> [p] in\nif (List.length pl) = (List.length sl)\nthen\n  let env = List.combine sl pl in\n  let p = Exp.substp _loc env e in ((new Objs.reloc) _loc)#pat p\nelse incorrect_number _loc pl sl\n",
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
                                   let p = Exp.substp _loc env e in
                                   ((new Objs.reloc) _loc)#pat p
                                 else incorrect_number _loc pl sl : 'pat )
                            | _ ->
                                failwith
                                  "let pl =\n  match param with\n  | (`Par (_loc,p) : FAst.pat) -> list_of_com p []\n  | p -> [p] in\nif (List.length pl) = (List.length sl)\nthen\n  let env = List.combine sl pl in\n  let p = Exp.substp _loc env e in ((new Objs.reloc) _loc)#pat p\nelse incorrect_number _loc pl sl\n"))))]))
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