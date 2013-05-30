open FAst

open AstLib

open LibUtil

type 'a item_or_def =  
  | Str of 'a
  | Def of string* (string list * exp) option
  | Und of string
  | ITE of bool* 'a item_or_def list* 'a item_or_def list
  | Lazy of 'a Lazy.t 

let defined = ref []

let is_defined i = List.mem_assoc i defined.contents

let incorrect_number loc l1 l2 =
  FLoc.raise loc
    (Failure
       (Printf.sprintf "expected %d parameters; found %d" (List.length l2)
          (List.length l1)))

let define ~exp  ~pat  eo x =
  begin
    (match eo with
     | Some ([],e) ->
         begin
           Fgram.extend_single (exp : 'exp Fgram.t )
             ((Some (`Level "simple")),
               (None, None,
                 [([`Stoken
                      (((function
                         | `Uid __fan__x when x = __fan__x -> true
                         | _ -> false)), (`Antiquot, "`Uid __fan__x"))],
                    ("Fgram.mk_action\n  (fun (__fan_0 : [> FToken.t])  (_loc : FLoc.t)  ->\n     match __fan_0 with\n     | `Uid _ -> (((new Objs.reloc) _loc)#exp e : 'exp )\n     | _ -> failwith \"((new Objs.reloc) _loc)#exp e\n\")\n",
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
                         | `Uid __fan__x when x = __fan__x -> true
                         | _ -> false)), (`Antiquot, "`Uid __fan__x"))],
                    ("Fgram.mk_action\n  (fun (__fan_0 : [> FToken.t])  (_loc : FLoc.t)  ->\n     match __fan_0 with\n     | `Uid _ ->\n         (let p = Exp.substp _loc [] e in ((new Objs.reloc) _loc)#pat p : \n         'pat )\n     | _ ->\n         failwith\n           \"let p = Exp.substp _loc [] e in ((new Objs.reloc) _loc)#pat p\n\")\n",
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
                         | `Uid __fan__x when x = __fan__x -> true
                         | _ -> false)), (`Antiquot, "`Uid __fan__x"));
                   `Sself],
                    ("Fgram.mk_action\n  (fun (param : 'exp)  (__fan_0 : [> FToken.t])  (_loc : FLoc.t)  ->\n     match __fan_0 with\n     | `Uid _ ->\n         (let el =\n            match param with\n            | (`Par (_loc,e) : FAst.exp) -> list_of_com e []\n            | e -> [e] in\n          if (List.length el) = (List.length sl)\n          then\n            let env = List.combine sl el in ((new Exp.subst) _loc env)#exp e\n          else incorrect_number _loc el sl : 'exp )\n     | _ ->\n         failwith\n           \"let el =\n  match param with\n  | (`Par (_loc,e) : FAst.exp) -> list_of_com e []\n  | e -> [e] in\nif (List.length el) = (List.length sl)\nthen let env = List.combine sl el in ((new Exp.subst) _loc env)#exp e\nelse incorrect_number _loc el sl\n\")\n",
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
                         | `Uid __fan__x when x = __fan__x -> true
                         | _ -> false)), (`Antiquot, "`Uid __fan__x"));
                   `Sself],
                    ("Fgram.mk_action\n  (fun (param : 'pat)  (__fan_0 : [> FToken.t])  (_loc : FLoc.t)  ->\n     match __fan_0 with\n     | `Uid _ ->\n         (let pl =\n            match param with\n            | (`Par (_loc,p) : FAst.pat) -> list_of_com p []\n            | p -> [p] in\n          if (List.length pl) = (List.length sl)\n          then\n            let env = List.combine sl pl in\n            let p = Exp.substp _loc env e in ((new Objs.reloc) _loc)#pat p\n          else incorrect_number _loc pl sl : 'pat )\n     | _ ->\n         failwith\n           \"let pl =\n  match param with\n  | (`Par (_loc,p) : FAst.pat) -> list_of_com p []\n  | p -> [p] in\nif (List.length pl) = (List.length sl)\nthen\n  let env = List.combine sl pl in\n  let p = Exp.substp _loc env e in ((new Objs.reloc) _loc)#pat p\nelse incorrect_number _loc pl sl\n\")\n",
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
    defined := ((x, eo) :: (defined.contents))
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
                     | _ -> false)), (`Antiquot, "`Uid __fan__x"))];
             Fgram.delete_rule pat
               [`Stoken
                  (((function
                     | `Uid __fan__x when x = __fan__x -> true
                     | _ -> false)), (`Antiquot, "`Uid __fan__x"))]
           end
       | Some (_,_) ->
           begin
             Fgram.delete_rule exp
               [`Stoken
                  (((function
                     | `Uid __fan__x when x = __fan__x -> true
                     | _ -> false)), (`Antiquot, "`Uid __fan__x"));
               `Sself];
             Fgram.delete_rule pat
               [`Stoken
                  (((function
                     | `Uid __fan__x when x = __fan__x -> true
                     | _ -> false)), (`Antiquot, "`Uid __fan__x"));
               `Sself]
           end
       | None  -> ());
      defined := (List.remove x defined.contents)
    end
  with | Not_found  -> ()

let parse_def ~exp  ~pat  s =
  match Fgram.parse_string exp ~loc:(FLoc.mk "<command line>") s with
  | (`Uid (_loc,n) : FAst.exp) -> define ~exp ~pat None n
  | (`App (_loc,`App (_,`Lid (_,"="),`Uid (_,n)),e) : FAst.exp) ->
      define ~exp ~pat (Some ([], e)) n
  | _ -> invalid_arg s

let rec execute_macro ~exp  ~pat  nil cons =
  function
  | Str i -> i
  | Def (x,eo) -> begin define ~exp ~pat eo x; nil end
  | Und x -> begin undef ~exp ~pat x; nil end
  | ITE (b,l1,l2) ->
      execute_macro_list ~exp ~pat nil cons (if b then l1 else l2)
  | Lazy l -> Lazy.force l
and execute_macro_list ~exp  ~pat  nil cons =
  function
  | [] -> nil
  | hd::tl ->
      let il1 = execute_macro ~exp ~pat nil cons hd in
      let il2 = execute_macro_list ~exp ~pat nil cons tl in cons il1 il2

let stack = Stack.create ()

let make_ITE_result st1 st2 =
  let test = Stack.pop stack in ITE (test, st1, st2)

type branch =  
  | Then
  | Else 

let execute_macro_if_active_branch ~exp  ~pat  _loc nil cons branch macro_def
  =
  let _ = Format.eprintf "execute_macro_if_active_branch@." in
  let test = Stack.top stack in
  let item =
    if (test && (branch = Then)) || ((not test) && (branch = Else))
    then
      let res = execute_macro ~exp ~pat nil cons macro_def in
      begin
        Format.eprintf "executing branch %s@."
          (if branch = Then then "Then" else "Else");
        res
      end
    else nil in
  Str item