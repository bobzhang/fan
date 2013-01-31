open Ast
open Lib
open LibUtil
type 'a item_or_def =  
  | Str of 'a
  | Def of string* (string list* expr) option
  | Und of string
  | ITE of bool* 'a item_or_def list* 'a item_or_def list
  | Lazy of 'a Lazy.t 
let defined = ref []
let is_defined i = List.mem_assoc i defined.contents
let incorrect_number loc l1 l2 =
  FanLoc.raise loc
    (Failure
       (Printf.sprintf "expected %d parameters; found %d" (List.length l2)
          (List.length l1)))
let define ~expr  ~patt  eo x =
  (match eo with
   | Some ([],e) ->
       (Gram.extend_single (expr : 'expr Gram.t )
          ((Some (`Level "simple")),
            (None, None,
              [([`Stoken
                   (((function
                      | `Uid __fan__x when x = __fan__x -> true
                      | _ -> false)), (`Antiquot, "`Uid __fan__x"))],
                 ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Uid _ -> (((new FanAst.reloc) _loc)#expr e : 'expr )\n     | _ -> failwith \"((new FanAst.reloc) _loc)#expr e\n\")\n",
                   (Gram.mk_action
                      (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                         match __fan_0 with
                         | `Uid _ ->
                             (((new FanAst.reloc) _loc)#expr e : 'expr )
                         | _ -> failwith "((new FanAst.reloc) _loc)#expr e\n"))))]));
        Gram.extend_single (patt : 'patt Gram.t )
          ((Some (`Level "simple")),
            (None, None,
              [([`Stoken
                   (((function
                      | `Uid __fan__x when x = __fan__x -> true
                      | _ -> false)), (`Antiquot, "`Uid __fan__x"))],
                 ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Uid _ ->\n         (let p = Expr.substp _loc [] e in ((new FanAst.reloc) _loc)#patt p : \n         'patt )\n     | _ ->\n         failwith\n           \"let p = Expr.substp _loc [] e in ((new FanAst.reloc) _loc)#patt p\n\")\n",
                   (Gram.mk_action
                      (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                         match __fan_0 with
                         | `Uid _ ->
                             (let p = Expr.substp _loc [] e in
                              ((new FanAst.reloc) _loc)#patt p : 'patt )
                         | _ ->
                             failwith
                               "let p = Expr.substp _loc [] e in ((new FanAst.reloc) _loc)#patt p\n"))))])))
   | Some (sl,e) ->
       (Gram.extend_single (expr : 'expr Gram.t )
          ((Some (`Level "apply")),
            (None, None,
              [([`Stoken
                   (((function
                      | `Uid __fan__x when x = __fan__x -> true
                      | _ -> false)), (`Antiquot, "`Uid __fan__x"));
                `Sself],
                 ("Gram.mk_action\n  (fun (param : 'expr)  (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Uid _ ->\n         (let el =\n            match param with\n            | `Tup (_loc,e) -> FanAst.list_of_com' e []\n            | e -> [e] in\n          if (List.length el) = (List.length sl)\n          then\n            let env = List.combine sl el in\n            ((new Expr.subst) _loc env)#expr e\n          else incorrect_number _loc el sl : 'expr )\n     | _ ->\n         failwith\n           \"let el =\n  match param with | `Tup (_loc,e) -> FanAst.list_of_com' e [] | e -> [e] in\nif (List.length el) = (List.length sl)\nthen let env = List.combine sl el in ((new Expr.subst) _loc env)#expr e\nelse incorrect_number _loc el sl\n\")\n",
                   (Gram.mk_action
                      (fun (param : 'expr)  (__fan_0 : [> FanToken.t]) 
                         (_loc : FanLoc.t)  ->
                         match __fan_0 with
                         | `Uid _ ->
                             (let el =
                                match param with
                                | `Tup (_loc,e) -> FanAst.list_of_com' e []
                                | e -> [e] in
                              if (List.length el) = (List.length sl)
                              then
                                let env = List.combine sl el in
                                ((new Expr.subst) _loc env)#expr e
                              else incorrect_number _loc el sl : 'expr )
                         | _ ->
                             failwith
                               "let el =\n  match param with | `Tup (_loc,e) -> FanAst.list_of_com' e [] | e -> [e] in\nif (List.length el) = (List.length sl)\nthen let env = List.combine sl el in ((new Expr.subst) _loc env)#expr e\nelse incorrect_number _loc el sl\n"))))]));
        Gram.extend_single (patt : 'patt Gram.t )
          ((Some (`Level "simple")),
            (None, None,
              [([`Stoken
                   (((function
                      | `Uid __fan__x when x = __fan__x -> true
                      | _ -> false)), (`Antiquot, "`Uid __fan__x"));
                `Sself],
                 ("Gram.mk_action\n  (fun (param : 'patt)  (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Uid _ ->\n         (let pl =\n            match param with\n            | `Tup (_loc,p) -> FanAst.list_of_com' p []\n            | p -> [p] in\n          if (List.length pl) = (List.length sl)\n          then\n            let env = List.combine sl pl in\n            let p = Expr.substp _loc env e in\n            ((new FanAst.reloc) _loc)#patt p\n          else incorrect_number _loc pl sl : 'patt )\n     | _ ->\n         failwith\n           \"let pl =\n  match param with | `Tup (_loc,p) -> FanAst.list_of_com' p [] | p -> [p] in\nif (List.length pl) = (List.length sl)\nthen\n  let env = List.combine sl pl in\n  let p = Expr.substp _loc env e in ((new FanAst.reloc) _loc)#patt p\nelse incorrect_number _loc pl sl\n\")\n",
                   (Gram.mk_action
                      (fun (param : 'patt)  (__fan_0 : [> FanToken.t]) 
                         (_loc : FanLoc.t)  ->
                         match __fan_0 with
                         | `Uid _ ->
                             (let pl =
                                match param with
                                | `Tup (_loc,p) -> FanAst.list_of_com' p []
                                | p -> [p] in
                              if (List.length pl) = (List.length sl)
                              then
                                let env = List.combine sl pl in
                                let p = Expr.substp _loc env e in
                                ((new FanAst.reloc) _loc)#patt p
                              else incorrect_number _loc pl sl : 'patt )
                         | _ ->
                             failwith
                               "let pl =\n  match param with | `Tup (_loc,p) -> FanAst.list_of_com' p [] | p -> [p] in\nif (List.length pl) = (List.length sl)\nthen\n  let env = List.combine sl pl in\n  let p = Expr.substp _loc env e in ((new FanAst.reloc) _loc)#patt p\nelse incorrect_number _loc pl sl\n"))))])))
   | None  -> ());
  defined := ((x, eo) :: (defined.contents))
let undef ~expr  ~patt  x =
  try
    (let eo = List.assoc x defined.contents in
     match eo with
     | Some ([],_) ->
         (Gram.delete_rule expr
            [`Stoken
               (((function
                  | `Uid __fan__x when x = __fan__x -> true
                  | _ -> false)), (`Antiquot, "`Uid __fan__x"))];
          Gram.delete_rule patt
            [`Stoken
               (((function
                  | `Uid __fan__x when x = __fan__x -> true
                  | _ -> false)), (`Antiquot, "`Uid __fan__x"))])
     | Some (_,_) ->
         (Gram.delete_rule expr
            [`Stoken
               (((function
                  | `Uid __fan__x when x = __fan__x -> true
                  | _ -> false)), (`Antiquot, "`Uid __fan__x"));
            `Sself];
          Gram.delete_rule patt
            [`Stoken
               (((function
                  | `Uid __fan__x when x = __fan__x -> true
                  | _ -> false)), (`Antiquot, "`Uid __fan__x"));
            `Sself])
     | None  -> ());
    defined := (List.remove x defined.contents)
  with | Not_found  -> ()
let parse_def ~expr  ~patt  s =
  match Gram.parse_string expr ~loc:(FanLoc.mk "<command line>") s with
  | `Id (_loc,`Uid (_,n)) -> define ~expr ~patt None n
  | `App (_loc,`App (_,`Id (_,`Lid (_,"=")),`Id (_,`Uid (_,n))),e) ->
      define ~expr ~patt (Some ([], e)) n
  | _ -> invalid_arg s
let rec execute_macro ~expr  ~patt  nil cons =
  function
  | Str i -> i
  | Def (x,eo) -> (define ~expr ~patt eo x; nil)
  | Und x -> (undef ~expr ~patt x; nil)
  | ITE (b,l1,l2) ->
      execute_macro_list ~expr ~patt nil cons (if b then l1 else l2)
  | Lazy l -> Lazy.force l
and execute_macro_list ~expr  ~patt  nil cons =
  function
  | [] -> nil
  | hd::tl ->
      let il1 = execute_macro ~expr ~patt nil cons hd in
      let il2 = execute_macro_list ~expr ~patt nil cons tl in cons il1 il2
let stack = Stack.create ()
let make_ITE_result st1 st2 =
  let test = Stack.pop stack in ITE (test, st1, st2)
type branch =  
  | Then
  | Else 
let execute_macro_if_active_branch ~expr  ~patt  _loc nil cons branch
  macro_def =
  let _ = Format.eprintf "execute_macro_if_active_branch@." in
  let test = Stack.top stack in
  let item =
    if (test && (branch = Then)) || ((not test) && (branch = Else))
    then
      let res = execute_macro ~expr ~patt nil cons macro_def in
      (Format.eprintf "executing branch %s@."
         (if branch = Then then "Then" else "Else");
       res)
    else nil in
  Str item