open Ast
module Ast = Camlp4Ast
open Lib
open LibUtil
type 'a item_or_def =  
  | Str of 'a
  | Def of string* (string list* Ast.expr) option
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
       (Gram.extend (expr : 'expr Gram.t )
          ((Some (`Level "simple")),
            [(None, None,
               [([`Stoken
                    (((function
                       | `UID __fan__x when x = __fan__x -> true
                       | _ -> false)), (`Antiquot, "`UID __fan__x"))],
                  (Gram.mk_action
                     (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                        match __fan_0 with
                        | `UID _ -> (((new Ast.reloc) _loc)#expr e : 'expr )
                        | _ -> assert false)))])]);
        Gram.extend (patt : 'patt Gram.t )
          ((Some (`Level "simple")),
            [(None, None,
               [([`Stoken
                    (((function
                       | `UID __fan__x when x = __fan__x -> true
                       | _ -> false)), (`Antiquot, "`UID __fan__x"))],
                  (Gram.mk_action
                     (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                        match __fan_0 with
                        | `UID _ ->
                            (let p = Expr.substp _loc [] e in
                             ((new Ast.reloc) _loc)#patt p : 'patt )
                        | _ -> assert false)))])]))
   | Some (sl,e) ->
       (Gram.extend (expr : 'expr Gram.t )
          ((Some (`Level "apply")),
            [(None, None,
               [([`Stoken
                    (((function
                       | `UID __fan__x when x = __fan__x -> true
                       | _ -> false)), (`Antiquot, "`UID __fan__x"));
                 `Sself],
                  (Gram.mk_action
                     (fun (param : 'expr)  (__fan_0 : [> FanToken.t]) 
                        (_loc : FanLoc.t)  ->
                        match __fan_0 with
                        | `UID _ ->
                            (let el =
                               match param with
                               | ExTup (_loc,e) -> Ast.list_of_expr e []
                               | e -> [e] in
                             if (List.length el) = (List.length sl)
                             then
                               let env = List.combine sl el in
                               ((new Expr.subst) _loc env)#expr e
                             else incorrect_number _loc el sl : 'expr )
                        | _ -> assert false)))])]);
        Gram.extend (patt : 'patt Gram.t )
          ((Some (`Level "simple")),
            [(None, None,
               [([`Stoken
                    (((function
                       | `UID __fan__x when x = __fan__x -> true
                       | _ -> false)), (`Antiquot, "`UID __fan__x"));
                 `Sself],
                  (Gram.mk_action
                     (fun (param : 'patt)  (__fan_0 : [> FanToken.t]) 
                        (_loc : FanLoc.t)  ->
                        match __fan_0 with
                        | `UID _ ->
                            (let pl =
                               match param with
                               | PaTup (_loc,p) -> Ast.list_of_patt p []
                               | p -> [p] in
                             if (List.length pl) = (List.length sl)
                             then
                               let env = List.combine sl pl in
                               let p = Expr.substp _loc env e in
                               ((new Ast.reloc) _loc)#patt p
                             else incorrect_number _loc pl sl : 'patt )
                        | _ -> assert false)))])]))
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
                  | `UID __fan__x when x = __fan__x -> true
                  | _ -> false)), (`Antiquot, "`UID __fan__x"))];
          Gram.delete_rule patt
            [`Stoken
               (((function
                  | `UID __fan__x when x = __fan__x -> true
                  | _ -> false)), (`Antiquot, "`UID __fan__x"))])
     | Some (_,_) ->
         (Gram.delete_rule expr
            [`Stoken
               (((function
                  | `UID __fan__x when x = __fan__x -> true
                  | _ -> false)), (`Antiquot, "`UID __fan__x"));
            `Sself];
          Gram.delete_rule patt
            [`Stoken
               (((function
                  | `UID __fan__x when x = __fan__x -> true
                  | _ -> false)), (`Antiquot, "`UID __fan__x"));
            `Sself])
     | None  -> ());
    defined := (List.remove x defined.contents)
  with | Not_found  -> ()
let parse_def ~expr  ~patt  s =
  match Gram.parse_string expr ~loc:(FanLoc.mk "<command line>") s with
  | ExId (_loc,IdUid (_,n)) -> define ~expr ~patt None n
  | ExApp (_loc,ExApp (_,ExId (_,IdLid (_,"=")),ExId (_,IdUid (_,n))),e) ->
      define ~expr ~patt (Some ([], e)) n
  | _ -> invalid_arg s
let include_dirs = ref []
let add_include_dir str =
  if str <> ""
  then
    let str =
      if (str.[(String.length str) - 1]) = '/' then str else str ^ "/" in
    include_dirs := (include_dirs.contents @ [str])
  else ()
let parse_include_file entry =
  let dir_ok file dir = Sys.file_exists (dir ^ file) in
  fun file  ->
    let file =
      try (List.find (dir_ok file) (include_dirs.contents @ ["./"])) ^ file
      with | Not_found  -> file in
    let ch = open_in file in
    let st = XStream.of_channel ch in Gram.parse entry (FanLoc.mk file) st
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