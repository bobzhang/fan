open FAst
open Util
type key = string 
let inject_exp_tbl: (key,exp) Hashtbl.t = Hashtbl.create 40
let inject_stru_tbl: (key,stru) Hashtbl.t = Hashtbl.create 40
let inject_clfield_tbl: (key,clfield) Hashtbl.t = Hashtbl.create 40
let register_inject_exp (k,f) = Hashtbl.replace inject_exp_tbl k f
let register_inject_stru (k,f) = Hashtbl.replace inject_stru_tbl k f
let register_inject_clfield (k,f) = Hashtbl.replace inject_clfield_tbl k f
let inject_exp = Gramf.mk "inject_exp"
let inject_stru = Gramf.mk "inject_stru"
let inject_clfield = Gramf.mk "inject_clfield"
let _ =
  Gramf.extend_single (inject_exp : 'inject_exp Gramf.t )
    (None,
      (None, None,
        [([`Token
             (((function | `Lid _ -> true | _ -> false)), (3802919, `Any),
               "`Lid x")],
           ("try Hashtbl.find inject_exp_tbl x\nwith | Not_found  -> failwithf \"inject.exp %s not found\" x\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Lid ({ txt = x;_} : Tokenf.txt) ->
                       ((try Hashtbl.find inject_exp_tbl x
                         with
                         | Not_found  ->
                             failwithf "inject.exp %s not found" x) : 
                       'inject_exp )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))))]));
  Gramf.extend_single (inject_stru : 'inject_stru Gramf.t )
    (None,
      (None, None,
        [([`Token
             (((function | `Lid _ -> true | _ -> false)), (3802919, `Any),
               "`Lid x")],
           ("try Hashtbl.find inject_stru_tbl x\nwith | Not_found  -> failwithf \"inject.exp %s not found\" x\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Lid ({ txt = x;_} : Tokenf.txt) ->
                       ((try Hashtbl.find inject_stru_tbl x
                         with
                         | Not_found  ->
                             failwithf "inject.exp %s not found" x) : 
                       'inject_stru )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))))]));
  Gramf.extend_single (inject_clfield : 'inject_clfield Gramf.t )
    (None,
      (None, None,
        [([`Token
             (((function | `Lid _ -> true | _ -> false)), (3802919, `Any),
               "`Lid x")],
           ("try Hashtbl.find inject_clfield_tbl x\nwith | Not_found  -> failwithf \"inject.exp %s not found\" x\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Lid ({ txt = x;_} : Tokenf.txt) ->
                       ((try Hashtbl.find inject_clfield_tbl x
                         with
                         | Not_found  ->
                             failwithf "inject.exp %s not found" x) : 
                       'inject_clfield )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))))]))
let _ =
  let open Ast_quotation in
    let d = Ns.inject in
    of_exp ~name:(d, "exp") ~entry:inject_exp ();
    of_stru ~name:(d, "stru") ~entry:inject_stru ();
    of_clfield ~name:(d, "clfield") ~entry:inject_clfield ()
