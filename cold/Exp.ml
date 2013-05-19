open Ast

open AstLib

open LibUtil

open Basic

open FanUtil

let substp loc env =
  let bad_pat _loc =
    FanLoc.errorf _loc
      "this macro cannot be used in a pattern (see its definition)" in
  let rec loop (x : exp) =
    match x with
    | (`App (_loc,e1,e2) : Ast.exp) ->
        (`App (loc, (loop e1), (loop e2)) : Ast.pat )
    | (`Lid (_loc,x) : Ast.exp) ->
        (try List.assoc x env with | Not_found  -> (`Lid (loc, x) : Ast.pat ))
    | (`Uid (_loc,x) : Ast.exp) ->
        (try List.assoc x env with | Not_found  -> (`Uid (loc, x) : Ast.pat ))
    | (`Int (_loc,x) : Ast.exp) -> (`Int (loc, x) : Ast.pat )
    | (`Str (_loc,s) : Ast.exp) -> (`Str (loc, s) : Ast.pat )
    | (`Par (_loc,x) : Ast.exp) -> (`Par (loc, (loop x)) : Ast.pat )
    | (`Com (_loc,x1,x2) : Ast.exp) ->
        (`Com (loc, (loop x1), (loop x2)) : Ast.pat )
    | (`Record (_loc,bi) : Ast.exp) ->
        let rec substbi =
          function
          | (`Sem (_loc,b1,b2) : Ast.rec_exp) ->
              `Sem (_loc, (substbi b1), (substbi b2))
          | (`RecBind (_loc,i,e) : Ast.rec_exp) ->
              `RecBind (loc, i, (loop e))
          | _ -> bad_pat _loc in
        (`Record (loc, (substbi bi)) : Ast.pat )
    | _ -> bad_pat loc in
  loop

class subst loc env =
  object 
    inherit  (Objs.reloc loc) as super
    method! exp =
      function
      | (`Lid (_loc,x) : Ast.exp)|(`Uid (_loc,x) : Ast.exp) as e ->
          (try List.assoc x env with | Not_found  -> super#exp e)
      | (`App (_loc,`Uid (_,"LOCATION_OF"),`Lid (_,x)) : Ast.exp)
        |(`App (_loc,`Uid (_,"LOCATION_OF"),`Uid (_,x)) : Ast.exp) as e ->
          (try
             let loc = loc_of (List.assoc x env) in
             let (a,b,c,d,e,f,g,h) = FanLoc.to_tuple loc in
             (`App
                (_loc,
                  (`Dot
                     (_loc, (`Uid (_loc, "FanLoc")),
                       (`Lid (_loc, "of_tuple")))),
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
                                  then (`Lid (_loc, "true") : Ast.exp )
                                  else (`Lid (_loc, "false") : Ast.exp ))))))))) : 
               Ast.exp )
           with | Not_found  -> super#exp e)
      | e -> super#exp e
    method! pat =
      function
      | (`Lid (_loc,x) : Ast.pat)|(`Uid (_loc,x) : Ast.pat) as p ->
          (try substp loc [] (List.assoc x env)
           with | Not_found  -> super#pat p)
      | p -> super#pat p
  end

class type antiquot_filter
  =
  object 
    inherit Objs.map
    method get_captured_variables : (exp * exp) list
    method clear_captured_variables : unit
  end

let capture_antiquot: antiquot_filter =
  object 
    inherit  Objs.map as super
    val mutable constraints = []
    method! pat =
      function
      | `Ant (_loc,s) ->
          (match s with
           | { content = code;_} ->
               let cons: Ast.exp = `Lid (_loc, code) in
               let code' = "__fan__" ^ code in
               let cons': Ast.exp = `Lid (_loc, code') in
               let () = constraints <- (cons, cons') :: constraints in
               (`Lid (_loc, code') : Ast.pat ))
      | p -> super#pat p
    method get_captured_variables = constraints
    method clear_captured_variables = constraints <- []
  end

let filter_pat_with_captured_variables pat =
  capture_antiquot#clear_captured_variables;
  (let pat = capture_antiquot#pat pat in
   let constraints = capture_antiquot#get_captured_variables in
   (pat, constraints))

let fun_args _loc args body =
  if args = []
  then (`Fun (_loc, (`Case (_loc, (`Uid (_loc, "()")), body))) : Ast.exp )
  else
    List.fold_right
      (fun arg  body  -> (`Fun (_loc, (`Case (_loc, arg, body))) : Ast.exp ))
      args body

let _loc = FanLoc.ghost

let mk_record label_exps =
  (let rec_exps =
     List.map
       (fun (label,exp)  ->
          (`RecBind (_loc, (`Lid (_loc, label)), exp) : Ast.rec_exp ))
       label_exps in
   `Record (_loc, (sem_of_list rec_exps)) : exp )

let mkfun names acc =
  List.fold_right
    (fun name  acc  ->
       (`Fun (_loc, (`Case (_loc, (`Lid (_loc, name)), acc))) : Ast.exp ))
    names acc

let (<+<) pats acc =
  List.fold_right
    (fun p  acc  -> (`Fun (_loc, (`Case (_loc, p, acc))) : Ast.exp )) pats
    acc

let eta_expand (exp : exp) number =
  (let names = List.init number (fun i  -> x ~off:0 i) in
   mkfun names (exp +> names) : exp )

let gen_curry_n (acc : exp) ~arity  cons n =
  (let args =
     List.init arity
       (fun i  -> List.init n (fun j  -> (xid ~off:i j : Ast.pat ))) in
   let pat = (EP.of_str cons :>pat) in
   List.fold_right
     (fun p  acc  -> (`Fun (_loc, (`Case (_loc, p, acc))) : Ast.exp ))
     (List.map (fun lst  -> appl_of_list (pat :: lst)) args) acc : exp )

let currying cases ~arity  =
  let cases = bar_of_list cases in
  if arity >= 2
  then
    let names = List.init arity (fun i  -> x ~off:i 0) in
    let exps = List.map (fun s  -> (`Lid (_loc, s) : Ast.exp )) names in
    let x = tuple_com exps in
    mkfun names (`Match (_loc, x, cases) : Ast.exp )
  else (`Fun (_loc, cases) : Ast.exp )

let unknown len =
  if len = 0
  then
    (`Send (_loc, (`Lid (_loc, "self")), (`Lid (_loc, "unknown"))) : 
    Ast.exp )
  else
    (`App
       (_loc, (`Lid (_loc, "failwith")), (`Str (_loc, "not implemented!"))) : 
    Ast.exp )

let mee_comma x y =
  (`App
     (_loc,
       (`App
          (_loc, (`App (_loc, (`Vrn (_loc, "Com")), (`Lid (_loc, "_loc")))),
            x)), y) : Ast.exp )

let mee_app x y =
  (`App
     (_loc,
       (`App
          (_loc, (`App (_loc, (`Vrn (_loc, "App")), (`Lid (_loc, "_loc")))),
            x)), y) : Ast.exp )

let mee_of_str s =
  let len = String.length s in
  if (s.[0]) = '`'
  then
    let s = String.sub s 1 (len - 1) in
    (`App
       (_loc, (`Vrn (_loc, "Vrn")),
         (`Par (_loc, (`Com (_loc, (`Lid (_loc, "_loc")), (`Str (_loc, s))))))) : 
      Ast.exp )
  else
    (`App
       (_loc, (`Vrn (_loc, "Uid")),
         (`Par (_loc, (`Com (_loc, (`Lid (_loc, "_loc")), (`Str (_loc, s))))))) : 
    Ast.exp )

let mk_tuple_ee =
  function
  | [] -> invalid_arg "mktupee arity is zero "
  | x::[] -> x
  | xs ->
      let v = List.reduce_right mee_comma xs in
      (`App
         (_loc, (`Vrn (_loc, "Par")),
           (`Par (_loc, (`Com (_loc, (`Lid (_loc, "_loc")), v))))) : 
        Ast.exp )

let mee_record_col label exp =
  (`App
     (_loc,
       (`App
          (_loc,
            (`App (_loc, (`Vrn (_loc, "RecBind")), (`Lid (_loc, "_loc")))),
            (`App
               (_loc, (`Vrn (_loc, "Lid")),
                 (`Par
                    (_loc,
                      (`Com
                         (_loc, (`Lid (_loc, "_loc")), (`Str (_loc, label)))))))))),
       exp) : Ast.exp )

let mee_record_semi a b =
  (`App
     (_loc,
       (`App
          (_loc, (`App (_loc, (`Vrn (_loc, "Sem")), (`Lid (_loc, "_loc")))),
            a)), b) : Ast.exp )

let mk_record_ee label_exps =
  (label_exps |> (List.map (fun (label,exp)  -> mee_record_col label exp)))
    |>
    (fun es  ->
       (`App
          (_loc,
            (`App (_loc, (`Vrn (_loc, "Record")), (`Lid (_loc, "_loc")))),
            (List.reduce_right mee_record_semi es)) : Ast.exp ))