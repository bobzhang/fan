open FanOps
open AstLoc
open LibUtil
open Basic
open FanUtil
open EP
let rec pattern_eq_expression p e =
  match (p, e) with
  | ((`Id (_loc,`Lid (_,a)) : Ast.pat),`Id (_,`Lid (_,b)))
    |((`Id (_loc,`Uid (_,a)) : Ast.pat),`Id (_,`Uid (_,b))) -> a = b
  | ((`App (_loc,p1,p2) : Ast.pat),`App (_,e1,e2)) ->
      (pattern_eq_expression p1 e1) && (pattern_eq_expression p2 e2)
  | _ -> false
let map loc (p : pat) (e : exp) (l : exp) =
  match (p, e) with
  | ((`Id (_loc,`Lid (_,x)) : Ast.pat),(`Id (_,`Lid (_,y)) : Ast.exp)) when
      x = y -> l
  | _ ->
      if is_irrefut_pat p
      then
        (`App
           (loc,
             (`App
                (loc,
                  (`Id
                     (loc,
                       (`Dot (loc, (`Uid (loc, "List")), (`Lid (loc, "map")))))),
                  (`Fun (loc, (`Case (loc, p, e)))))), l) : Ast.exp )
      else
        (`App
           (loc,
             (`App
                (loc,
                  (`App
                     (loc,
                       (`Id
                          (loc,
                            (`Dot
                               (loc, (`Uid (loc, "List")),
                                 (`Lid (loc, "fold_right")))))),
                       (`Fun
                          (loc,
                            (`Or
                               (loc,
                                 (`CaseWhen
                                    (loc, p,
                                      (`Id (loc, (`Lid (loc, "true")))),
                                      (`App
                                         (loc,
                                           (`Fun
                                              (loc,
                                                (`Case
                                                   (loc,
                                                     (`Id
                                                        (loc,
                                                          (`Lid (loc, "x")))),
                                                     (`Fun
                                                        (loc,
                                                          (`Case
                                                             (loc,
                                                               (`Id
                                                                  (loc,
                                                                    (
                                                                    `Lid
                                                                    (loc,
                                                                    "xs")))),
                                                               (`App
                                                                  (loc,
                                                                    (
                                                                    `App
                                                                    (loc,
                                                                    (`Id
                                                                    (loc,
                                                                    (`Uid
                                                                    (loc,
                                                                    "::")))),
                                                                    (`Id
                                                                    (loc,
                                                                    (`Lid
                                                                    (loc,
                                                                    "x")))))),
                                                                    (
                                                                    `Id
                                                                    (loc,
                                                                    (`Lid
                                                                    (loc,
                                                                    "xs")))))))))))))),
                                           e)))),
                                 (`Case
                                    (loc, (`Any loc),
                                      (`Fun
                                         (loc,
                                           (`Case
                                              (loc,
                                                (`Id (loc, (`Lid (loc, "l")))),
                                                (`Id (loc, (`Lid (loc, "l")))))))))))))))),
                  l)), (`Id (loc, (`Uid (loc, "[]"))))) : Ast.exp )
let filter loc p b l =
  if is_irrefut_pat p
  then
    (`App
       (loc,
         (`App
            (loc,
              (`Id
                 (loc,
                   (`Dot (loc, (`Uid (loc, "List")), (`Lid (loc, "filter")))))),
              (`Fun (loc, (`Case (loc, p, b)))))), l) : Ast.exp )
  else
    (`App
       (loc,
         (`App
            (loc,
              (`Id
                 (loc,
                   (`Dot (loc, (`Uid (loc, "List")), (`Lid (loc, "filter")))))),
              (`Fun
                 (loc,
                   (`Or
                      (loc,
                        (`CaseWhen
                           (loc, p, (`Id (loc, (`Lid (loc, "true")))), b)),
                        (`Case
                           (loc, (`Any loc),
                             (`Id (loc, (`Lid (loc, "false")))))))))))), l) : 
    Ast.exp )
let concat _loc l =
  (`App
     (_loc,
       (`Id
          (_loc,
            (`Dot (_loc, (`Uid (_loc, "List")), (`Lid (_loc, "concat")))))),
       l) : Ast.exp )
let rec compr _loc e =
  function
  | (`gen (p,l))::[] -> map _loc p e l
  | (`gen (p,l))::(`cond b)::items ->
      compr _loc e ((`gen (p, (filter _loc p b l))) :: items)
  | (`gen (p,l))::((`gen (_,_))::_ as is) ->
      concat _loc (map _loc p (compr _loc e is) l)
  | _ -> raise Stream.Failure
let bad_pat _loc =
  FanLoc.raise _loc
    (Failure "this macro cannot be used in a pattern (see its definition)")
let substp loc env =
  let rec loop (x : exp) =
    match x with
    | (`App (_loc,e1,e2) : Ast.exp) ->
        (`App (loc, (loop e1), (loop e2)) : Ast.pat )
    | (`Id (_loc,`Lid (_,x)) : Ast.exp) ->
        (try List.assoc x env
         with | Not_found  -> (`Id (loc, (`Lid (loc, x))) : Ast.pat ))
    | (`Id (_loc,`Uid (_,x)) : Ast.exp) ->
        (try List.assoc x env
         with | Not_found  -> (`Id (loc, (`Uid (loc, x))) : Ast.pat ))
    | (`Int (_loc,x) : Ast.exp) -> (`Int (loc, x) : Ast.pat )
    | (`Str (_loc,s) : Ast.exp) -> (`Str (loc, s) : Ast.pat )
    | (`Tup (_loc,x) : Ast.exp) -> (`Tup (loc, (loop x)) : Ast.pat )
    | (`Com (_loc,x1,x2) : Ast.exp) ->
        (`Com (loc, (loop x1), (loop x2)) : Ast.pat )
    | (`Record (_loc,bi) : Ast.exp) ->
        let rec substbi =
          function
          | `Sem (_loc,b1,b2) -> `Sem (_loc, (substbi b1), (substbi b2))
          | `RecBind (_loc,i,e) -> `RecBind (loc, i, (loop e))
          | _ -> bad_pat _loc in
        (`Record (loc, (substbi bi)) : Ast.pat )
    | _ -> bad_pat loc in
  loop
class subst loc env =
  object 
    inherit  (Objs.reloc loc) as super
    method! exp =
      function
      | (`Id (_loc,`Lid (_,x)) : Ast.exp)|(`Id (_loc,`Uid (_,x)) : Ast.exp)
          as e -> (try List.assoc x env with | Not_found  -> super#exp e)
      | (`App (_loc,`Id (_,`Uid (_,"LOCATION_OF")),`Id (_,`Lid (_,x))) :
          Ast.exp)
        |(`App (_loc,`Id (_,`Uid (_,"LOCATION_OF")),`Id (_,`Uid (_,x))) :
           Ast.exp)
          as e ->
          (try
             let loc = loc_of (List.assoc x env) in
             let (a,b,c,d,e,f,g,h) = FanLoc.to_tuple loc in
             (`App
                (_loc,
                  (`Id
                     (_loc,
                       (`Dot
                          (_loc, (`Uid (_loc, "FanLoc")),
                            (`Lid (_loc, "of_tuple")))))),
                  (`Tup
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
                                  then
                                    (`Id (_loc, (`Lid (_loc, "true"))) : 
                                    Ast.exp )
                                  else
                                    (`Id (_loc, (`Lid (_loc, "false"))) : 
                                    Ast.exp ))))))))) : Ast.exp )
           with | Not_found  -> super#exp e)
      | e -> super#exp e
    method! pat =
      function
      | (`Id (_loc,`Lid (_,x)) : Ast.pat)|(`Id (_loc,`Uid (_,x)) : Ast.pat)
          as p ->
          (try substp loc [] (List.assoc x env)
           with | Not_found  -> super#pat p)
      | p -> super#pat p
  end
class type antiquot_filter
  =
  object 
    inherit Objs.map
    method get_captured_variables : (exp* exp) list
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
               let cons = `Id (_loc, (`Lid (_loc, code))) in
               let code' = "__fan__" ^ code in
               let cons' = `Id (_loc, (`Lid (_loc, code'))) in
               let () = constraints <- (cons, cons') :: constraints in
               (`Id (_loc, (`Lid (_loc, code'))) : Ast.pat ))
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
  then
    (`Fun (_loc, (`Case (_loc, (`Id (_loc, (`Uid (_loc, "()")))), body))) : 
    Ast.exp )
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
let failure: Ast.exp =
  `App
    (_loc, (`Id (_loc, (`Lid (_loc, "raise")))),
      (`App
         (_loc, (`Id (_loc, (`Uid (_loc, "Failure")))),
           (`Str (_loc, "metafilter: Cannot handle that kind of types ")))))
let (<+) names acc =
  List.fold_right
    (fun name  acc  ->
       (`Fun (_loc, (`Case (_loc, (`Id (_loc, (`Lid (_loc, name)))), acc))) : 
       Ast.exp )) names acc
let (<+<) pats acc =
  List.fold_right (fun p  acc  -> `Fun (_loc, (`Case (_loc, p, acc)))) pats
    acc
let mee_comma x y =
  `App
    (_loc,
      (`App
         (_loc,
           (`App
              (_loc, (`Vrn (_loc, "Com")),
                (`Id (_loc, (`Lid (_loc, "_loc")))))), x)), y)
let mvee_comma x y =
  `App
    (_loc, (`Vrn (_loc, "Com")),
      (`Tup
         (_loc,
           (`Com
              (_loc, (`Id (_loc, (`Lid (_loc, "_loc")))),
                (`Com (_loc, x, y)))))))
let mee_app x y =
  `App
    (_loc,
      (`App
         (_loc,
           (`App
              (_loc, (`Vrn (_loc, "App")),
                (`Id (_loc, (`Lid (_loc, "_loc")))))), x)), y)
let mee_of_str s =
  let len = String.length s in
  if (s.[0]) = '`'
  then
    let s = String.sub s 1 (len - 1) in
    `App
      (_loc, (`Vrn (_loc, "Vrn")),
        (`Tup
           (_loc,
             (`Com
                (_loc, (`Id (_loc, (`Lid (_loc, "_loc")))), (`Str (_loc, s)))))))
  else
    (let u =
       `App
         (_loc, (`Vrn (_loc, "Uid")),
           (`Tup
              (_loc,
                (`Com
                   (_loc, (`Id (_loc, (`Lid (_loc, "_loc")))),
                     (`Str (_loc, s))))))) in
     `App
       (_loc,
         (`App
            (_loc, (`Vrn (_loc, "Id")), (`Id (_loc, (`Lid (_loc, "_loc")))))),
         u))
let vee_of_str s =
  `App
    (_loc, (`Vrn (_loc, "Vrn")),
      (`Tup
         (_loc,
           (`Com
              (_loc, (`Id (_loc, (`Lid (_loc, "_loc")))), (`Str (_loc, s)))))))
let meee_of_str s =
  let u =
    `App
      (_loc,
        (`App
           (_loc,
             (`App
                (_loc, (`Vrn (_loc, "App")),
                  (`Id (_loc, (`Lid (_loc, "_loc")))))),
             (`App
                (_loc,
                  (`App
                     (_loc, (`Vrn (_loc, "Vrn")),
                       (`Id (_loc, (`Lid (_loc, "_loc")))))),
                  (`Str (_loc, "Uid")))))),
        (`App
           (_loc,
             (`App
                (_loc, (`Vrn (_loc, "Tup")),
                  (`Id (_loc, (`Lid (_loc, "_loc")))))),
             (`App
                (_loc,
                  (`App
                     (_loc,
                       (`App
                          (_loc, (`Vrn (_loc, "Com")),
                            (`Id (_loc, (`Lid (_loc, "_loc")))))),
                       (`App
                          (_loc,
                            (`App
                               (_loc, (`Vrn (_loc, "Id")),
                                 (`Id (_loc, (`Lid (_loc, "_loc")))))),
                            (`App
                               (_loc,
                                 (`App
                                    (_loc, (`Vrn (_loc, "Lid")),
                                      (`Id (_loc, (`Lid (_loc, "_loc")))))),
                                 (`Str (_loc, "_loc")))))))),
                  (`App
                     (_loc, (`Vrn (_loc, "Str")),
                       (`Tup
                          (_loc,
                            (`Com
                               (_loc, (`Id (_loc, (`Lid (_loc, "_loc")))),
                                 (`Str (_loc, s))))))))))))) in
  `App
    (_loc,
      (`App
         (_loc,
           (`App
              (_loc, (`Vrn (_loc, "App")),
                (`Id (_loc, (`Lid (_loc, "_loc")))))),
           (`App
              (_loc,
                (`App
                   (_loc,
                     (`App
                        (_loc, (`Vrn (_loc, "App")),
                          (`Id (_loc, (`Lid (_loc, "_loc")))))),
                     (`App
                        (_loc,
                          (`App
                             (_loc, (`Vrn (_loc, "Vrn")),
                               (`Id (_loc, (`Lid (_loc, "_loc")))))),
                          (`Str (_loc, "Id")))))),
                (`App
                   (_loc,
                     (`App
                        (_loc, (`Vrn (_loc, "Id")),
                          (`Id (_loc, (`Lid (_loc, "_loc")))))),
                     (`App
                        (_loc,
                          (`App
                             (_loc, (`Vrn (_loc, "Lid")),
                               (`Id (_loc, (`Lid (_loc, "_loc")))))),
                          (`Str (_loc, "_loc")))))))))), u)
let mk_tuple_ee =
  function
  | [] -> invalid_arg "mktupee arity is zero "
  | x::[] -> x
  | xs ->
      `App
        (_loc, (`Vrn (_loc, "Tup")),
          (`Tup
             (_loc,
               (`Com
                  (_loc, (`Id (_loc, (`Lid (_loc, "_loc")))),
                    (List.reduce_right mee_comma xs))))))
let mee_record_col label exp =
  `App
    (_loc,
      (`App
         (_loc,
           (`App
              (_loc, (`Vrn (_loc, "RecBind")),
                (`Id (_loc, (`Lid (_loc, "_loc")))))),
           (`App
              (_loc, (`Vrn (_loc, "Lid")),
                (`Tup
                   (_loc,
                     (`Com
                        (_loc, (`Id (_loc, (`Lid (_loc, "_loc")))),
                          (`Str (_loc, label)))))))))), exp)
let mee_record_semi a b =
  `App
    (_loc,
      (`App
         (_loc,
           (`App
              (_loc, (`Vrn (_loc, "Sem")),
                (`Id (_loc, (`Lid (_loc, "_loc")))))), a)), b)
let mk_record_ee label_exps =
  (label_exps |> (List.map (fun (label,exp)  -> mee_record_col label exp)))
    |>
    (fun es  ->
       `App
         (_loc,
           (`App
              (_loc, (`Vrn (_loc, "Record")),
                (`Id (_loc, (`Lid (_loc, "_loc")))))),
           (List.reduce_right mee_record_semi es)))
let eta_expand (exp : exp) number =
  (let names = List.init number (fun i  -> x ~off:0 i) in
   names <+ (exp +> names) : exp )
let gen_curry_n (acc : exp) ~arity  cons n =
  (let args =
     List.init arity
       (fun i  -> List.init n (fun j  -> `Id (_loc, (xid ~off:i j)))) in
   let pat = of_str cons in
   List.fold_right (fun p  acc  -> `Fun (_loc, (`Case (_loc, p, acc))))
     (List.map (fun lst  -> appl_of_list (pat :: lst)) args) acc : exp )
let currying cases ~arity  =
  let cases = or_of_list cases in
  if arity >= 2
  then
    let names = List.init arity (fun i  -> x ~off:i 0) in
    let exps = List.map (fun s  -> `Id (_loc, (`Lid (_loc, s)))) names in
    let x = tuple_com exps in names <+ (`Match (_loc, x, cases))
  else `Fun (_loc, cases)
let unknown len =
  if len = 0
  then
    `Send
      (_loc, (`Id (_loc, (`Lid (_loc, "self")))), (`Lid (_loc, "unknown")))
  else
    `App
      (_loc, (`Id (_loc, (`Lid (_loc, "failwith")))),
        (`Str (_loc, "not implemented!")))