open FanOps
open AstLoc
open LibUtil
open Basic
open FanUtil
open EP
let rec pattern_eq_expression p e =
  match (p, e) with
  | (`Id (_loc,`Lid (_,a)),`Id (_,`Lid (_,b)))
    |(`Id (_loc,`Uid (_,a)),`Id (_,`Uid (_,b))) -> a = b
  | (`App (_loc,p1,p2),`App (_,e1,e2)) ->
      (pattern_eq_expression p1 e1) && (pattern_eq_expression p2 e2)
  | _ -> false
let map loc (p : patt) (e : expr) (l : expr) =
  (match (p, e) with
   | (`Id (_loc,`Lid (_,x)),`Id (_,`Lid (_,y))) when x = y -> l
   | _ ->
       if is_irrefut_patt p
       then
         `App
           (loc,
             (`App
                (loc,
                  (`Id
                     (loc,
                       (`Dot (loc, (`Uid (loc, "List")), (`Lid (loc, "map")))))),
                  (`Fun (loc, (`Case (loc, p,  e)))))), l)
       else
         `App
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
                  l)), (`Id (loc, (`Uid (loc, "[]"))))) : expr )
let filter loc p b l =
  if is_irrefut_patt p
  then
    `App
      (loc,
        (`App
           (loc,
             (`Id
                (loc,
                  (`Dot (loc, (`Uid (loc, "List")), (`Lid (loc, "filter")))))),
             (`Fun (loc, (`Case (loc, p,  b)))))), l)
  else
    `App
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
                       (`CaseWhen (loc, p, (`Id (loc, (`Lid (loc, "true")))), b)),
                       (`Case
                          (loc, (`Any loc), 
                            (`Id (loc, (`Lid (loc, "false")))))))))))), l)
let concat _loc l =
  `App
    (_loc,
      (`Id
         (_loc,
           (`Dot (_loc, (`Uid (_loc, "List")), (`Lid (_loc, "concat")))))),
      l)
let rec compr _loc e =
  function
  | (`gen (p,l))::[] -> map _loc p e l
  | (`gen (p,l))::(`cond b)::items ->
      compr _loc e ((`gen (p, (filter _loc p b l))) :: items)
  | (`gen (p,l))::((`gen (_,_))::_ as is) ->
      concat _loc (map _loc p (compr _loc e is) l)
  | _ -> raise Stream.Failure
let bad_patt _loc =
  FanLoc.raise _loc
    (Failure "this macro cannot be used in a pattern (see its definition)")
let substp loc env =
  let rec loop =
    function
    | `App (_loc,e1,e2) -> `App (loc, (loop e1), (loop e2))
    | `Nil _loc -> `Nil loc
    | `Id (_loc,`Lid (_,x)) ->
        (try List.assoc x env with | Not_found  -> `Id (loc, (`Lid (loc, x))))
    | `Id (_loc,`Uid (_,x)) ->
        (try List.assoc x env with | Not_found  -> `Id (loc, (`Uid (loc, x))))
    | `Int (_loc,x) -> `Int (loc, x)
    | `Str (_loc,s) -> `Str (loc, s)
    | `Tup (_loc,x) -> `Tup (loc, (loop x))
    | `Com (_loc,x1,x2) -> `Com (loc, (loop x1), (loop x2))
    | `Record (_loc,bi) ->
        let rec substbi =
          function
          | `Sem (_loc,b1,b2) -> `Sem (_loc, (substbi b1), (substbi b2))
          | `RecBind (_loc,i,e) -> `RecBind (loc, i, (loop e))
          | _ -> bad_patt _loc in
        `Record (loc, (substbi bi))
    | _ -> bad_patt loc in
  loop
class subst loc env =
  object 
    inherit  (FanObjs.reloc loc) as super
    method! expr =
      function
      | `Id (_loc,`Lid (_,x))|`Id (_loc,`Uid (_,x)) as e ->
          (try List.assoc x env with | Not_found  -> super#expr e)
      | `App (_loc,`Id (_,`Uid (_,"LOCATION_OF")),`Id (_,`Lid (_,x)))
        |`App (_loc,`Id (_,`Uid (_,"LOCATION_OF")),`Id (_,`Uid (_,x))) as e
          ->
          (try
             let loc = loc_of (List.assoc x env) in
             let (a,b,c,d,e,f,g,h) = FanLoc.to_tuple loc in
             `App
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
                                 then `Id (_loc, (`Lid (_loc, "true")))
                                 else `Id (_loc, (`Lid (_loc, "false")))))))))))
           with | Not_found  -> super#expr e)
      | e -> super#expr e
    method! patt =
      function
      | `Id (_loc,`Lid (_,x))|`Id (_loc,`Uid (_,x)) as p ->
          (try substp loc [] (List.assoc x env)
           with | Not_found  -> super#patt p)
      | p -> super#patt p
  end
class type antiquot_filter
  =
  object 
    inherit Objs.map
    method get_captured_variables : (expr* expr) list
    method clear_captured_variables : unit
  end
let capture_antiquot: antiquot_filter =
  object 
    inherit  Objs.map as super
    val mutable constraints = []
    method! patt =
      function
      | `Ant (_loc,s) ->
          (match s with
           | { content = code;_} ->
               let cons = `Id (_loc, (`Lid (_loc, code))) in
               let code' = "__fan__" ^ code in
               let cons' = `Id (_loc, (`Lid (_loc, code'))) in
               let () = constraints <- (cons, cons') :: constraints in
               `Id (_loc, (`Lid (_loc, code'))))
      | p -> super#patt p
    method get_captured_variables = constraints
    method clear_captured_variables = constraints <- []
  end
let filter_patt_with_captured_variables patt =
  capture_antiquot#clear_captured_variables;
  (let patt = capture_antiquot#patt patt in
   let constraints = capture_antiquot#get_captured_variables in
   (patt, constraints))
let fun_args _loc args body =
  if args = []
  then
    `Fun
      (_loc,
        (`Case (_loc, (`Id (_loc, (`Uid (_loc, "()")))),  body)))
  else
    List.fold_right
      (fun arg  body  -> `Fun (_loc, (`Case (_loc, arg,  body))))
      args body
let _loc = FanLoc.ghost
let mk_record label_exprs =
  let rec_exprs =
    List.map
      (fun (label,expr)  -> `RecBind (_loc, (`Lid (_loc, label)), expr))
      label_exprs in
  `Record (_loc, (sem_of_list rec_exprs))
let failure =
  `App
    (_loc, (`Id (_loc, (`Lid (_loc, "raise")))),
      (`App
         (_loc, (`Id (_loc, (`Uid (_loc, "Failure")))),
           (`Str (_loc, "metafilter: Cannot handle that kind of types ")))))
let (<+) names acc =
  List.fold_right
    (fun name  acc  ->
       `Fun
         (_loc,
           (`Case (_loc, (`Id (_loc, (`Lid (_loc, name)))),  acc))))
    names acc
let (<+<) patts acc =
  List.fold_right
    (fun p  acc  -> `Fun (_loc, (`Case (_loc, p,  acc)))) patts
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
let mee_record_col label expr =
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
                          (`Str (_loc, label)))))))))), expr)
let mee_record_semi a b =
  `App
    (_loc,
      (`App
         (_loc,
           (`App
              (_loc, (`Vrn (_loc, "Sem")),
                (`Id (_loc, (`Lid (_loc, "_loc")))))), a)), b)
let mk_record_ee label_exprs =
  (label_exprs |> (List.map (fun (label,expr)  -> mee_record_col label expr)))
    |>
    (fun es  ->
       `App
         (_loc,
           (`App
              (_loc, (`Vrn (_loc, "Record")),
                (`Id (_loc, (`Lid (_loc, "_loc")))))),
           (List.reduce_right mee_record_semi es)))
let eta_expand expr number =
  let names = List.init number (fun i  -> x ~off:0 i) in
  names <+ (expr +> names)
let gen_curry_n acc ~arity  cons n =
  let args =
    List.init arity
      (fun i  -> List.init n (fun j  -> `Id (_loc, (xid ~off:i j)))) in
  let pat = of_str cons in
  List.fold_right
    (fun p  acc  -> `Fun (_loc, (`Case (_loc, p,  acc))))
    (List.map (fun lst  -> appl_of_list (pat :: lst)) args) acc
let currying match_cases ~arity  =
  let cases = or_of_list match_cases in
  if arity >= 2
  then
    let names = List.init arity (fun i  -> x ~off:i 0) in
    let exprs = List.map (fun s  -> `Id (_loc, (`Lid (_loc, s)))) names in
    let x = tuple_com exprs in names <+ (`Match (_loc, x, cases))
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
