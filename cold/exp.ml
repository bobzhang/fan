open FAst

open AstLib

open LibUtil

open FanUtil

let substp loc env =
  let bad_pat _loc =
    FanLoc.errorf _loc
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
               let cons: FAst.exp = `Lid (_loc, code) in
               let code' = "__fan__" ^ code in
               let cons': FAst.exp = `Lid (_loc, code') in
               let () = constraints <- (cons, cons') :: constraints in
               (`Lid (_loc, code') : FAst.pat ))
      | p -> super#pat p
    method get_captured_variables = constraints
    method clear_captured_variables = constraints <- []
  end

let filter_pat_with_captured_variables pat =
  begin
    capture_antiquot#clear_captured_variables;
    (let pat = capture_antiquot#pat pat in
     let constraints = capture_antiquot#get_captured_variables in
     (pat, constraints))
  end