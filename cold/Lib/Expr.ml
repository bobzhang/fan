open FanUtil
module Ast  = Camlp4Ast
let rec  sep_expr (acc) =
  
  function
  | Ast.ExAcc(_,e1,e2) -> (sep_expr ( (sep_expr acc e2) ) e1)
  | (Ast.ExId(loc,Ast.IdUid(_,s)) as e) ->
    
    (match acc
    with
    | []  -> [(loc,[] ,e)]
    | (loc',sl,e)::l -> (( (FanLoc.merge loc loc') ),( s::sl ),e)::l)
  | Ast.ExId(_,(Ast.IdAcc(_,_,_) as i)) ->
    (sep_expr acc ( (Ident.normalize_acc i) ))
  | e -> (( (Ast.loc_of_expr e) ),[] ,e)::acc
let rec  fa (al) =
  
  function
  | Ast.ExApp(_,f,a) -> (fa ( a::al ) f)
  | f -> (f,al)
let rec  apply (accu) =
  
  function
  | []  -> accu
  | x::xs ->
    
    let  _loc = (Ast.loc_of_expr x) in
    (apply ( Ast.ExApp ((_loc,accu,x)) ) xs)
let  mklist (_loc) =
  
  let rec  loop (top) =
  
  function
  | []  -> Ast.ExId ((_loc,( Ast.IdUid ((_loc,"[]")) )))
  | e1::el ->
    
    let  _loc =
    if top then _loc else (FanLoc.merge ( (Ast.loc_of_expr e1) ) _loc) in
    Ast.ExApp
      ((_loc,(
        Ast.ExApp
          ((_loc,( Ast.ExId ((_loc,( Ast.IdUid ((_loc,"::")) ))) ),e1)) ),(
        (loop false  el) ))) in (loop true )
let  mkumin (_loc) (f) (arg) =
  
  (match arg
  with
  | Ast.ExInt(_,n) -> Ast.ExInt ((_loc,( (neg_string n) )))
  | Ast.ExInt32(_,n) -> Ast.ExInt32 ((_loc,( (neg_string n) )))
  | Ast.ExInt64(_,n) -> Ast.ExInt64 ((_loc,( (neg_string n) )))
  | Ast.ExNativeInt(_,n) -> Ast.ExNativeInt ((_loc,( (neg_string n) )))
  | Ast.ExFlo(_,n) -> Ast.ExFlo ((_loc,( (neg_string n) )))
  | _ ->
    Ast.ExApp
      ((_loc,( Ast.ExId ((_loc,( Ast.IdLid ((_loc,( ("~" ^ f) ))) ))) ),arg)))
let  mkassert (_loc) =
  
  function
  | Ast.ExId(_,Ast.IdUid(_,"False")) -> Ast.ExAsf (_loc)
  | e -> Ast.ExAsr ((_loc,e))
let  mklist_last ?last  (_loc) =
  
  let rec  loop (top) =
  
  function
  | []  ->
    
    (match last
    with
    | Some(e) -> e
    | None  -> Ast.ExId ((_loc,( Ast.IdUid ((_loc,"[]")) ))))
  | e1::el ->
    
    let  _loc =
    if top then _loc else (FanLoc.merge ( (Ast.loc_of_expr e1) ) _loc) in
    Ast.ExApp
      ((_loc,(
        Ast.ExApp
          ((_loc,( Ast.ExId ((_loc,( Ast.IdUid ((_loc,"::")) ))) ),e1)) ),(
        (loop false  el) ))) in (loop true )
let  mksequence (_loc) =
  
  function
  | ((Ast.ExSem(_,_,_) |Ast.ExAnt(_,_)) as e) -> Ast.ExSeq ((_loc,e))
  | e -> e
let  mksequence' (_loc) =
  
  function
  | (Ast.ExSem(_,_,_) as e) -> Ast.ExSeq ((_loc,e))
  | e -> e
let  bigarray_get (_loc) (arr) (arg) =
  
  let  coords =
  
  (match arg
  with
  | (Ast.ExTup(_,Ast.ExCom(_,e1,e2)) |Ast.ExCom(_,e1,e2)) ->
    (Ast.list_of_expr e1 ( (Ast.list_of_expr e2 [] ) ))
  | _ -> [arg]) in
  
  (match coords
  with
  | []  -> (failwith "bigarray_get null list")
  | c1::[]  ->
    Ast.ExApp
      ((_loc,(
        Ast.ExApp
          ((_loc,(
            Ast.ExId
              ((_loc,(
                Ast.IdAcc
                  ((_loc,( Ast.IdUid ((_loc,"Bigarray")) ),(
                    Ast.IdAcc
                      ((_loc,( Ast.IdUid ((_loc,"Array1")) ),(
                        Ast.IdLid ((_loc,"get")) ))) ))) ))) ),arr)) ),c1))
  | c1::c2::[]  ->
    Ast.ExApp
      ((_loc,(
        Ast.ExApp
          ((_loc,(
            Ast.ExApp
              ((_loc,(
                Ast.ExId
                  ((_loc,(
                    Ast.IdAcc
                      ((_loc,( Ast.IdUid ((_loc,"Bigarray")) ),(
                        Ast.IdAcc
                          ((_loc,( Ast.IdUid ((_loc,"Array2")) ),(
                            Ast.IdLid ((_loc,"get")) ))) ))) ))) ),arr))
            ),c1)) ),c2))
  | c1::c2::c3::[]  ->
    Ast.ExApp
      ((_loc,(
        Ast.ExApp
          ((_loc,(
            Ast.ExApp
              ((_loc,(
                Ast.ExApp
                  ((_loc,(
                    Ast.ExId
                      ((_loc,(
                        Ast.IdAcc
                          ((_loc,( Ast.IdUid ((_loc,"Bigarray")) ),(
                            Ast.IdAcc
                              ((_loc,( Ast.IdUid ((_loc,"Array3")) ),(
                                Ast.IdLid ((_loc,"get")) ))) ))) ))) ),arr))
                ),c1)) ),c2)) ),c3))
  | c1::c2::c3::coords ->
    Ast.ExApp
      ((_loc,(
        Ast.ExApp
          ((_loc,(
            Ast.ExId
              ((_loc,(
                Ast.IdAcc
                  ((_loc,( Ast.IdUid ((_loc,"Bigarray")) ),(
                    Ast.IdAcc
                      ((_loc,( Ast.IdUid ((_loc,"Genarray")) ),(
                        Ast.IdLid ((_loc,"get")) ))) ))) ))) ),arr)) ),(
        Ast.ExArr
          ((_loc,(
            Ast.ExSem
              ((_loc,c1,(
                Ast.ExSem
                  ((_loc,c2,(
                    Ast.ExSem ((_loc,c3,( (Ast.exSem_of_list coords) ))) )))
                ))) ))) ))))
let  bigarray_set (_loc) (var) (newval) =
  
  (match var
  with
  |
    Ast.ExApp(_,Ast.ExApp(_,Ast.ExId(_,Ast.IdAcc(_,Ast.IdUid(_,"Bigarray"),Ast.IdAcc
                                                 (_,Ast.IdUid(_,"Array1"),Ast.IdLid
                                                  (_,"get")))),arr),c1) ->
    Some
      (Ast.ExAss
         ((_loc,(
           Ast.ExAcc
             ((_loc,(
               Ast.ExApp
                 ((_loc,(
                   Ast.ExApp
                     ((_loc,(
                       Ast.ExId
                         ((_loc,(
                           Ast.IdAcc
                             ((_loc,( Ast.IdUid ((_loc,"Bigarray")) ),(
                               Ast.IdAcc
                                 ((_loc,( Ast.IdUid ((_loc,"Array1")) ),(
                                   Ast.IdLid ((_loc,"get")) ))) ))) )))
                       ),arr)) ),c1)) ),(
               Ast.ExId ((_loc,( Ast.IdLid ((_loc,"contents")) ))) )))
           ),newval)))
  |
    Ast.ExApp(_,Ast.ExApp(_,Ast.ExApp(_,Ast.ExId(_,Ast.IdAcc(_,Ast.IdUid
                                                             (_,"Bigarray"),Ast.IdAcc
                                                             (_,Ast.IdUid
                                                              (_,"Array2"),Ast.IdLid
                                                              (_,"get")))),arr),c1),c2)
    ->
    Some
      (Ast.ExAss
         ((_loc,(
           Ast.ExAcc
             ((_loc,(
               Ast.ExApp
                 ((_loc,(
                   Ast.ExApp
                     ((_loc,(
                       Ast.ExApp
                         ((_loc,(
                           Ast.ExId
                             ((_loc,(
                               Ast.IdAcc
                                 ((_loc,( Ast.IdUid ((_loc,"Bigarray")) ),(
                                   Ast.IdAcc
                                     ((_loc,( Ast.IdUid ((_loc,"Array2")) ),(
                                       Ast.IdLid ((_loc,"get")) ))) ))) )))
                           ),arr)) ),c1)) ),c2)) ),(
               Ast.ExId ((_loc,( Ast.IdLid ((_loc,"contents")) ))) )))
           ),newval)))
  |
    Ast.ExApp(_,Ast.ExApp(_,Ast.ExApp(_,Ast.ExApp(_,Ast.ExId(_,Ast.IdAcc
                                                             (_,Ast.IdUid
                                                              (_,"Bigarray"),Ast.IdAcc
                                                              (_,Ast.IdUid
                                                               (_,"Array3"),Ast.IdLid
                                                               (_,"get")))),arr),c1),c2),c3)
    ->
    Some
      (Ast.ExAss
         ((_loc,(
           Ast.ExAcc
             ((_loc,(
               Ast.ExApp
                 ((_loc,(
                   Ast.ExApp
                     ((_loc,(
                       Ast.ExApp
                         ((_loc,(
                           Ast.ExApp
                             ((_loc,(
                               Ast.ExId
                                 ((_loc,(
                                   Ast.IdAcc
                                     ((_loc,( Ast.IdUid ((_loc,"Bigarray"))
                                       ),(
                                       Ast.IdAcc
                                         ((_loc,( Ast.IdUid ((_loc,"Array3"))
                                           ),( Ast.IdLid ((_loc,"get")) )))
                                       ))) ))) ),arr)) ),c1)) ),c2)) ),c3))
               ),( Ast.ExId ((_loc,( Ast.IdLid ((_loc,"contents")) ))) )))
           ),newval)))
  |
    Ast.ExApp(_,Ast.ExApp(_,Ast.ExId(_,Ast.IdAcc(_,Ast.IdUid(_,"Bigarray"),Ast.IdAcc
                                                 (_,Ast.IdUid(_,"Genarray"),Ast.IdLid
                                                  (_,"get")))),arr),Ast.ExArr
              (_,coords)) ->
    Some
      (Ast.ExApp
         ((_loc,(
           Ast.ExApp
             ((_loc,(
               Ast.ExApp
                 ((_loc,(
                   Ast.ExId
                     ((_loc,(
                       Ast.IdAcc
                         ((_loc,( Ast.IdUid ((_loc,"Bigarray")) ),(
                           Ast.IdAcc
                             ((_loc,( Ast.IdUid ((_loc,"Genarray")) ),(
                               Ast.IdLid ((_loc,"set")) ))) ))) ))) ),arr))
               ),( Ast.ExArr ((_loc,coords)) ))) ),newval)))
  | _ -> None)
let  map (_loc) (p) (e) (l) =
  
  (match (p,e)
  with
  | (Ast.PaId(_,Ast.IdLid(_,x)),Ast.ExId(_,Ast.IdLid(_,y))) when (x = y) -> l
  | _ ->
    if (Ast.is_irrefut_patt p) then
     (
     Ast.ExApp
       ((_loc,(
         Ast.ExApp
           ((_loc,(
             Ast.ExId
               ((_loc,(
                 Ast.IdAcc
                   ((_loc,( Ast.IdUid ((_loc,"List")) ),(
                     Ast.IdLid ((_loc,"map")) ))) ))) ),(
             Ast.ExFun
               ((_loc,( Ast.McArr ((_loc,p,( Ast.ExNil (_loc) ),e)) ))) )))
         ),l))
     )
    else
     Ast.ExApp
       ((_loc,(
         Ast.ExApp
           ((_loc,(
             Ast.ExApp
               ((_loc,(
                 Ast.ExId
                   ((_loc,(
                     Ast.IdAcc
                       ((_loc,( Ast.IdUid ((_loc,"List")) ),(
                         Ast.IdLid ((_loc,"fold_right")) ))) ))) ),(
                 Ast.ExFun
                   ((_loc,(
                     Ast.McOr
                       ((_loc,(
                         Ast.McArr
                           ((_loc,p,(
                             Ast.ExId ((_loc,( Ast.IdUid ((_loc,"True")) )))
                             ),(
                             Ast.ExApp
                               ((_loc,(
                                 Ast.ExFun
                                   ((_loc,(
                                     Ast.McArr
                                       ((_loc,(
                                         Ast.PaId
                                           ((_loc,( Ast.IdLid ((_loc,"x")) )))
                                         ),( Ast.ExNil (_loc) ),(
                                         Ast.ExFun
                                           ((_loc,(
                                             Ast.McArr
                                               ((_loc,(
                                                 Ast.PaId
                                                   ((_loc,(
                                                     Ast.IdLid ((_loc,"xs"))
                                                     ))) ),( Ast.ExNil (_loc)
                                                 ),(
                                                 Ast.ExApp
                                                   ((_loc,(
                                                     Ast.ExApp
                                                       ((_loc,(
                                                         Ast.ExId
                                                           ((_loc,(
                                                             Ast.IdUid
                                                               ((_loc,"::"))
                                                             ))) ),(
                                                         Ast.ExId
                                                           ((_loc,(
                                                             Ast.IdLid
                                                               ((_loc,"x"))
                                                             ))) ))) ),(
                                                     Ast.ExId
                                                       ((_loc,(
                                                         Ast.IdLid
                                                           ((_loc,"xs")) )))
                                                     ))) ))) ))) ))) ))) ),e))
                             ))) ),(
                         Ast.McArr
                           ((_loc,( Ast.PaAny (_loc) ),( Ast.ExNil (_loc) ),(
                             Ast.ExFun
                               ((_loc,(
                                 Ast.McArr
                                   ((_loc,(
                                     Ast.PaId
                                       ((_loc,( Ast.IdLid ((_loc,"l")) )))
                                     ),( Ast.ExNil (_loc) ),(
                                     Ast.ExId
                                       ((_loc,( Ast.IdLid ((_loc,"l")) ))) )))
                                 ))) ))) ))) ))) ))) ),l)) ),(
         Ast.ExId ((_loc,( Ast.IdUid ((_loc,"[]")) ))) ))))
let  filter (_loc) (p) (b) (l) =
  if (Ast.is_irrefut_patt p) then
   (
   Ast.ExApp
     ((_loc,(
       Ast.ExApp
         ((_loc,(
           Ast.ExId
             ((_loc,(
               Ast.IdAcc
                 ((_loc,( Ast.IdUid ((_loc,"List")) ),(
                   Ast.IdLid ((_loc,"filter")) ))) ))) ),(
           Ast.ExFun ((_loc,( Ast.McArr ((_loc,p,( Ast.ExNil (_loc) ),b)) )))
           ))) ),l))
   )
  else
   Ast.ExApp
     ((_loc,(
       Ast.ExApp
         ((_loc,(
           Ast.ExId
             ((_loc,(
               Ast.IdAcc
                 ((_loc,( Ast.IdUid ((_loc,"List")) ),(
                   Ast.IdLid ((_loc,"filter")) ))) ))) ),(
           Ast.ExFun
             ((_loc,(
               Ast.McOr
                 ((_loc,(
                   Ast.McArr
                     ((_loc,p,(
                       Ast.ExId ((_loc,( Ast.IdUid ((_loc,"True")) ))) ),b))
                   ),(
                   Ast.McArr
                     ((_loc,( Ast.PaAny (_loc) ),( Ast.ExNil (_loc) ),(
                       Ast.ExId ((_loc,( Ast.IdUid ((_loc,"False")) ))) )))
                   ))) ))) ))) ),l))
let  concat (_loc) (l) =
  Ast.ExApp
    ((_loc,(
      Ast.ExId
        ((_loc,(
          Ast.IdAcc
            ((_loc,( Ast.IdUid ((_loc,"List")) ),(
              Ast.IdLid ((_loc,"concat")) ))) ))) ),l))
let rec  compr (_loc) (e) =
  
  function
  | (`gen (p,l))::[]  -> (map _loc p e l)
  | (`gen (p,l))::(`cond b)::items ->
    (compr _loc e ( `gen ((p,( (filter _loc p b l) )))::items ))
  | (`gen (p,l))::((`gen (_,_))::_ as is) ->
    (concat _loc ( (map _loc p ( (compr _loc e is) ) l) ))
  | _ -> (raise Stream.Failure )
let  bad_patt (_loc) =
  (FanLoc.raise _loc (
    Failure ("this macro cannot be used in a pattern (see its definition)")
    ))
let  substp (_loc) (env) =
  
  let rec  loop =
  
  function
  | Ast.ExApp(_,e1,e2) -> Ast.PaApp ((_loc,( (loop e1) ),( (loop e2) )))
  | Ast.ExNil(_) -> Ast.PaNil (_loc)
  | Ast.ExId(_,Ast.IdLid(_,x)) ->
    
    (try (List.assoc x env)
    with
    | Not_found  -> Ast.PaId ((_loc,( Ast.IdLid ((_loc,x)) ))))
  | Ast.ExId(_,Ast.IdUid(_,x)) ->
    
    (try (List.assoc x env)
    with
    | Not_found  -> Ast.PaId ((_loc,( Ast.IdUid ((_loc,x)) ))))
  | Ast.ExInt(_,x) -> Ast.PaInt ((_loc,x))
  | Ast.ExStr(_,s) -> Ast.PaStr ((_loc,s))
  | Ast.ExTup(_,x) -> Ast.PaTup ((_loc,( (loop x) )))
  | Ast.ExCom(_,x1,x2) -> Ast.PaCom ((_loc,( (loop x1) ),( (loop x2) )))
  | Ast.ExRec(_,bi,Ast.ExNil(_)) ->
    
    let rec  substbi =
    
    function
    | Ast.RbSem(_,b1,b2) ->
      Ast.PaSem ((_loc,( (substbi b1) ),( (substbi b2) )))
    | Ast.RbEq(_,i,e) -> Ast.PaEq ((_loc,i,( (loop e) )))
    | _ -> (bad_patt _loc) in Ast.PaRec ((_loc,( (substbi bi) )))
  | _ -> (bad_patt _loc) in loop
class
   subst (_loc) (env) = object
     inherit  (Ast.reloc _loc) as super
    method !  expr =
      
      function
      | ((Ast.ExId(_,Ast.IdLid(_,x)) |Ast.ExId(_,Ast.IdUid(_,x))) as e) ->
        
        (try (List.assoc x env) with
        | Not_found  -> (super#expr e))
      |
        ((Ast.ExApp(_loc,Ast.ExId(_,Ast.IdUid(_,"LOCATION_OF")),Ast.ExId
                    (_,Ast.IdLid(_,x)))
           |Ast.ExApp(_loc,Ast.ExId(_,Ast.IdUid(_,"LOCATION_OF")),Ast.ExId
                      (_,Ast.IdUid(_,x)))) as e) ->
        
        (try
        
        let  loc = (Ast.loc_of_expr ( (List.assoc x env) )) in
        
        let  (a,b,c,d,e,f,g,h) = (FanLoc.to_tuple loc) in
        Ast.ExApp
          ((_loc,(
            Ast.ExId
              ((_loc,(
                Ast.IdAcc
                  ((_loc,( Ast.IdUid ((_loc,"FanLoc")) ),(
                    Ast.IdLid ((_loc,"of_tuple")) ))) ))) ),(
            Ast.ExTup
              ((_loc,(
                Ast.ExCom
                  ((_loc,( Ast.ExStr ((_loc,( (Ast.safe_string_escaped a) )))
                    ),(
                    Ast.ExCom
                      ((_loc,(
                        Ast.ExCom
                          ((_loc,(
                            Ast.ExCom
                              ((_loc,(
                                Ast.ExCom
                                  ((_loc,(
                                    Ast.ExCom
                                      ((_loc,(
                                        Ast.ExCom
                                          ((_loc,(
                                            Ast.ExInt
                                              ((_loc,( (string_of_int b) )))
                                            ),(
                                            Ast.ExInt
                                              ((_loc,( (string_of_int c) )))
                                            ))) ),(
                                        Ast.ExInt
                                          ((_loc,( (string_of_int d) ))) )))
                                    ),(
                                    Ast.ExInt ((_loc,( (string_of_int e) )))
                                    ))) ),(
                                Ast.ExInt ((_loc,( (string_of_int f) ))) )))
                            ),( Ast.ExInt ((_loc,( (string_of_int g) ))) )))
                        ),(
                        if h then
                         (
                         Ast.ExId ((_loc,( Ast.IdUid ((_loc,"True")) )))
                         )
                        else Ast.ExId ((_loc,( Ast.IdUid ((_loc,"False")) )))
                        ))) ))) ))) ))) with
        | Not_found  -> (super#expr e))
      | e -> (super#expr e)
    method !  patt =
      
      function
      | ((Ast.PaId(_,Ast.IdLid(_,x)) |Ast.PaId(_,Ast.IdUid(_,x))) as p) ->
        
        (try (substp _loc []  ( (List.assoc x env) ))
        with
        | Not_found  -> (super#patt p))
      | p -> (super#patt p) end
let  map_expr =
  
  function
  |
    (Ast.ExApp(_,e,Ast.ExId(_,Ast.IdUid(_,"NOTHING")))
      |Ast.ExFun(_,Ast.McArr(_,Ast.PaId(_,Ast.IdUid(_,"NOTHING")),Ast.ExNil(_),e)))
    -> e
  | Ast.ExId(_loc,Ast.IdLid(_,"__FILE__")) ->
    Ast.ExStr
      ((_loc,( (Ast.safe_string_escaped ( (FanLoc.file_name _loc) )) )))
  | Ast.ExId(_loc,Ast.IdLid(_,"__LOCATION__")) ->
    
    let  (a,b,c,d,e,f,g,h) = (FanLoc.to_tuple _loc) in
    Ast.ExApp
      ((_loc,(
        Ast.ExId
          ((_loc,(
            Ast.IdAcc
              ((_loc,( Ast.IdUid ((_loc,"FanLoc")) ),(
                Ast.IdLid ((_loc,"of_tuple")) ))) ))) ),(
        Ast.ExTup
          ((_loc,(
            Ast.ExCom
              ((_loc,( Ast.ExStr ((_loc,( (Ast.safe_string_escaped a) ))) ),(
                Ast.ExCom
                  ((_loc,(
                    Ast.ExCom
                      ((_loc,(
                        Ast.ExCom
                          ((_loc,(
                            Ast.ExCom
                              ((_loc,(
                                Ast.ExCom
                                  ((_loc,(
                                    Ast.ExCom
                                      ((_loc,(
                                        Ast.ExInt
                                          ((_loc,( (string_of_int b) ))) ),(
                                        Ast.ExInt
                                          ((_loc,( (string_of_int c) ))) )))
                                    ),(
                                    Ast.ExInt ((_loc,( (string_of_int d) )))
                                    ))) ),(
                                Ast.ExInt ((_loc,( (string_of_int e) ))) )))
                            ),( Ast.ExInt ((_loc,( (string_of_int f) ))) )))
                        ),( Ast.ExInt ((_loc,( (string_of_int g) ))) ))) ),(
                    if h then
                     (
                     Ast.ExId ((_loc,( Ast.IdUid ((_loc,"True")) )))
                     )
                    else Ast.ExId ((_loc,( Ast.IdUid ((_loc,"False")) ))) )))
                ))) ))) )))
  | e -> e
let  antiquot_expander ~parse_patt  ~parse_expr  =
  object
     inherit  Ast.map as super
    method !  patt =
      
      function
      | ((Ast.PaAnt(_loc,s) |Ast.PaStr(_loc,s)) as p) ->
        
        let  mloc (_loc) = (Meta.MetaLocQuotation.meta_loc_patt _loc _loc) in
        (handle_antiquot_in_string ~s:s ~default:p ~parse:parse_patt
          ~loc:_loc ~decorate:(
          (fun (n) ->
            (fun (p) ->
              
              (match n
              with
              | "antisig_item" ->
                Ast.PaApp
                  ((_loc,(
                    Ast.PaApp
                      ((_loc,(
                        Ast.PaId
                          ((_loc,(
                            Ast.IdAcc
                              ((_loc,( Ast.IdUid ((_loc,"Ast")) ),(
                                Ast.IdUid ((_loc,"SgAnt")) ))) ))) ),(
                        (mloc _loc) ))) ),p))
              | "antistr_item" ->
                Ast.PaApp
                  ((_loc,(
                    Ast.PaApp
                      ((_loc,(
                        Ast.PaId
                          ((_loc,(
                            Ast.IdAcc
                              ((_loc,( Ast.IdUid ((_loc,"Ast")) ),(
                                Ast.IdUid ((_loc,"StAnt")) ))) ))) ),(
                        (mloc _loc) ))) ),p))
              | "antictyp" ->
                Ast.PaApp
                  ((_loc,(
                    Ast.PaApp
                      ((_loc,(
                        Ast.PaId
                          ((_loc,(
                            Ast.IdAcc
                              ((_loc,( Ast.IdUid ((_loc,"Ast")) ),(
                                Ast.IdUid ((_loc,"TyAnt")) ))) ))) ),(
                        (mloc _loc) ))) ),p))
              | "antipatt" ->
                Ast.PaApp
                  ((_loc,(
                    Ast.PaApp
                      ((_loc,(
                        Ast.PaId
                          ((_loc,(
                            Ast.IdAcc
                              ((_loc,( Ast.IdUid ((_loc,"Ast")) ),(
                                Ast.IdUid ((_loc,"PaAnt")) ))) ))) ),(
                        (mloc _loc) ))) ),p))
              | "antiexpr" ->
                Ast.PaApp
                  ((_loc,(
                    Ast.PaApp
                      ((_loc,(
                        Ast.PaId
                          ((_loc,(
                            Ast.IdAcc
                              ((_loc,( Ast.IdUid ((_loc,"Ast")) ),(
                                Ast.IdUid ((_loc,"ExAnt")) ))) ))) ),(
                        (mloc _loc) ))) ),p))
              | "antimodule_type" ->
                Ast.PaApp
                  ((_loc,(
                    Ast.PaApp
                      ((_loc,(
                        Ast.PaId
                          ((_loc,(
                            Ast.IdAcc
                              ((_loc,( Ast.IdUid ((_loc,"Ast")) ),(
                                Ast.IdUid ((_loc,"MtAnt")) ))) ))) ),(
                        (mloc _loc) ))) ),p))
              | "antimodule_expr" ->
                Ast.PaApp
                  ((_loc,(
                    Ast.PaApp
                      ((_loc,(
                        Ast.PaId
                          ((_loc,(
                            Ast.IdAcc
                              ((_loc,( Ast.IdUid ((_loc,"Ast")) ),(
                                Ast.IdUid ((_loc,"MeAnt")) ))) ))) ),(
                        (mloc _loc) ))) ),p))
              | "anticlass_type" ->
                Ast.PaApp
                  ((_loc,(
                    Ast.PaApp
                      ((_loc,(
                        Ast.PaId
                          ((_loc,(
                            Ast.IdAcc
                              ((_loc,( Ast.IdUid ((_loc,"Ast")) ),(
                                Ast.IdUid ((_loc,"CtAnt")) ))) ))) ),(
                        (mloc _loc) ))) ),p))
              | "anticlass_expr" ->
                Ast.PaApp
                  ((_loc,(
                    Ast.PaApp
                      ((_loc,(
                        Ast.PaId
                          ((_loc,(
                            Ast.IdAcc
                              ((_loc,( Ast.IdUid ((_loc,"Ast")) ),(
                                Ast.IdUid ((_loc,"CeAnt")) ))) ))) ),(
                        (mloc _loc) ))) ),p))
              | "anticlass_sig_item" ->
                Ast.PaApp
                  ((_loc,(
                    Ast.PaApp
                      ((_loc,(
                        Ast.PaId
                          ((_loc,(
                            Ast.IdAcc
                              ((_loc,( Ast.IdUid ((_loc,"Ast")) ),(
                                Ast.IdUid ((_loc,"CgAnt")) ))) ))) ),(
                        (mloc _loc) ))) ),p))
              | "anticlass_str_item" ->
                Ast.PaApp
                  ((_loc,(
                    Ast.PaApp
                      ((_loc,(
                        Ast.PaId
                          ((_loc,(
                            Ast.IdAcc
                              ((_loc,( Ast.IdUid ((_loc,"Ast")) ),(
                                Ast.IdUid ((_loc,"CrAnt")) ))) ))) ),(
                        (mloc _loc) ))) ),p))
              | "antiwith_constr" ->
                Ast.PaApp
                  ((_loc,(
                    Ast.PaApp
                      ((_loc,(
                        Ast.PaId
                          ((_loc,(
                            Ast.IdAcc
                              ((_loc,( Ast.IdUid ((_loc,"Ast")) ),(
                                Ast.IdUid ((_loc,"WcAnt")) ))) ))) ),(
                        (mloc _loc) ))) ),p))
              | "antibinding" ->
                Ast.PaApp
                  ((_loc,(
                    Ast.PaApp
                      ((_loc,(
                        Ast.PaId
                          ((_loc,(
                            Ast.IdAcc
                              ((_loc,( Ast.IdUid ((_loc,"Ast")) ),(
                                Ast.IdUid ((_loc,"BiAnt")) ))) ))) ),(
                        (mloc _loc) ))) ),p))
              | "antirec_binding" ->
                Ast.PaApp
                  ((_loc,(
                    Ast.PaApp
                      ((_loc,(
                        Ast.PaId
                          ((_loc,(
                            Ast.IdAcc
                              ((_loc,( Ast.IdUid ((_loc,"Ast")) ),(
                                Ast.IdUid ((_loc,"RbAnt")) ))) ))) ),(
                        (mloc _loc) ))) ),p))
              | "antimatch_case" ->
                Ast.PaApp
                  ((_loc,(
                    Ast.PaApp
                      ((_loc,(
                        Ast.PaId
                          ((_loc,(
                            Ast.IdAcc
                              ((_loc,( Ast.IdUid ((_loc,"Ast")) ),(
                                Ast.IdUid ((_loc,"McAnt")) ))) ))) ),(
                        (mloc _loc) ))) ),p))
              | "antimodule_binding" ->
                Ast.PaApp
                  ((_loc,(
                    Ast.PaApp
                      ((_loc,(
                        Ast.PaId
                          ((_loc,(
                            Ast.IdAcc
                              ((_loc,( Ast.IdUid ((_loc,"Ast")) ),(
                                Ast.IdUid ((_loc,"MbAnt")) ))) ))) ),(
                        (mloc _loc) ))) ),p))
              | "antiident" ->
                Ast.PaApp
                  ((_loc,(
                    Ast.PaApp
                      ((_loc,(
                        Ast.PaId
                          ((_loc,(
                            Ast.IdAcc
                              ((_loc,( Ast.IdUid ((_loc,"Ast")) ),(
                                Ast.IdUid ((_loc,"IdAnt")) ))) ))) ),(
                        (mloc _loc) ))) ),p))
              | _ -> p))) ))
      | p -> (super#patt p)
    method !  expr =
      
      function
      | ((Ast.ExAnt(_loc,s) |Ast.ExStr(_loc,s)) as e) ->
        
        let  mloc (_loc) = (Meta.MetaLocQuotation.meta_loc_expr _loc _loc) in
        (handle_antiquot_in_string ~s:s ~default:e ~parse:parse_expr
          ~loc:_loc ~decorate:(
          (fun (n) ->
            (fun (e) ->
              
              (match n
              with
              | "`int" ->
                Ast.ExApp
                  ((_loc,(
                    Ast.ExId ((_loc,( Ast.IdLid ((_loc,"string_of_int")) )))
                    ),e))
              | "`int32" ->
                Ast.ExApp
                  ((_loc,(
                    Ast.ExId
                      ((_loc,(
                        Ast.IdAcc
                          ((_loc,( Ast.IdUid ((_loc,"Int32")) ),(
                            Ast.IdLid ((_loc,"to_string")) ))) ))) ),e))
              | "`int64" ->
                Ast.ExApp
                  ((_loc,(
                    Ast.ExId
                      ((_loc,(
                        Ast.IdAcc
                          ((_loc,( Ast.IdUid ((_loc,"Int64")) ),(
                            Ast.IdLid ((_loc,"to_string")) ))) ))) ),e))
              | "`nativeint" ->
                Ast.ExApp
                  ((_loc,(
                    Ast.ExId
                      ((_loc,(
                        Ast.IdAcc
                          ((_loc,( Ast.IdUid ((_loc,"Nativeint")) ),(
                            Ast.IdLid ((_loc,"to_string")) ))) ))) ),e))
              | "`flo" ->
                Ast.ExApp
                  ((_loc,(
                    Ast.ExId
                      ((_loc,(
                        Ast.IdAcc
                          ((_loc,( Ast.IdUid ((_loc,"FanUtil")) ),(
                            Ast.IdLid ((_loc,"float_repres")) ))) ))) ),e))
              | "`str" ->
                Ast.ExApp
                  ((_loc,(
                    Ast.ExId
                      ((_loc,(
                        Ast.IdAcc
                          ((_loc,( Ast.IdUid ((_loc,"Ast")) ),(
                            Ast.IdLid ((_loc,"safe_string_escaped")) ))) )))
                    ),e))
              | "`chr" ->
                Ast.ExApp
                  ((_loc,(
                    Ast.ExId
                      ((_loc,(
                        Ast.IdAcc
                          ((_loc,( Ast.IdUid ((_loc,"Char")) ),(
                            Ast.IdLid ((_loc,"escaped")) ))) ))) ),e))
              | "`bool" ->
                Ast.ExApp
                  ((_loc,(
                    Ast.ExApp
                      ((_loc,(
                        Ast.ExId
                          ((_loc,(
                            Ast.IdAcc
                              ((_loc,( Ast.IdUid ((_loc,"Ast")) ),(
                                Ast.IdUid ((_loc,"IdUid")) ))) ))) ),(
                        (mloc _loc) ))) ),(
                    Ast.ExIfe
                      ((_loc,e,( Ast.ExStr ((_loc,"True")) ),(
                        Ast.ExStr ((_loc,"False")) ))) )))
              | "liststr_item" ->
                Ast.ExApp
                  ((_loc,(
                    Ast.ExId
                      ((_loc,(
                        Ast.IdAcc
                          ((_loc,( Ast.IdUid ((_loc,"Ast")) ),(
                            Ast.IdLid ((_loc,"stSem_of_list")) ))) ))) ),e))
              | "listsig_item" ->
                Ast.ExApp
                  ((_loc,(
                    Ast.ExId
                      ((_loc,(
                        Ast.IdAcc
                          ((_loc,( Ast.IdUid ((_loc,"Ast")) ),(
                            Ast.IdLid ((_loc,"sgSem_of_list")) ))) ))) ),e))
              | "listclass_sig_item" ->
                Ast.ExApp
                  ((_loc,(
                    Ast.ExId
                      ((_loc,(
                        Ast.IdAcc
                          ((_loc,( Ast.IdUid ((_loc,"Ast")) ),(
                            Ast.IdLid ((_loc,"cgSem_of_list")) ))) ))) ),e))
              | "listclass_str_item" ->
                Ast.ExApp
                  ((_loc,(
                    Ast.ExId
                      ((_loc,(
                        Ast.IdAcc
                          ((_loc,( Ast.IdUid ((_loc,"Ast")) ),(
                            Ast.IdLid ((_loc,"crSem_of_list")) ))) ))) ),e))
              | "listmodule_expr" ->
                Ast.ExApp
                  ((_loc,(
                    Ast.ExId
                      ((_loc,(
                        Ast.IdAcc
                          ((_loc,( Ast.IdUid ((_loc,"Ast")) ),(
                            Ast.IdLid ((_loc,"meApp_of_list")) ))) ))) ),e))
              | "listmodule_type" ->
                Ast.ExApp
                  ((_loc,(
                    Ast.ExId
                      ((_loc,(
                        Ast.IdAcc
                          ((_loc,( Ast.IdUid ((_loc,"Ast")) ),(
                            Ast.IdLid ((_loc,"mtApp_of_list")) ))) ))) ),e))
              | "listmodule_binding" ->
                Ast.ExApp
                  ((_loc,(
                    Ast.ExId
                      ((_loc,(
                        Ast.IdAcc
                          ((_loc,( Ast.IdUid ((_loc,"Ast")) ),(
                            Ast.IdLid ((_loc,"mbAnd_of_list")) ))) ))) ),e))
              | "listbinding" ->
                Ast.ExApp
                  ((_loc,(
                    Ast.ExId
                      ((_loc,(
                        Ast.IdAcc
                          ((_loc,( Ast.IdUid ((_loc,"Ast")) ),(
                            Ast.IdLid ((_loc,"biAnd_of_list")) ))) ))) ),e))
              | "listbinding;" ->
                Ast.ExApp
                  ((_loc,(
                    Ast.ExId
                      ((_loc,(
                        Ast.IdAcc
                          ((_loc,( Ast.IdUid ((_loc,"Ast")) ),(
                            Ast.IdLid ((_loc,"biSem_of_list")) ))) ))) ),e))
              | "listrec_binding" ->
                Ast.ExApp
                  ((_loc,(
                    Ast.ExId
                      ((_loc,(
                        Ast.IdAcc
                          ((_loc,( Ast.IdUid ((_loc,"Ast")) ),(
                            Ast.IdLid ((_loc,"rbSem_of_list")) ))) ))) ),e))
              | "listclass_type" ->
                Ast.ExApp
                  ((_loc,(
                    Ast.ExId
                      ((_loc,(
                        Ast.IdAcc
                          ((_loc,( Ast.IdUid ((_loc,"Ast")) ),(
                            Ast.IdLid ((_loc,"ctAnd_of_list")) ))) ))) ),e))
              | "listclass_expr" ->
                Ast.ExApp
                  ((_loc,(
                    Ast.ExId
                      ((_loc,(
                        Ast.IdAcc
                          ((_loc,( Ast.IdUid ((_loc,"Ast")) ),(
                            Ast.IdLid ((_loc,"ceAnd_of_list")) ))) ))) ),e))
              | "listident" ->
                Ast.ExApp
                  ((_loc,(
                    Ast.ExId
                      ((_loc,(
                        Ast.IdAcc
                          ((_loc,( Ast.IdUid ((_loc,"Ast")) ),(
                            Ast.IdLid ((_loc,"idAcc_of_list")) ))) ))) ),e))
              | "listctypand" ->
                Ast.ExApp
                  ((_loc,(
                    Ast.ExId
                      ((_loc,(
                        Ast.IdAcc
                          ((_loc,( Ast.IdUid ((_loc,"Ast")) ),(
                            Ast.IdLid ((_loc,"tyAnd_of_list")) ))) ))) ),e))
              | "listctyp;" ->
                Ast.ExApp
                  ((_loc,(
                    Ast.ExId
                      ((_loc,(
                        Ast.IdAcc
                          ((_loc,( Ast.IdUid ((_loc,"Ast")) ),(
                            Ast.IdLid ((_loc,"tySem_of_list")) ))) ))) ),e))
              | "listctyp*" ->
                Ast.ExApp
                  ((_loc,(
                    Ast.ExId
                      ((_loc,(
                        Ast.IdAcc
                          ((_loc,( Ast.IdUid ((_loc,"Ast")) ),(
                            Ast.IdLid ((_loc,"tySta_of_list")) ))) ))) ),e))
              | "listctyp|" ->
                Ast.ExApp
                  ((_loc,(
                    Ast.ExId
                      ((_loc,(
                        Ast.IdAcc
                          ((_loc,( Ast.IdUid ((_loc,"Ast")) ),(
                            Ast.IdLid ((_loc,"tyOr_of_list")) ))) ))) ),e))
              | "listctyp," ->
                Ast.ExApp
                  ((_loc,(
                    Ast.ExId
                      ((_loc,(
                        Ast.IdAcc
                          ((_loc,( Ast.IdUid ((_loc,"Ast")) ),(
                            Ast.IdLid ((_loc,"tyCom_of_list")) ))) ))) ),e))
              | "listctyp&" ->
                Ast.ExApp
                  ((_loc,(
                    Ast.ExId
                      ((_loc,(
                        Ast.IdAcc
                          ((_loc,( Ast.IdUid ((_loc,"Ast")) ),(
                            Ast.IdLid ((_loc,"tyAmp_of_list")) ))) ))) ),e))
              | "listwith_constr" ->
                Ast.ExApp
                  ((_loc,(
                    Ast.ExId
                      ((_loc,(
                        Ast.IdAcc
                          ((_loc,( Ast.IdUid ((_loc,"Ast")) ),(
                            Ast.IdLid ((_loc,"wcAnd_of_list")) ))) ))) ),e))
              | "listmatch_case" ->
                Ast.ExApp
                  ((_loc,(
                    Ast.ExId
                      ((_loc,(
                        Ast.IdAcc
                          ((_loc,( Ast.IdUid ((_loc,"Ast")) ),(
                            Ast.IdLid ((_loc,"mcOr_of_list")) ))) ))) ),e))
              | "listpatt," ->
                Ast.ExApp
                  ((_loc,(
                    Ast.ExId
                      ((_loc,(
                        Ast.IdAcc
                          ((_loc,( Ast.IdUid ((_loc,"Ast")) ),(
                            Ast.IdLid ((_loc,"paCom_of_list")) ))) ))) ),e))
              | "listpatt;" ->
                Ast.ExApp
                  ((_loc,(
                    Ast.ExId
                      ((_loc,(
                        Ast.IdAcc
                          ((_loc,( Ast.IdUid ((_loc,"Ast")) ),(
                            Ast.IdLid ((_loc,"paSem_of_list")) ))) ))) ),e))
              | "listexpr," ->
                Ast.ExApp
                  ((_loc,(
                    Ast.ExId
                      ((_loc,(
                        Ast.IdAcc
                          ((_loc,( Ast.IdUid ((_loc,"Ast")) ),(
                            Ast.IdLid ((_loc,"exCom_of_list")) ))) ))) ),e))
              | "listexpr;" ->
                Ast.ExApp
                  ((_loc,(
                    Ast.ExId
                      ((_loc,(
                        Ast.IdAcc
                          ((_loc,( Ast.IdUid ((_loc,"Ast")) ),(
                            Ast.IdLid ((_loc,"exSem_of_list")) ))) ))) ),e))
              | "antisig_item" ->
                Ast.ExApp
                  ((_loc,(
                    Ast.ExApp
                      ((_loc,(
                        Ast.ExId
                          ((_loc,(
                            Ast.IdAcc
                              ((_loc,( Ast.IdUid ((_loc,"Ast")) ),(
                                Ast.IdUid ((_loc,"SgAnt")) ))) ))) ),(
                        (mloc _loc) ))) ),e))
              | "antistr_item" ->
                Ast.ExApp
                  ((_loc,(
                    Ast.ExApp
                      ((_loc,(
                        Ast.ExId
                          ((_loc,(
                            Ast.IdAcc
                              ((_loc,( Ast.IdUid ((_loc,"Ast")) ),(
                                Ast.IdUid ((_loc,"StAnt")) ))) ))) ),(
                        (mloc _loc) ))) ),e))
              | "antictyp" ->
                Ast.ExApp
                  ((_loc,(
                    Ast.ExApp
                      ((_loc,(
                        Ast.ExId
                          ((_loc,(
                            Ast.IdAcc
                              ((_loc,( Ast.IdUid ((_loc,"Ast")) ),(
                                Ast.IdUid ((_loc,"TyAnt")) ))) ))) ),(
                        (mloc _loc) ))) ),e))
              | "antipatt" ->
                Ast.ExApp
                  ((_loc,(
                    Ast.ExApp
                      ((_loc,(
                        Ast.ExId
                          ((_loc,(
                            Ast.IdAcc
                              ((_loc,( Ast.IdUid ((_loc,"Ast")) ),(
                                Ast.IdUid ((_loc,"PaAnt")) ))) ))) ),(
                        (mloc _loc) ))) ),e))
              | "antiexpr" ->
                Ast.ExApp
                  ((_loc,(
                    Ast.ExApp
                      ((_loc,(
                        Ast.ExId
                          ((_loc,(
                            Ast.IdAcc
                              ((_loc,( Ast.IdUid ((_loc,"Ast")) ),(
                                Ast.IdUid ((_loc,"ExAnt")) ))) ))) ),(
                        (mloc _loc) ))) ),e))
              | "antimodule_type" ->
                Ast.ExApp
                  ((_loc,(
                    Ast.ExApp
                      ((_loc,(
                        Ast.ExId
                          ((_loc,(
                            Ast.IdAcc
                              ((_loc,( Ast.IdUid ((_loc,"Ast")) ),(
                                Ast.IdUid ((_loc,"MtAnt")) ))) ))) ),(
                        (mloc _loc) ))) ),e))
              | "antimodule_expr" ->
                Ast.ExApp
                  ((_loc,(
                    Ast.ExApp
                      ((_loc,(
                        Ast.ExId
                          ((_loc,(
                            Ast.IdAcc
                              ((_loc,( Ast.IdUid ((_loc,"Ast")) ),(
                                Ast.IdUid ((_loc,"MeAnt")) ))) ))) ),(
                        (mloc _loc) ))) ),e))
              | "anticlass_type" ->
                Ast.ExApp
                  ((_loc,(
                    Ast.ExApp
                      ((_loc,(
                        Ast.ExId
                          ((_loc,(
                            Ast.IdAcc
                              ((_loc,( Ast.IdUid ((_loc,"Ast")) ),(
                                Ast.IdUid ((_loc,"CtAnt")) ))) ))) ),(
                        (mloc _loc) ))) ),e))
              | "anticlass_expr" ->
                Ast.ExApp
                  ((_loc,(
                    Ast.ExApp
                      ((_loc,(
                        Ast.ExId
                          ((_loc,(
                            Ast.IdAcc
                              ((_loc,( Ast.IdUid ((_loc,"Ast")) ),(
                                Ast.IdUid ((_loc,"CeAnt")) ))) ))) ),(
                        (mloc _loc) ))) ),e))
              | "anticlass_sig_item" ->
                Ast.ExApp
                  ((_loc,(
                    Ast.ExApp
                      ((_loc,(
                        Ast.ExId
                          ((_loc,(
                            Ast.IdAcc
                              ((_loc,( Ast.IdUid ((_loc,"Ast")) ),(
                                Ast.IdUid ((_loc,"CgAnt")) ))) ))) ),(
                        (mloc _loc) ))) ),e))
              | "anticlass_str_item" ->
                Ast.ExApp
                  ((_loc,(
                    Ast.ExApp
                      ((_loc,(
                        Ast.ExId
                          ((_loc,(
                            Ast.IdAcc
                              ((_loc,( Ast.IdUid ((_loc,"Ast")) ),(
                                Ast.IdUid ((_loc,"CrAnt")) ))) ))) ),(
                        (mloc _loc) ))) ),e))
              | "antiwith_constr" ->
                Ast.ExApp
                  ((_loc,(
                    Ast.ExApp
                      ((_loc,(
                        Ast.ExId
                          ((_loc,(
                            Ast.IdAcc
                              ((_loc,( Ast.IdUid ((_loc,"Ast")) ),(
                                Ast.IdUid ((_loc,"WcAnt")) ))) ))) ),(
                        (mloc _loc) ))) ),e))
              | "antibinding" ->
                Ast.ExApp
                  ((_loc,(
                    Ast.ExApp
                      ((_loc,(
                        Ast.ExId
                          ((_loc,(
                            Ast.IdAcc
                              ((_loc,( Ast.IdUid ((_loc,"Ast")) ),(
                                Ast.IdUid ((_loc,"BiAnt")) ))) ))) ),(
                        (mloc _loc) ))) ),e))
              | "antirec_binding" ->
                Ast.ExApp
                  ((_loc,(
                    Ast.ExApp
                      ((_loc,(
                        Ast.ExId
                          ((_loc,(
                            Ast.IdAcc
                              ((_loc,( Ast.IdUid ((_loc,"Ast")) ),(
                                Ast.IdUid ((_loc,"RbAnt")) ))) ))) ),(
                        (mloc _loc) ))) ),e))
              | "antimatch_case" ->
                Ast.ExApp
                  ((_loc,(
                    Ast.ExApp
                      ((_loc,(
                        Ast.ExId
                          ((_loc,(
                            Ast.IdAcc
                              ((_loc,( Ast.IdUid ((_loc,"Ast")) ),(
                                Ast.IdUid ((_loc,"McAnt")) ))) ))) ),(
                        (mloc _loc) ))) ),e))
              | "antimodule_binding" ->
                Ast.ExApp
                  ((_loc,(
                    Ast.ExApp
                      ((_loc,(
                        Ast.ExId
                          ((_loc,(
                            Ast.IdAcc
                              ((_loc,( Ast.IdUid ((_loc,"Ast")) ),(
                                Ast.IdUid ((_loc,"MbAnt")) ))) ))) ),(
                        (mloc _loc) ))) ),e))
              | "antiident" ->
                Ast.ExApp
                  ((_loc,(
                    Ast.ExApp
                      ((_loc,(
                        Ast.ExId
                          ((_loc,(
                            Ast.IdAcc
                              ((_loc,( Ast.IdUid ((_loc,"Ast")) ),(
                                Ast.IdUid ((_loc,"IdAnt")) ))) ))) ),(
                        (mloc _loc) ))) ),e))
              | _ -> e))) ))
      | e -> (super#expr e) end
let  capture_antiquot =
  object
     inherit  Camlp4Ast.map as super val  mutable  constraints = []
    method !  patt =
      
      function
      | ((Ast.PaAnt(_loc,s) |Ast.PaStr(_loc,s)) as p) when (is_antiquot s) ->
        
        (match (view_antiquot s)
        with
        | Some(_name,code) ->
          
          let  cons = Ast.ExId ((_loc,( Ast.IdLid ((_loc,code)) ))) in
          
          let  code' = ("__" ^ code) in
          
          let  cons' = Ast.ExId ((_loc,( Ast.IdLid ((_loc,code')) ))) in
          
          let  ()  = constraints <- (cons,cons')::constraints in
          Ast.PaId ((_loc,( Ast.IdLid ((_loc,code')) )))
        | None  -> p)
      | p -> (super#patt p) method   get_captured_variables = constraints
    method   clear_captured_variables = constraints <- [] end
let  filter_patt_with_captured_variables (patt) =
  begin
  capture_antiquot#clear_captured_variables;
  
  let  patt = (capture_antiquot#patt patt) in
  
  let  constraints = capture_antiquot#get_captured_variables in
  (patt,constraints)
  end