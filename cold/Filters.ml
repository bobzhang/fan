open LibUtil
module Ast = FanAst
module MetaLoc =
  struct
  let meta_loc_patt _loc _ = `PaId (_loc, (`IdLid (_loc, "loc")))
  let meta_loc_expr _loc _ = `ExId (_loc, (`IdLid (_loc, "loc")))
  end
module MetaAst = FanAst.Make(MetaLoc)
let _ =
  AstFilters.register_str_item_filter
    ("lift",
      (fun ast  ->
         let _loc = Ast.loc_of_str_item ast in
         `StExp
           (_loc,
             (`ExLet
                (_loc, `ReNil,
                  (`BiEq
                     (_loc, (`PaId (_loc, (`IdLid (_loc, "loc")))),
                       (`ExId
                          (_loc,
                            (`IdAcc
                               (_loc, (`IdUid (_loc, "FanLoc")),
                                 (`IdLid (_loc, "ghost")))))))),
                  (MetaAst.Expr.meta_str_item _loc ast))))))
let add_debug_expr e =
  let _loc = Ast.loc_of_expr e in
  let msg = "camlp4-debug: exc: %s at " ^ ((FanLoc.to_string _loc) ^ "@.") in
  `ExTry
    (_loc, e,
      (`McOr
         (_loc,
           (`McArr
              (_loc,
                (`PaAli
                   (_loc,
                     (`PaOrp
                        (_loc,
                          (`PaId
                             (_loc,
                               (`IdAcc
                                  (_loc, (`IdUid (_loc, "XStream")),
                                    (`IdUid (_loc, "Failure")))))),
                          (`PaId (_loc, (`IdUid (_loc, "Exit")))))),
                     (`PaId (_loc, (`IdLid (_loc, "exc")))))), (`ExNil _loc),
                (`ExApp
                   (_loc, (`ExId (_loc, (`IdLid (_loc, "raise")))),
                     (`ExId (_loc, (`IdLid (_loc, "exc")))))))),
           (`McArr
              (_loc, (`PaId (_loc, (`IdLid (_loc, "exc")))), (`ExNil _loc),
                (`ExSeq
                   (_loc,
                     (`ExSem
                        (_loc,
                          (`ExIfe
                             (_loc,
                               (`ExApp
                                  (_loc,
                                    (`ExId
                                       (_loc,
                                         (`IdAcc
                                            (_loc, (`IdUid (_loc, "Debug")),
                                              (`IdLid (_loc, "mode")))))),
                                    (`ExStr (_loc, "exc")))),
                               (`ExApp
                                  (_loc,
                                    (`ExApp
                                       (_loc,
                                         (`ExId
                                            (_loc,
                                              (`IdAcc
                                                 (_loc,
                                                   (`IdUid (_loc, "Format")),
                                                   (`IdLid (_loc, "eprintf")))))),
                                         (`ExStr
                                            (_loc,
                                              (Ast.safe_string_escaped msg))))),
                                    (`ExApp
                                       (_loc,
                                         (`ExId
                                            (_loc,
                                              (`IdAcc
                                                 (_loc,
                                                   (`IdUid (_loc, "Printexc")),
                                                   (`IdLid
                                                      (_loc, "to_string")))))),
                                         (`ExId
                                            (_loc, (`IdLid (_loc, "exc")))))))),
                               (`ExId (_loc, (`IdUid (_loc, "()")))))),
                          (`ExApp
                             (_loc, (`ExId (_loc, (`IdLid (_loc, "raise")))),
                               (`ExId (_loc, (`IdLid (_loc, "exc")))))))))))))))
let rec map_match_case =
  function
  | `McOr (_loc,m1,m2) ->
      `McOr (_loc, (map_match_case m1), (map_match_case m2))
  | `McArr (_loc,p,w,e) -> `McArr (_loc, p, w, (add_debug_expr e))
  | m -> m
let _ =
  AstFilters.register_str_item_filter
    ("exception",
      ((object 
          inherit  Ast.map as super
          method! expr =
            function
            | `ExFun (_loc,m) -> `ExFun (_loc, (map_match_case m))
            | x -> super#expr x
          method! str_item =
            function
            | `StMod (_loc,"Debug",_) as st -> st
            | st -> super#str_item st
        end)#str_item))
let _ =
  AstFilters.register_str_item_filter
    ("strip", (((new Ast.reloc) FanLoc.ghost)#str_item))
let decorate_binding decorate_fun =
  (object 
     inherit  Ast.map as super
     method! binding =
       function
       | `BiEq (_loc,`PaId (_,`IdLid (_,id)),(`ExFun (_,_) as e)) ->
           `BiEq
             (_loc, (`PaId (_loc, (`IdLid (_loc, id)))), (decorate_fun id e))
       | b -> super#binding b
   end)#binding
let decorate decorate_fun =
  object (o)
    inherit  Ast.map as super
    method! str_item =
      function
      | `StVal (_loc,r,b) ->
          `StVal (_loc, r, (decorate_binding decorate_fun b))
      | st -> super#str_item st
    method! expr =
      function
      | `ExLet (_loc,r,b,e) ->
          `ExLet (_loc, r, (decorate_binding decorate_fun b), (o#expr e))
      | `ExFun (_loc,_) as e -> decorate_fun "<fun>" e
      | e -> super#expr e
  end
let decorate_this_expr e id =
  let buf = Buffer.create 42 in
  let _loc = Ast.loc_of_expr e in
  let () = Format.bprintf buf "%s @@ %a@?" id FanLoc.dump _loc in
  let s = Buffer.contents buf in
  `ExLet
    (_loc, `ReNil,
      (`BiEq
         (_loc, (`PaId (_loc, (`IdUid (_loc, "()")))),
           (`ExApp
              (_loc,
                (`ExId
                   (_loc,
                     (`IdAcc
                        (_loc, (`IdUid (_loc, "Camlp4prof")),
                          (`IdLid (_loc, "count")))))),
                (`ExStr (_loc, (Ast.safe_string_escaped s))))))), e)
let rec decorate_fun id =
  let decorate = decorate decorate_fun in
  let decorate_expr = decorate#expr in
  let decorate_match_case = decorate#match_case in
  function
  | `ExFun (_loc,`McArr (_,p,`ExNil _,e)) ->
      `ExFun (_loc, (`McArr (_loc, p, (`ExNil _loc), (decorate_fun id e))))
  | `ExFun (_loc,m) ->
      decorate_this_expr (`ExFun (_loc, (decorate_match_case m))) id
  | e -> decorate_this_expr (decorate_expr e) id
let _ =
  AstFilters.register_str_item_filter
    ("profile", ((decorate decorate_fun)#str_item))
let _ =
  AstFilters.register_str_item_filter
    ("trash",
      ((Ast.map_str_item
          (function | `StMod (_loc,"Camlp4Trash",_) -> `StNil _loc | st -> st))#str_item))
let map_expr =
  function
  | `ExApp (_loc,e,`ExId (_,`IdUid (_,"NOTHING")))|`ExFun
                                                     (_loc,`McArr
                                                             (_,`PaId
                                                                  (_,
                                                                   `IdUid
                                                                    (_,"NOTHING")),
                                                              `ExNil _,e))
      -> e
  | `ExId (_loc,`IdLid (_,"__FILE__")) ->
      `ExStr (_loc, (Ast.safe_string_escaped (FanLoc.file_name _loc)))
  | `ExId (_loc,`IdLid (_,"__PWD__")) ->
      `ExStr
        (_loc,
          (Ast.safe_string_escaped (Filename.dirname (FanLoc.file_name _loc))))
  | `ExId (_loc,`IdLid (_,"__LOCATION__")) ->
      let (a,b,c,d,e,f,g,h) = FanLoc.to_tuple _loc in
      `ExApp
        (_loc,
          (`ExId
             (_loc,
               (`IdAcc
                  (_loc, (`IdUid (_loc, "FanLoc")),
                    (`IdLid (_loc, "of_tuple")))))),
          (`ExTup
             (_loc,
               (`ExCom
                  (_loc, (`ExStr (_loc, (Ast.safe_string_escaped a))),
                    (`ExCom
                       (_loc,
                         (`ExCom
                            (_loc,
                              (`ExCom
                                 (_loc,
                                   (`ExCom
                                      (_loc,
                                        (`ExCom
                                           (_loc,
                                             (`ExCom
                                                (_loc,
                                                  (`ExInt
                                                     (_loc,
                                                       (string_of_int b))),
                                                  (`ExInt
                                                     (_loc,
                                                       (string_of_int c))))),
                                             (`ExInt
                                                (_loc, (string_of_int d))))),
                                        (`ExInt (_loc, (string_of_int e))))),
                                   (`ExInt (_loc, (string_of_int f))))),
                              (`ExInt (_loc, (string_of_int g))))),
                         (if h
                          then `ExId (_loc, (`IdLid (_loc, "true")))
                          else `ExId (_loc, (`IdLid (_loc, "false")))))))))))
  | e -> e
let _ =
  AstFilters.register_str_item_filter
    ("trash_nothing", ((Ast.map_expr map_expr)#str_item))
let make_filter (s,code) =
  let f =
    function
    | `StExp (_loc,`ExId (_,`IdLid (_,s'))) when s = s' -> code
    | e -> e in
  (("filter_" ^ s), ((Ast.map_str_item f)#str_item))