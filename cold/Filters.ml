open LibUtil
open Ast
module MetaLoc =
  struct
    let meta_loc_patt _loc _ = `Id (_loc, (`Lid (_loc, "loc")))
    let meta_loc_expr _loc _ = `Id (_loc, (`Lid (_loc, "loc")))
  end
module MetaAst = FanAst.Make(MetaLoc)
let _ =
  AstFilters.register_str_item_filter
    ("lift",
      (fun ast  ->
         let _loc = FanAst.loc_of ast in
         `StExp
           (_loc,
             (`LetIn
                (_loc, (`ReNil _loc),
                  (`Bind
                     (_loc, (`Id (_loc, (`Lid (_loc, "loc")))),
                       (`Id
                          (_loc,
                            (`IdAcc
                               (_loc, (`Uid (_loc, "FanLoc")),
                                 (`Lid (_loc, "ghost")))))))),
                  (MetaAst.Expr.meta_str_item _loc ast))))))
let add_debug_expr (e : expr) =
  (let _loc = FanAst.loc_of e in
   let msg = "camlp4-debug: exc: %s at " ^ ((FanLoc.to_string _loc) ^ "@.") in
   `Try
     (_loc, e,
       (`Or
          (_loc,
            (`Case
               (_loc,
                 (`Alias
                    (_loc,
                      (`PaOrp
                         (_loc,
                           (`Id
                              (_loc,
                                (`IdAcc
                                   (_loc, (`Uid (_loc, "XStream")),
                                     (`Uid (_loc, "Failure")))))),
                           (`Id (_loc, (`Uid (_loc, "Exit")))))),
                      (`Lid (_loc, "exc")))), (`Nil _loc),
                 (`ExApp
                    (_loc, (`Id (_loc, (`Lid (_loc, "raise")))),
                      (`Id (_loc, (`Lid (_loc, "exc")))))))),
            (`Case
               (_loc, (`Id (_loc, (`Lid (_loc, "exc")))), (`Nil _loc),
                 (`Seq
                    (_loc,
                      (`Sem
                         (_loc,
                           (`IfThenElse
                              (_loc,
                                (`ExApp
                                   (_loc,
                                     (`Id
                                        (_loc,
                                          (`IdAcc
                                             (_loc, (`Uid (_loc, "Debug")),
                                               (`Lid (_loc, "mode")))))),
                                     (`Str (_loc, "exc")))),
                                (`ExApp
                                   (_loc,
                                     (`ExApp
                                        (_loc,
                                          (`Id
                                             (_loc,
                                               (`IdAcc
                                                  (_loc,
                                                    (`Uid (_loc, "Format")),
                                                    (`Lid (_loc, "eprintf")))))),
                                          (`Str
                                             (_loc,
                                               (FanAst.safe_string_escaped
                                                  msg))))),
                                     (`ExApp
                                        (_loc,
                                          (`Id
                                             (_loc,
                                               (`IdAcc
                                                  (_loc,
                                                    (`Uid (_loc, "Printexc")),
                                                    (`Lid (_loc, "to_string")))))),
                                          (`Id (_loc, (`Lid (_loc, "exc")))))))),
                                (`Id (_loc, (`Uid (_loc, "()")))))),
                           (`ExApp
                              (_loc, (`Id (_loc, (`Lid (_loc, "raise")))),
                                (`Id (_loc, (`Lid (_loc, "exc"))))))))))))))) : 
  expr )
let rec map_match_case: match_case -> match_case =
  function
  | `Or (_loc,m1,m2) -> `Or (_loc, (map_match_case m1), (map_match_case m2))
  | `Case (_loc,p,w,e) -> `Case (_loc, p, w, (add_debug_expr e))
  | m -> m
let _ =
  AstFilters.register_str_item_filter
    ("exception",
      ((object 
          inherit  FanAst.map as super
          method! expr =
            function
            | `Fun (_loc,m) -> `Fun (_loc, (map_match_case m))
            | x -> super#expr x
          method! str_item =
            function
            | `Module (_loc,`Uid (_,"Debug"),_) as st -> st
            | st -> super#str_item st
        end)#str_item))
let _ =
  AstFilters.register_str_item_filter
    ("strip", (((new FanAst.reloc) FanLoc.ghost)#str_item))
let decorate_binding decorate_fun =
  (object 
     inherit  FanAst.map as super
     method! binding =
       function
       | `Bind (_loc,`Id (_,`Lid (_,id)),(`Fun (_,_) as e)) ->
           `Bind (_loc, (`Id (_loc, (`Lid (_loc, id)))), (decorate_fun id e))
       | b -> super#binding b
   end)#binding
let decorate decorate_fun =
  object (o)
    inherit  FanAst.map as super
    method! str_item =
      function
      | `Value (_loc,r,b) ->
          `Value (_loc, r, (decorate_binding decorate_fun b))
      | st -> super#str_item st
    method! expr =
      function
      | `LetIn (_loc,r,b,e) ->
          `LetIn (_loc, r, (decorate_binding decorate_fun b), (o#expr e))
      | `Fun (_loc,_) as e -> decorate_fun "<fun>" e
      | e -> super#expr e
  end
let decorate_this_expr e id =
  let buf = Buffer.create 42 in
  let _loc = FanAst.loc_of e in
  let () = Format.bprintf buf "%s @@ %a@?" id FanLoc.dump _loc in
  let s = Buffer.contents buf in
  `LetIn
    (_loc, (`ReNil _loc),
      (`Bind
         (_loc, (`Id (_loc, (`Uid (_loc, "()")))),
           (`ExApp
              (_loc,
                (`Id
                   (_loc,
                     (`IdAcc
                        (_loc, (`Uid (_loc, "Camlp4prof")),
                          (`Lid (_loc, "count")))))),
                (`Str (_loc, (FanAst.safe_string_escaped s))))))), e)
let rec decorate_fun id =
  let decorate = decorate decorate_fun in
  let decorate_expr = decorate#expr in
  let decorate_match_case = decorate#match_case in
  function
  | `Fun (_loc,`Case (_,p,`Nil _,e)) ->
      `Fun (_loc, (`Case (_loc, p, (`Nil _loc), (decorate_fun id e))))
  | `Fun (_loc,m) ->
      decorate_this_expr (`Fun (_loc, (decorate_match_case m))) id
  | e -> decorate_this_expr (decorate_expr e) id
let _ =
  AstFilters.register_str_item_filter
    ("profile", ((decorate decorate_fun)#str_item))
let map_expr =
  function
  | `ExApp (_loc,e,`Id (_,`Uid (_,"NOTHING")))
    |`Fun (_loc,`Case (_,`Id (_,`Uid (_,"NOTHING")),`Nil _,e)) -> e
  | `Id (_loc,`Lid (_,"__FILE__")) ->
      `Str (_loc, (FanAst.safe_string_escaped (FanLoc.file_name _loc)))
  | `Id (_loc,`Lid (_,"__PWD__")) ->
      `Str
        (_loc,
          (FanAst.safe_string_escaped
             (Filename.dirname (FanLoc.file_name _loc))))
  | `Id (_loc,`Lid (_,"__LOCATION__")) ->
      let (a,b,c,d,e,f,g,h) = FanLoc.to_tuple _loc in
      `ExApp
        (_loc,
          (`Id
             (_loc,
               (`IdAcc
                  (_loc, (`Uid (_loc, "FanLoc")), (`Lid (_loc, "of_tuple")))))),
          (`Tup
             (_loc,
               (`Com
                  (_loc, (`Str (_loc, (FanAst.safe_string_escaped a))),
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
                                                       (string_of_int b))),
                                                  (`Int
                                                     (_loc,
                                                       (string_of_int c))))),
                                             (`Int (_loc, (string_of_int d))))),
                                        (`Int (_loc, (string_of_int e))))),
                                   (`Int (_loc, (string_of_int f))))),
                              (`Int (_loc, (string_of_int g))))),
                         (if h
                          then `Id (_loc, (`Lid (_loc, "true")))
                          else `Id (_loc, (`Lid (_loc, "false")))))))))))
  | e -> e
let _ =
  AstFilters.register_str_item_filter
    ("trash_nothing", ((FanAst.map_expr map_expr)#str_item))
let make_filter (s,code) =
  let f =
    function | `StExp (_loc,`Id (_,`Lid (_,s'))) when s = s' -> code | e -> e in
  (("filter_" ^ s), ((FanAst.map_str_item f)#str_item))