open LibUtil
open AstLoc
let meta =
  object 
    inherit  FanMeta.meta
    method! loc _loc _ = `Id (_loc, (`Lid (_loc, "loc")))
  end
let _ =
  AstFilters.register_stru_filter
    ("lift",
      (fun ast  ->
         let _loc = loc_of ast in
         let e = (meta#stru _loc ast : ep  :>exp) in
         `StExp
           (_loc,
             (`LetIn
                (_loc, (`ReNil _loc),
                  (`Bind
                     (_loc, (`Id (_loc, (`Lid (_loc, "loc")))),
                       (`Id
                          (_loc,
                            (`Dot
                               (_loc, (`Uid (_loc, "FanLoc")),
                                 (`Lid (_loc, "ghost")))))))), e)))))
let add_debug_exp (e : exp) =
  (let _loc = loc_of e in
   let msg = "camlp4-debug: exc: %s at " ^ ((FanLoc.to_string _loc) ^ "@.") in
   `Try
     (_loc, e,
       (`Or
          (_loc,
            (`Case
               (_loc,
                 (`Alias
                    (_loc,
                      (`Or
                         (_loc,
                           (`Id
                              (_loc,
                                (`Dot
                                   (_loc, (`Uid (_loc, "XStream")),
                                     (`Uid (_loc, "Failure")))))),
                           (`Id (_loc, (`Uid (_loc, "Exit")))))),
                      (`Lid (_loc, "exc")))),
                 (`App
                    (_loc, (`Id (_loc, (`Lid (_loc, "raise")))),
                      (`Id (_loc, (`Lid (_loc, "exc")))))))),
            (`Case
               (_loc, (`Id (_loc, (`Lid (_loc, "exc")))),
                 (`Seq
                    (_loc,
                      (`Sem
                         (_loc,
                           (`IfThenElse
                              (_loc,
                                (`App
                                   (_loc,
                                     (`Id
                                        (_loc,
                                          (`Dot
                                             (_loc, (`Uid (_loc, "Debug")),
                                               (`Lid (_loc, "mode")))))),
                                     (`Str (_loc, "exc")))),
                                (`App
                                   (_loc,
                                     (`App
                                        (_loc,
                                          (`Id
                                             (_loc,
                                               (`Dot
                                                  (_loc,
                                                    (`Uid (_loc, "Format")),
                                                    (`Lid (_loc, "eprintf")))))),
                                          (`Str (_loc, (String.escaped msg))))),
                                     (`App
                                        (_loc,
                                          (`Id
                                             (_loc,
                                               (`Dot
                                                  (_loc,
                                                    (`Uid (_loc, "Printexc")),
                                                    (`Lid (_loc, "to_string")))))),
                                          (`Id (_loc, (`Lid (_loc, "exc")))))))),
                                (`Id (_loc, (`Uid (_loc, "()")))))),
                           (`App
                              (_loc, (`Id (_loc, (`Lid (_loc, "raise")))),
                                (`Id (_loc, (`Lid (_loc, "exc"))))))))))))))) : 
  exp )
let rec map_case: case -> case =
  function
  | `Or (_loc,m1,m2) -> `Or (_loc, (map_case m1), (map_case m2))
  | `Case (_loc,p,e) -> `Case (_loc, p, (add_debug_exp e))
  | `CaseWhen (_loc,p,w,e) -> `CaseWhen (_loc, p, w, (add_debug_exp e))
  | m -> m
let _ =
  AstFilters.register_stru_filter
    ("exception",
      ((object 
          inherit  Objs.map as super
          method! exp =
            function
            | `Fun (_loc,m) -> `Fun (_loc, (map_case m))
            | x -> super#exp x
          method! stru =
            function
            | `Module (_loc,`Uid (_,"Debug"),_) as st -> st
            | st -> super#stru st
        end)#stru))
let _ =
  AstFilters.register_stru_filter
    ("strip", (((new FanObjs.reloc) FanLoc.ghost)#stru))
let decorate_binding decorate_fun =
  (object 
     inherit  Objs.map as super
     method! binding =
       function
       | `Bind (_loc,(`Id (_,`Lid (_,id)) as x),(`Fun (_,_) as e)) ->
           `Bind (_loc, x, (decorate_fun id e))
       | b -> super#binding b
   end)#binding
let decorate decorate_fun =
  object (o)
    inherit  Objs.map as super
    method! stru =
      function
      | `Value (_loc,r,b) ->
          `Value (_loc, r, (decorate_binding decorate_fun b))
      | st -> super#stru st
    method! exp =
      function
      | `LetIn (_loc,r,b,e) ->
          `LetIn (_loc, r, (decorate_binding decorate_fun b), (o#exp e))
      | `Fun (_loc,_) as e -> decorate_fun "<fun>" e
      | e -> super#exp e
  end
let decorate_this_exp e id =
  let buf = Buffer.create 42 in
  let _loc = loc_of e in
  let () = Format.bprintf buf "%s @@ %a@?" id FanLoc.dump _loc in
  let s = Buffer.contents buf in
  `LetIn
    (_loc, (`ReNil _loc),
      (`Bind
         (_loc, (`Id (_loc, (`Uid (_loc, "()")))),
           (`App
              (_loc,
                (`Id
                   (_loc,
                     (`Dot
                        (_loc, (`Uid (_loc, "Camlp4prof")),
                          (`Lid (_loc, "count")))))),
                (`Str (_loc, (String.escaped s))))))), e)
let rec decorate_fun id =
  let decorate = decorate decorate_fun in
  let decorate_exp = decorate#exp in
  let decorate_case = decorate#case in
  function
  | `Fun (_loc,`Case (_,p,e)) ->
      `Fun (_loc, (`Case (_loc, p, (decorate_fun id e))))
  | `Fun (_loc,m) -> decorate_this_exp (`Fun (_loc, (decorate_case m))) id
  | e -> decorate_this_exp (decorate_exp e) id
let _ =
  AstFilters.register_stru_filter ("profile", ((decorate decorate_fun)#stru))
let map_exp =
  function
  | `App (_loc,e,`Id (_,`Uid (_,"NOTHING")))
    |`Fun (_loc,`Case (_,`Id (_,`Uid (_,"NOTHING")),e)) -> e
  | `Id (_loc,`Lid (_,"__FILE__")) ->
      `Str (_loc, (String.escaped (FanLoc.file_name _loc)))
  | `Id (_loc,`Lid (_,"__PWD__")) ->
      `Str
        (_loc, (String.escaped (Filename.dirname (FanLoc.file_name _loc))))
  | `Id (_loc,`Lid (_,"__LOCATION__")) ->
      let (a,b,c,d,e,f,g,h) = FanLoc.to_tuple _loc in
      `App
        (_loc,
          (`Id
             (_loc,
               (`Dot
                  (_loc, (`Uid (_loc, "FanLoc")), (`Lid (_loc, "of_tuple")))))),
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
  AstFilters.register_stru_filter
    ("trash_nothing", ((FanObjs.map_exp map_exp)#stru))
let make_filter (s,code) =
  let f =
    function | `StExp (_loc,`Id (_,`Lid (_,s'))) when s = s' -> code | e -> e in
  (("filter_" ^ s), ((FanObjs.map_stru f)#stru))
let me =
  object 
    inherit  FanMeta.meta
    method! loc _loc loc =
      match AstQuotation.current_loc_name.contents with
      | None  -> `Id (_loc, (`Lid (_loc, (FanLoc.name.contents))))
      | Some "here" -> Lib.Meta.meta_loc _loc loc
      | Some x -> `Id (_loc, (`Lid (_loc, x)))
  end
let mp = object  inherit  FanMeta.meta method! loc _loc _ = `Any _loc end
let _ =
  AstFilters.register_stru_filter
    ("serialize",
      (fun x  ->
         let _loc = FanLoc.ghost in
         let y = (me#stru _loc x : ep  :>exp) in
         `Sem
           (_loc, x,
             (`Value
                (_loc, (`ReNil _loc),
                  (`Bind
                     (_loc,
                       (`Id (_loc, (`Lid (_loc, "__fan_repr_of_file")))), y)))))))