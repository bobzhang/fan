open LibUtil

open Ast

open AstLoc

let meta =
  object  inherit  FanMeta.meta method! loc _loc _ = lid _loc "loc" end

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

let _ =
  AstFilters.register_stru_filter
    ("strip", (((new Objs.reloc) FanLoc.ghost)#stru))

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
    ("trash_nothing", ((Objs.map_exp map_exp)#stru))

let make_filter (s,code) =
  let f =
    function | `StExp (_loc,`Id (_,`Lid (_,s'))) when s = s' -> code | e -> e in
  (("filter_" ^ s), ((Objs.map_stru f)#stru))

let me =
  object 
    inherit  FanMeta.meta
    method! loc _loc loc =
      match AstQuotation.current_loc_name.contents with
      | None  -> lid _loc FanLoc.name.contents
      | Some "here" -> FanMeta.meta_loc _loc loc
      | Some x -> lid _loc x
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