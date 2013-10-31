open FAst
open Ast_gen
let meta =
  object  inherit  FMeta.meta method! loc _loc _ = lid _loc "loc" end
let _ =
  Ast_filters.register_stru_filter
    ("lift",
      (fun ast  ->
         let _loc = loc_of ast in
         let e = (meta#stru _loc ast : ep  :>exp) in
         (`StExp
            (_loc,
              (`LetIn
                 (_loc, (`Negative _loc),
                   (`Bind
                      (_loc, (`Lid (_loc, "loc")),
                        (`Dot
                           (_loc, (`Uid (_loc, "Locf")),
                             (`Lid (_loc, "ghost")))))), e))) : FAst.stru )))
let _ =
  Ast_filters.register_stru_filter
    ("strip", (((new Objs.reloc) Locf.ghost)#stru))
let map_exp =
  function
  | (`App (_loc,e,`Uid (_,"NOTHING")) : FAst.exp)
    |(`Fun (_loc,`Case (_,`Uid (_,"NOTHING"),e)) : FAst.exp) -> e
  | (`Lid (_loc,"__FILE__") : FAst.exp) ->
      (`Str (_loc, (String.escaped (Locf.file_name _loc))) : FAst.exp )
  | (`Lid (_loc,"__PWD__") : FAst.exp) ->
      (`Str (_loc, (String.escaped (Filename.dirname (Locf.file_name _loc)))) : 
      FAst.exp )
  | (`Lid (_loc,"__LOCATION__") : FAst.exp) -> Ast_gen.meta_here _loc _loc
  | e -> e
let _ =
  Ast_filters.register_stru_filter
    ("trash_nothing", ((Objs.map_exp map_exp)#stru))
let make_filter (s,code) =
  let f =
    function
    | (`StExp (_loc,`Lid (_,s')) : FAst.stru) when s = s' ->
        FanAstN.fill_stru _loc code
    | e -> e in
  (("filter_" ^ s), ((Objs.map_stru f)#stru))
let me =
  object 
    inherit  FMeta.meta
    method! loc _loc loc =
      match !Ast_quotation.current_loc_name with
      | None  -> lid _loc (!Locf.name)
      | Some "here" -> meta_here _loc loc
      | Some x -> lid _loc x
  end
let mp = object  inherit  FMeta.meta method! loc _loc _ = `Any _loc end
let _ =
  Ast_filters.register_stru_filter
    ("serialize",
      (fun x  ->
         let _loc = Locf.ghost in
         let y = (me#stru _loc x : ep  :>exp) in
         (`Sem
            (_loc, x,
              (`Value
                 (_loc, (`Negative _loc),
                   (`Bind (_loc, (`Lid (_loc, "__fan_repr_of_file")), y))))) : 
           FAst.stru )))
