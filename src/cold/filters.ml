open Astf
open Ast_gen
let meta =
  object  inherit  FMeta.meta method! loc _loc _ = lid _loc "loc" end
let make_filter (s,code) =
  let f =
    function
    | (`StExp (_loc,`Lid (_,s')) : Astf.stru) when s = s' ->
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
           Astf.stru )))
