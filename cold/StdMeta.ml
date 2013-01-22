open Ast
open LibUtil
module Expr =
  struct
    let meta_int _loc i = `Int (_loc, (string_of_int i))
    let meta_int32 _loc i = `Int32 (_loc, (Int32.to_string i))
    let meta_int64 _loc i = `Int64 (_loc, (Int64.to_string i))
    let meta_nativeint _loc i = `NativeInt (_loc, (Nativeint.to_string i))
    let meta_float _loc i = `Flo (_loc, (FanUtil.float_repres i))
    let meta_string _loc i = `Str (_loc, (FanAst.safe_string_escaped i))
    let meta_char _loc i = `Chr (_loc, (Char.escaped i))
    let meta_unit _loc _ = `Id (_loc, (`Uid (_loc, "()")))
    let meta_bool _loc =
      function
      | true  -> `Id (_loc, (`Lid (_loc, "true")))
      | false  -> `Id (_loc, (`Lid (_loc, "false")))
    let meta_ref mf_a _loc i =
      `Record
        (_loc,
          (`RecBind (_loc, (`Lid (_loc, "contents")), (mf_a _loc i.contents))),
          (`Nil _loc))
    let mklist loc =
      let rec loop top =
        function
        | [] -> `Id (loc, (`Uid (loc, "[]")))
        | e1::el ->
            let _loc = if top then loc else FanLoc.merge (loc_of e1) loc in
            `App
              (_loc, (`App (_loc, (`Id (_loc, (`Uid (_loc, "::")))), e1)),
                (loop false el)) in
      loop true
    let mkarray loc arr =
      let rec loop top =
        function
        | [] -> `Id (loc, (`Uid (loc, "[]")))
        | e1::el ->
            let _loc = if top then loc else FanLoc.merge (loc_of e1) loc in
            `Array (_loc, (`Sem (_loc, e1, (loop false el)))) in
      let items = arr |> Array.to_list in loop true items
    let meta_list mf_a _loc ls =
      mklist _loc (List.map (fun x  -> mf_a _loc x) ls)
    let meta_array mf_a _loc ls =
      mkarray _loc (Array.map (fun x  -> mf_a _loc x) ls)
    let meta_option mf_a _loc =
      function
      | None  -> `Id (_loc, (`Uid (_loc, "None")))
      | Some x ->
          `App (_loc, (`Id (_loc, (`Uid (_loc, "Some")))), (mf_a _loc x))
    let meta_arrow (type t) (_mf_a : FanLoc.t -> 'a -> t)
      (_mf_b : FanLoc.t -> 'b -> t) (_loc : FanLoc.t) (_x : 'a -> 'b) =
      invalid_arg "meta_arrow not implemented"
  end
module Patt =
  struct
    let meta_int _loc i = `Int (_loc, (string_of_int i))
    let meta_int32 _loc i = `Int32 (_loc, (Int32.to_string i))
    let meta_int64 _loc i = `Int64 (_loc, (Int64.to_string i))
    let meta_nativeint _loc i = `NativeInt (_loc, (Nativeint.to_string i))
    let meta_float _loc i = `Flo (_loc, (FanUtil.float_repres i))
    let meta_string _loc i = `Str (_loc, (FanAst.safe_string_escaped i))
    let meta_char _loc i = `Chr (_loc, (Char.escaped i))
    let meta_unit _loc _ = `Id (_loc, (`Uid (_loc, "()")))
    let meta_bool _loc =
      function
      | true  -> `Id (_loc, (`Lid (_loc, "true")))
      | false  -> `Id (_loc, (`Lid (_loc, "false")))
    let meta_ref mf_a _loc i =
      `PaRec
        (_loc,
          (`PaEq (_loc, (`Lid (_loc, "contents")), (mf_a _loc i.contents))))
    let mklist loc =
      let rec loop top =
        function
        | [] -> `Id (loc, (`Uid (loc, "[]")))
        | e1::el ->
            let _loc = if top then loc else FanLoc.merge (loc_of e1) loc in
            `App
              (_loc, (`App (_loc, (`Id (_loc, (`Uid (_loc, "::")))), e1)),
                (loop false el)) in
      loop true
    let mkarray loc arr =
      let rec loop top =
        function
        | [] -> `Id (loc, (`Uid (loc, "[]")))
        | e1::el ->
            let _loc = if top then loc else FanLoc.merge (loc_of e1) loc in
            `Array (_loc, (`Sem (_loc, e1, (loop false el)))) in
      let items = arr |> Array.to_list in loop true items
    let meta_list mf_a _loc ls =
      mklist _loc (List.map (fun x  -> mf_a _loc x) ls)
    let meta_array mf_a _loc ls =
      mkarray _loc (Array.map (fun x  -> mf_a _loc x) ls)
    let meta_option mf_a _loc =
      function
      | None  -> `Id (_loc, (`Uid (_loc, "None")))
      | Some x ->
          `App (_loc, (`Id (_loc, (`Uid (_loc, "Some")))), (mf_a _loc x))
    let meta_arrow (type t) (_mf_a : FanLoc.t -> 'a -> t)
      (_mf_b : FanLoc.t -> 'b -> t) (_loc : FanLoc.t) (_x : 'a -> 'b) =
      invalid_arg "meta_arrow not implemented"
  end