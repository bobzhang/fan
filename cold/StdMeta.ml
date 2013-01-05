open Ast
open LibUtil
module Expr = struct
  let meta_int _loc i = `ExInt (_loc, (string_of_int i))
  let meta_int32 _loc i = `ExInt32 (_loc, (Int32.to_string i))
  let meta_int64 _loc i = `ExInt64 (_loc, (Int64.to_string i))
  let meta_nativeint _loc i = `ExNativeInt (_loc, (Nativeint.to_string i))
  let meta_float _loc i = `ExFlo (_loc, (FanUtil.float_repres i))
  let meta_string _loc i = `ExStr (_loc, (FanAst.safe_string_escaped i))
  let meta_char _loc i = `ExChr (_loc, (Char.escaped i))
  let meta_unit _loc _ = `ExId (_loc, (`Uid (_loc, "()")))
  let meta_bool _loc =
    function
    | true  -> `ExId (_loc, (`Lid (_loc, "true")))
    | false  -> `ExId (_loc, (`Lid (_loc, "false")))
  let meta_ref mf_a _loc i =
    `ExRec
      (_loc,
        (`RbEq (_loc, (`Lid (_loc, "contents")), (mf_a _loc i.contents))),
        (`ExNil _loc))
  let mklist loc =
    let rec loop top =
      function
      | [] -> `ExId (loc, (`Uid (loc, "[]")))
      | e1::el ->
          let _loc = if top then loc else FanLoc.merge (loc_of_expr e1) loc in
          `ExApp
            (_loc, (`ExApp (_loc, (`ExId (_loc, (`Uid (_loc, "::")))), e1)),
              (loop false el)) in
    loop true
  let mkarray loc arr =
    let rec loop top =
      function
      | [] -> `ExId (loc, (`Uid (loc, "[]")))
      | e1::el ->
          let _loc = if top then loc else FanLoc.merge (loc_of_expr e1) loc in
          `ExArr (_loc, (`ExSem (_loc, e1, (loop false el)))) in
    let items = arr |> Array.to_list in loop true items
  let meta_list mf_a _loc ls =
    mklist _loc (List.map (fun x  -> mf_a _loc x) ls)
  let meta_array mf_a _loc ls =
    mkarray _loc (Array.map (fun x  -> mf_a _loc x) ls)
  let meta_option mf_a _loc =
    function
    | None  -> `ExId (_loc, (`Uid (_loc, "None")))
    | Some x ->
        `ExApp (_loc, (`ExId (_loc, (`Uid (_loc, "Some")))), (mf_a _loc x))
  let meta_arrow (type t) (_mf_a : FanLoc.t -> 'a -> t)
    (_mf_b : FanLoc.t -> 'b -> t) (_loc : FanLoc.t) (_x : 'a -> 'b) =
    invalid_arg "meta_arrow not implemented"
  end
module Patt = struct
  let meta_int _loc i = `PaInt (_loc, (string_of_int i))
  let meta_int32 _loc i = `PaInt32 (_loc, (Int32.to_string i))
  let meta_int64 _loc i = `PaInt64 (_loc, (Int64.to_string i))
  let meta_nativeint _loc i = `PaNativeInt (_loc, (Nativeint.to_string i))
  let meta_float _loc i = `PaFlo (_loc, (FanUtil.float_repres i))
  let meta_string _loc i = `PaStr (_loc, (FanAst.safe_string_escaped i))
  let meta_char _loc i = `PaChr (_loc, (Char.escaped i))
  let meta_unit _loc _ = `PaId (_loc, (`Uid (_loc, "()")))
  let meta_bool _loc =
    function
    | true  -> `PaId (_loc, (`Lid (_loc, "true")))
    | false  -> `PaId (_loc, (`Lid (_loc, "false")))
  let meta_ref mf_a _loc i =
    `PaRec
      (_loc,
        (`PaEq (_loc, (`Lid (_loc, "contents")), (mf_a _loc i.contents))))
  let mklist loc =
    let rec loop top =
      function
      | [] -> `PaId (loc, (`Uid (loc, "[]")))
      | e1::el ->
          let _loc = if top then loc else FanLoc.merge (loc_of_patt e1) loc in
          `PaApp
            (_loc, (`PaApp (_loc, (`PaId (_loc, (`Uid (_loc, "::")))), e1)),
              (loop false el)) in
    loop true
  let mkarray loc arr =
    let rec loop top =
      function
      | [] -> `PaId (loc, (`Uid (loc, "[]")))
      | e1::el ->
          let _loc = if top then loc else FanLoc.merge (loc_of_patt e1) loc in
          `PaArr (_loc, (`PaSem (_loc, e1, (loop false el)))) in
    let items = arr |> Array.to_list in loop true items
  let meta_list mf_a _loc ls =
    mklist _loc (List.map (fun x  -> mf_a _loc x) ls)
  let meta_array mf_a _loc ls =
    mkarray _loc (Array.map (fun x  -> mf_a _loc x) ls)
  let meta_option mf_a _loc =
    function
    | None  -> `PaId (_loc, (`Uid (_loc, "None")))
    | Some x ->
        `PaApp (_loc, (`PaId (_loc, (`Uid (_loc, "Some")))), (mf_a _loc x))
  let meta_arrow (type t) (_mf_a : FanLoc.t -> 'a -> t)
    (_mf_b : FanLoc.t -> 'b -> t) (_loc : FanLoc.t) (_x : 'a -> 'b) =
    invalid_arg "meta_arrow not implemented"
  end