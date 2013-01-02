module Ast = Camlp4Ast
open LibUtil
module Expr = struct
  let meta_int _loc i = Ast.ExInt (_loc, (string_of_int i))
  let meta_int32 _loc i = Ast.ExInt32 (_loc, (Int32.to_string i))
  let meta_int64 _loc i = Ast.ExInt64 (_loc, (Int64.to_string i))
  let meta_nativeint _loc i = Ast.ExNativeInt (_loc, (Nativeint.to_string i))
  let meta_float _loc i = Ast.ExFlo (_loc, (FanUtil.float_repres i))
  let meta_string _loc i = Ast.ExStr (_loc, (Ast.safe_string_escaped i))
  let meta_char _loc i = Ast.ExChr (_loc, (Char.escaped i))
  let meta_unit _loc _ = Ast.ExId (_loc, (Ast.IdUid (_loc, "()")))
  let meta_bool _loc =
    function
    | true  -> Ast.ExId (_loc, (Ast.IdLid (_loc, "true")))
    | false  -> Ast.ExId (_loc, (Ast.IdLid (_loc, "false")))
  let meta_ref mf_a _loc i =
    Ast.ExRec
      (_loc,
        (Ast.RbEq
           (_loc, (Ast.IdLid (_loc, "contents")), (mf_a _loc i.contents))),
        (Ast.ExNil _loc))
  let meta_list mf_a _loc ls =
    Lib.Expr.mklist _loc (List.map (fun x  -> mf_a _loc x) ls)
  let meta_array mf_a _loc ls =
    Lib.Expr.mkarray _loc (Array.map (fun x  -> mf_a _loc x) ls)
  let meta_option mf_a _loc =
    function
    | None  -> Ast.ExId (_loc, (Ast.IdUid (_loc, "None")))
    | Some x ->
        Ast.ExApp
          (_loc, (Ast.ExId (_loc, (Ast.IdUid (_loc, "Some")))),
            (mf_a _loc x))
  let meta_arrow (type t) (_mf_a : FanLoc.t -> 'a -> t)
    (_mf_b : FanLoc.t -> 'b -> t) (_loc : FanLoc.t) (_x : 'a -> 'b) =
    invalid_arg "meta_arrow not implemented"
  end
module Patt = struct
  let meta_int _loc i = Ast.PaInt (_loc, (string_of_int i))
  let meta_int32 _loc i = Ast.PaInt32 (_loc, (Int32.to_string i))
  let meta_int64 _loc i = Ast.PaInt64 (_loc, (Int64.to_string i))
  let meta_nativeint _loc i = Ast.PaNativeInt (_loc, (Nativeint.to_string i))
  let meta_float _loc i = Ast.PaFlo (_loc, (FanUtil.float_repres i))
  let meta_string _loc i = Ast.PaStr (_loc, (Ast.safe_string_escaped i))
  let meta_char _loc i = Ast.PaChr (_loc, (Char.escaped i))
  let meta_unit _loc _ = Ast.PaId (_loc, (Ast.IdUid (_loc, "()")))
  let meta_bool _loc =
    function
    | true  -> Ast.PaId (_loc, (Ast.IdLid (_loc, "true")))
    | false  -> Ast.PaId (_loc, (Ast.IdLid (_loc, "false")))
  let meta_ref mf_a _loc i =
    Ast.PaRec
      (_loc,
        (Ast.PaEq
           (_loc, (Ast.IdLid (_loc, "contents")), (mf_a _loc i.contents))))
  let meta_list mf_a _loc ls =
    Lib.Patt.mklist _loc (List.map (fun x  -> mf_a _loc x) ls)
  let meta_array mf_a _loc ls =
    Lib.Patt.mkarray _loc (Array.map (fun x  -> mf_a _loc x) ls)
  let meta_option mf_a _loc =
    function
    | None  -> Ast.PaId (_loc, (Ast.IdUid (_loc, "None")))
    | Some x ->
        Ast.PaApp
          (_loc, (Ast.PaId (_loc, (Ast.IdUid (_loc, "Some")))),
            (mf_a _loc x))
  let meta_arrow (type t) (_mf_a : FanLoc.t -> 'a -> t)
    (_mf_b : FanLoc.t -> 'b -> t) (_loc : FanLoc.t) (_x : 'a -> 'b) =
    invalid_arg "meta_arrow not implemented"
  end