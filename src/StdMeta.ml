open Ast;
(* module Ast = FanAst; *)
open LibUtil;
#default_quotation "expr";;


DEFINE GETLOC(x) = loc_of_expr(x);


module Expr = struct
  INCLUDE "src/MetaTemplate.ml"; (* FIXME INCLUDE as a langauge :default *)
end;
#default_quotation "patt"  ;;


DEFINE GETLOC(x) = loc_of_patt(x);
module Patt = struct
  INCLUDE "src/MetaTemplate.ml";
end;

(*
module PExpr = struct
  let meta_int _loc i = `ExInt (_loc, (string_of_int i));
  let meta_int32 _loc i = `ExInt32 (_loc, (Int32.to_string i));
  let meta_int64 _loc i = `ExInt64 (_loc, (Int64.to_string i));
  let meta_nativeint _loc i = `ExNativeInt (_loc, (Nativeint.to_string i));
  let meta_float _loc i = `ExFlo (_loc, (FanUtil.float_repres i));
  let meta_string _loc i = `ExStr (_loc, (Ast.safe_string_escaped i));
  let meta_char _loc i = `ExChr (_loc, (Char.escaped i));
  let meta_unit _loc _ = `ExId (_loc, (`IdUid (_loc, "()")));
  let meta_bool _loc =
    function
    [ true  -> `ExId (_loc, (`IdLid (_loc, "true")))
    | false  -> `ExId (_loc, (`IdLid (_loc, "false")))];
  let meta_string _loc i = `ExStr (_loc, (Ast.safe_string_escaped i));
  let mklist loc =
    let rec loop top =
      function
      [ [] -> `ExId (loc, (`IdUid (loc, "[]")))
      | [e1::el] ->
          let _loc = (* FanLoc.ghost in *) if top then loc else FanLoc.merge (loc_of_expr e1) loc in
          `ExApp
            (_loc, (`ExApp (_loc, (`ExId (_loc, (`IdUid (_loc, "::")))), e1)),
              (loop false el))] in
    loop true;
  let meta_list mf_a _loc ls =
    mklist _loc (List.map (fun x  -> mf_a _loc x) ls);
      
end;

  
module PPatt = struct
  let meta_int _loc i = `PaInt (_loc, (string_of_int i));
  let meta_int32 _loc i = `PaInt32 (_loc, (Int32.to_string i));
  let meta_int64 _loc i = `PaInt64 (_loc, (Int64.to_string i));
  let meta_nativeint _loc i = `PaNativeInt (_loc, (Nativeint.to_string i));
  let meta_float _loc i = `PaFlo (_loc, (FanUtil.float_repres i));
  let meta_string _loc i = `PaStr (_loc, (Ast.safe_string_escaped i));
  let meta_char _loc i = `PaChr (_loc, (Char.escaped i));
  let meta_unit _loc _ = `PaId (_loc, (`IdUid (_loc, "()")));
  let meta_bool _loc =
    function
    [ true  -> `PaId (_loc, (`IdLid (_loc, "true")))
    | false  -> `PaId (_loc, (`IdLid (_loc, "false")))];
  let meta_string _loc i = `PaStr (_loc, (Ast.safe_string_escaped i));
  let mklist loc =
    let rec loop top =
      function
      [ [] -> `PaId (loc, (`IdUid (loc, "[]")))
      | [e1::el] ->
          let _loc = (* FanLoc.ghost in *) if top then loc else FanLoc.merge (loc_of_patt e1) loc in
          `PaApp
            (_loc, (`PaApp (_loc, (`PaId (_loc, (`IdUid (_loc, "::")))), e1)),
              (loop false el))] in
    loop true;
  let meta_list mf_a _loc ls =
    mklist _loc (List.map (fun x  -> mf_a _loc x) ls);
      
end;
*)
