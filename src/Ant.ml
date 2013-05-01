open Ast;
open FanUtil;
open AstLoc;

let meta_loc_exp _loc loc =
  match !AstQuotation.current_loc_name with
  | None -> lid _loc !FanLoc.name
  | Some "here" -> FanMeta.meta_loc _loc loc
  | Some x -> lid _loc x  ;;

(* we use [subst_first_loc] *)
let meta_loc_pat _loc _ =  {:pat| _ |}; 


(* when the antiquotation appears in the pattern position,
   its final context is [pat] *)  
let antiquot_expander ~parse_pat ~parse_exp = object
  inherit Objs.map as super;
  method! pat (x:pat)= with pat'
    match x with 
    |`Ant(_loc, {cxt;sep;decorations;content=code}) ->
      let mloc _loc =meta_loc_pat  _loc _loc in
      let e = parse_pat _loc code in
      begin match (decorations,cxt,sep) with
      | (("uid" | "lid" | "par" | "seq"
      |"flo" |"int" | "int32" | "int64" |"nativeint"
      |"chr" |"str" as x),_,_) | (("vrn" as x), ("exp" |"pat"),_) ->
           {|$(vrn:String.capitalize x) ($(mloc _loc),$e) |}
      | _ -> super#pat e end 
    | e -> super#pat e ;
  method! exp (x:exp) = with exp'
    match x with 
    |`Ant(_loc,{cxt;sep;decorations;content=code}) ->
      let mloc _loc = (meta_loc_exp _loc _loc :> exp) in
      let e = parse_exp _loc code in
      begin match (decorations,cxt,sep) with
      |(("uid" | "lid" | "par" | "seq"
      |"flo" |"int" | "int32" | "int64" |"nativeint"
      |"chr" |"str" as x),_,_) | (("vrn" as x), ("exp" |"pat"),_) ->
           {|$(vrn:String.capitalize x) ($(mloc _loc),$e) |}         
      | ("`nativeint",_,_) ->
          let e = {| Nativeint.to_string $e |} in
          {| `Nativeint ($(mloc _loc), $e) |}
      | ("`int",_,_) ->
          let e = {|string_of_int $e |} in
          {| `Int ($(mloc _loc), $e) |}
      | ("`int32",_,_) ->
          let e = {|Int32.to_string $e |} in
          {| `Int32 ($(mloc _loc), $e) |}
      | ("`int64",_,_) ->
          let e = {|Int64.to_string $e |} in
          {| `Int64 ($(mloc _loc), $e) |}
      | ("`chr",_,_) ->
          let e = {|Char.escaped $e|} in
          {| `Chr ($(mloc _loc), $e) |}
      | ("`str",_,_) ->
          let e = {|String.escaped $e |} in
          {| `Str ($(mloc _loc), $e) |}
      | ("`flo",_,_) ->
          let e = {| FanUtil.float_repres $e |} in
          {| `Flo ($(mloc _loc), $e) |}
      | ("`bool",_,_) ->
          let x = {| `Lid ($(mloc _loc), (if $e then "true" else "false" )) |} in
          {| {| $(id:$x)  |} |}
      | _ -> super#exp e end
    | e -> super#exp e;
  end;;
                  

