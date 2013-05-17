
(*************************************************************************)
(** antiquot filter for Ast without locations, slightly different from [Ant]
 *)  
open Ast
open FanUtil

(*************************************************************************)    
(* when the antiquotation appears in the pattern position,
   its final context is [pat] *)  
let antiquot_expander ~parse_pat ~parse_exp = object
  inherit Objs.map as super;
  method! pat (x:pat)= with pat
    match x with 
    |`Ant(_loc, {cxt;sep;decorations;content=code}) ->
      let e = parse_pat _loc code in
      (match (decorations,cxt,sep) with
      | (("uid" | "lid" | "par" | "seq"
      |"flo" |"int" | "int32" | "int64" |"nativeint"
      |"chr" |"str" as x),_,_) | (("vrn" as x), ("exp" |"pat"),_) ->
           {|$(vrn:String.capitalize x) $e |}
      | _ -> super#pat e)
    | e -> super#pat e 
  method! exp (x:exp) = with exp
    match x with 
    |`Ant(_loc,{cxt;sep;decorations;content=code}) ->
      let e = parse_exp _loc code in
      (match (decorations,cxt,sep) with
      |(("uid" | "lid" | "par" | "seq"
      |"flo" |"int" | "int32" | "int64" |"nativeint"
      |"chr" |"str" as x),_,_) | (("vrn" as x), ("exp" |"pat"),_) ->
           {|$(vrn:String.capitalize x) $e |}         
      | ("`nativeint",_,_) ->
          let e = {| Nativeint.to_string $e |} in
          {| `Nativeint  $e |}
      | ("`int",_,_) ->
          let e = {|string_of_int $e |} in
          {| `Int  $e |}
      | ("`int32",_,_) ->
          let e = {|Int32.to_string $e |} in
          {| `Int32  $e |}
      | ("`int64",_,_) ->
          let e = {|Int64.to_string $e |} in
          {| `Int64  $e |}
      | ("`chr",_,_) ->
          let e = {|Char.escaped $e|} in
          {| `Chr  $e |}
      | ("`str",_,_) ->
          let e = {|String.escaped $e |} in
          {| `Str  $e |}
      | ("`flo",_,_) ->
          let e = {| FanUtil.float_repres $e |} in
          {| `Flo  $e |}
      | ("`bool",_,_) ->
          {| `Lid (if $e then "true" else "false" ) |} 
      | _ -> super#exp e)
    | e -> super#exp e
  end
                  

