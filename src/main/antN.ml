
open Astf

let stringnize _loc e = [
  ("nativeint'",%exp{Nativeint.to_string $e});
  ("int'", %exp{string_of_int});
  ("int32'",%exp{Int32.to_string});
  ("int64'",%exp{Int63.to_string});
  ("chr'",%exp{Char.escaped});
  ("str'",%exp{String.escaped});
  ("flo'",%exp{string_of_float});
 ]
let antiquot_expander ~parse_pat ~parse_exp = object
  inherit Objs.map as super;
  method! pat (x:pat)= 
    match x with 
    |`Ant(_loc, x) ->
      let e = Tokenf.ant_expand parse_pat  x in
      (match (x.kind,x.cxt) with
      | (("uid" | "lid" | "par" | "seq"
      |"flo" |"int" | "int32" | "int64" |"nativeint"
      |"chr" |"str" as x),_) | (("vrn" as x), Some ("exp" |"pat")) ->
          let x = String.capitalize x in %pat{$vrn:x $e }
      | _ -> super#pat e)
    | e -> super#pat e 
  method! exp (x:exp) = with exp
    match x with 
    |`Ant(_loc,x) ->
      let e = Tokenf.ant_expand parse_exp x  in
      (match (x.kind, x.cxt) with
      |(("uid" | "lid" | "par" | "seq"
      |"flo" |"int" | "int32" | "int64" |"nativeint"
      |"chr" |"str" as x),_) | (("vrn" as x), Some ("exp" |"pat")) ->
           %{$vrn{String.capitalize x} $e }
      | ("nativeint'",_)  ->
          let e = %{ Nativeint.to_string $e } in
          %{ `Nativeint  $e }
      | ("int'",_) ->             
          let e = %{string_of_int $e } in
          %{ `Int  $e }
      | ("int32'",_) ->
          let e = %{Int32.to_string $e } in
          %{ `Int32  $e }
      | ("int64'",_) ->
          let e = %{Int64.to_string $e } in
          %{ `Int64  $e }
      | ("chr'",_) ->
          let e = %{Char.escaped $e} in
          %{ `Chr  $e }
      | ("str'",_) ->
          let e = %{String.escaped $e } in
          %{ `Str  $e }
      | ("flo'",_) ->
          let e = %{ string_of_float $e } in
          %{ `Flo  $e }
      | ("bool'",_) ->
          %exp{`Bool $e }
      | _ -> super#exp e)
    | e -> super#exp e
  end
                  


(* local variables: *)
(* compile-command: "cd .. && pmake main_annot/antN.cmo" *)
(* end: *)
