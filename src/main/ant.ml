open FAst

open FanUtil

let antiquot_expander ~parse_pat ~parse_exp = object
  inherit Objs.map as super
  method! pat (x:pat)= 
    match x with 
    |`Ant(_loc, {cxt;sep;decorations;content=code}) ->
      let meta_loc_pat _loc _ =  %pat{ _ } in
      let mloc _loc = meta_loc_pat  _loc _loc in
      let e = parse_pat _loc code in
      (match (decorations,cxt,sep) with
      | (("uid" | "lid" | "par" | "seq"
         |"flo" |"int" | "int32" | "int64" |"nativeint"
         |"chr" |"str" as x),_,_) |
         (("vrn" as x), ("exp" |"pat"),_) ->
           let x = String.capitalize x in
           %pat{ $vrn:x ($(mloc _loc),$e) }
      | _ -> super#pat e)
    | e -> super#pat e 
  method! exp (x:exp) =  with exp
    match x with 
    |`Ant(_loc,{cxt;sep;decorations;content=code}) ->
        let meta_loc_exp _loc loc =
          match !Ast_quotation.current_loc_name with
          | Some "here" -> Ast_gen.meta_here _loc loc
          | x ->
              let x = Option.default !Locf.name  x in
              %exp{$lid:x} in
      let mloc _loc = meta_loc_exp _loc _loc  in
      let e = parse_exp _loc code in
      (match (decorations,cxt,sep) with
      |(("uid" | "lid" | "par" | "seq"
      |"flo" |"int" | "int32" | "int64" |"nativeint"
      |"chr" |"str" as x),_,_) | (("vrn" as x), ("exp" |"pat"),_) ->
           %{$(vrn:String.capitalize x) ($(mloc _loc),$e) }         
      | ("`nativeint",_,_) ->
          let e = %{ Nativeint.to_string $e } in
          %{ `Nativeint ($(mloc _loc), $e) }
      | ("`int",_,_) ->
          let e = %{string_of_int $e } in
          %{ `Int ($(mloc _loc), $e) }
      | ("`int32",_,_) ->
          let e = %{Int32.to_string $e } in
          %{ `Int32 ($(mloc _loc), $e) }
      | ("`int64",_,_) ->
          let e = %{Int64.to_string $e } in
          %{ `Int64 ($(mloc _loc), $e) }

      | ("`chr",_,_) ->
          let e = %{Char.escaped $e} in
          %{ `Chr ($(mloc _loc), $e) }

      | ("`str",_,_) ->
          let e = %{String.escaped $e } in
          %{ `Str ($(mloc _loc), $e) }

      | ("`flo",_,_) ->
          let e = %{ string_of_float $e } in
          %{ `Flo ($(mloc _loc), $e) }
      | ("`bool",_,_) ->
            %{ `Lid ($(mloc _loc), (if $e then "true" else "false" )) }
      | _ -> super#exp e)
    | e -> super#exp e
  end


(* local variables: *)
(* compile-command: "cd .. && pmake main_annot/ant.cmo" *)
(* end: *)
