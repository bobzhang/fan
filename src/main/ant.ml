open Astf

let antiquot_expander ~parse_pat ~parse_exp = object
  inherit Objs.map as super
  method! pat (x:pat)= 
    match x with 
    |`Ant(_loc, x) ->
      let meta_loc_pat _loc _ =  %pat{ _ } in
      let mloc _loc = meta_loc_pat  _loc _loc in
      let e =
        Tokenf.ant_expand parse_pat x   in
      begin 
        match (x.kind,x.cxt) with
        | (("uid" | "lid" | "par" | "seq"
        |"flo" |"int" | "int32" | "int64" |"nativeint"
        |"chr" |"str" as x),_) |
          (("vrn" as x), Some ("exp" |"pat")) ->
            let x = String.capitalize x in
            %pat{ $vrn:x (${mloc _loc},$e) }
        | _ -> super#pat e
      end
    | e -> super#pat e 
  method! exp (x:exp) =  with exp
    match x with 
    |`Ant(_loc, x) ->
        let meta_loc_exp _loc loc =
          match !Ast_quotation.current_loc_name with
          | Some "here" -> (Ast_gen.meta_here _loc loc :> exp)
          | x ->
              let x = Option.default !Locf.name  x in
              %exp{$lid:x} in
      let mloc _loc = meta_loc_exp _loc _loc  in (* FIXME Simplify*)
      let e = Tokenf.ant_expand parse_exp x in
      (match (x.kind,x.cxt) with
      |(("uid" | "lid" | "par" | "seq"
      |"flo" |"int" | "int32" | "int64" |"nativeint"
      |"chr" |"str" as x),_) | (("vrn" as x), Some ("exp" |"pat")) ->
           %{$vrn{String.capitalize x} (${mloc _loc},$e) }
      | ("nativeint'",_) ->
          let e = %{ Nativeint.to_string $e } in
          %{ `Nativeint (${mloc _loc}, $e) }
      | ("int'",_) ->
          let e = %{string_of_int $e } in
          %{ `Int (${mloc _loc}, $e) }
      | ("int32'",_) ->
          let e = %{Int32.to_string $e } in
          %{ `Int32 (${mloc _loc}, $e) }
      | ("int64'",_) ->

          let e = %{Int64.to_string $e } in
          %{ `Int64 (${mloc _loc}, $e) }
      | ("chr'",_) ->
          let e = %{Char.escaped $e} in
          %{ `Chr (${mloc _loc}, $e) }
      | ("str'",_) ->
          let e = %{String.escaped $e } in
          %{ `Str (${mloc _loc}, $e) }
      | ("flo'",_) ->
          let e = %{ string_of_float $e } in
          %{ `Flo (${mloc _loc}, $e) }
      | ("bool'",_) ->
          %exp{`Bool (${mloc _loc}, $e) }
      | _ -> super#exp e)
    | e -> super#exp e
  end


let stringnize  = [
  ("nativeint'",Some %exp-{Nativeint.to_string});
  ("int'", Some %exp-{string_of_int });
  ("int32'",Some %exp-{Int32.to_string});
  ("int64'",Some %exp-{Int64.to_string});
  ("chr'",Some %exp-{Char.escaped});
  ("str'",Some %exp-{String.escaped});
  ("flo'",Some %exp-{string_of_float});
  ("bool'",None)
 ]

    
let expandern ~parse_pat ~parse_exp = object
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
      | (("nativeint'" | "int'"|"int32'"|"int64'"|"chr'"|"str'"|"flo'"|"bool'" as x),_) ->
          let v =
            match List.assoc x stringnize with
            | Some x -> let  x = FanAstN.fill_exp _loc x in %exp{$x $e}
            | None -> e in
          let s = String.sub x 0 (String.length x - 1) |> String.capitalize in
          %exp{$vrn:s $v}
      | _ -> super#exp e)
    | e -> super#exp e
  end
    

(* local variables: *)
(* compile-command: "cd .. && pmake main_annot/ant.cmo" *)
(* end: *)
