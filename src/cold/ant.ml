open Astf
let stringnize =
  [("nativeint'",
     (Some (`Dot ((`Uid "Nativeint"), (`Lid "to_string")) : Astfn.exp )));
  ("int'", (Some (`Lid "string_of_int" : Astfn.exp )));
  ("int32'", (Some (`Dot ((`Uid "Int32"), (`Lid "to_string")) : Astfn.exp )));
  ("int64'", (Some (`Dot ((`Uid "Int64"), (`Lid "to_string")) : Astfn.exp )));
  ("chr'", (Some (`Dot ((`Uid "Char"), (`Lid "escaped")) : Astfn.exp )));
  ("str'", (Some (`Dot ((`Uid "String"), (`Lid "escaped")) : Astfn.exp )));
  ("flo'", (Some (`Lid "string_of_float" : Astfn.exp )));
  ("bool'", None)]
let antiquot_expander ~parse_pat  ~parse_exp  =
  object 
    inherit  Objs.map as super
    method! pat (x : pat) =
      match x with
      | `Ant (_loc,x) ->
          let meta_loc_pat _loc _ = (`Any _loc : Astf.pat ) in
          let mloc _loc = meta_loc_pat _loc _loc in
          let e = Tokenf.ant_expand parse_pat x in
          (match ((x.kind), (x.cxt)) with
           | (("uid"|"lid"|"par"|"seq"|"flo"|"int"|"int32"|"int64"
               |"nativeint"|"chr"|"str" as x),_)
             |(("vrn" as x),Some ("exp"|"pat")) ->
               let x = String.capitalize x in
               (`App
                  (_loc, (`Vrn (_loc, x)),
                    (`Par (_loc, (`Com (_loc, (mloc _loc), e))))) : Astf.pat )
           | _ -> super#pat e)
      | e -> super#pat e
    method! exp (x : exp) =
      match x with
      | `Ant (_loc,x) ->
          let meta_loc_exp _loc loc =
            match !Ast_quotation.current_loc_name with
            | Some "here" -> (Ast_gen.meta_here _loc loc :>exp)
            | x ->
                let x = Option.default (!Locf.name) x in
                (`Lid (_loc, x) : Astf.exp ) in
          let mloc _loc = meta_loc_exp _loc _loc in
          let e = Tokenf.ant_expand parse_exp x in
          (match ((x.kind), (x.cxt)) with
           | (("uid"|"lid"|"par"|"seq"|"flo"|"int"|"int32"|"int64"
               |"nativeint"|"chr"|"str" as x),_)
             |(("vrn" as x),Some ("exp"|"pat")) ->
               (`App
                  (_loc, (`Vrn (_loc, (String.capitalize x))),
                    (`Par (_loc, (`Com (_loc, (mloc _loc), e))))) : Astf.exp )
           | (("nativeint'"|"int'"|"int32'"|"int64'"|"chr'"|"str'"|"flo'"
               |"bool'" as x),_) ->
               let v =
                 match List.assoc x stringnize with
                 | Some x ->
                     let x = FanAstN.fill_exp _loc x in
                     (`App (_loc, x, e) : Astf.exp )
                 | None  -> e in
               let s =
                 (String.sub x 0 ((String.length x) - 1)) |>
                   String.capitalize in
               (`App
                  (_loc, (`Vrn (_loc, s)),
                    (`Par (_loc, (`Com (_loc, (mloc _loc), v))))) : Astf.exp )
           | _ -> super#exp e)
      | e -> super#exp e
  end
let expandern ~parse_pat  ~parse_exp  =
  object 
    inherit  Objs.map as super
    method! pat (x : pat) =
      match x with
      | `Ant (_loc,x) ->
          let e = Tokenf.ant_expand parse_pat x in
          (match ((x.kind), (x.cxt)) with
           | (("uid"|"lid"|"par"|"seq"|"flo"|"int"|"int32"|"int64"
               |"nativeint"|"chr"|"str" as x),_)
             |(("vrn" as x),Some ("exp"|"pat")) ->
               let x = String.capitalize x in
               (`App (_loc, (`Vrn (_loc, x)), e) : Astf.pat )
           | _ -> super#pat e)
      | e -> super#pat e
    method! exp (x : exp) =
      match x with
      | `Ant (_loc,x) ->
          let e = Tokenf.ant_expand parse_exp x in
          (match ((x.kind), (x.cxt)) with
           | (("uid"|"lid"|"par"|"seq"|"flo"|"int"|"int32"|"int64"
               |"nativeint"|"chr"|"str" as x),_)
             |(("vrn" as x),Some ("exp"|"pat")) ->
               (`App (_loc, (`Vrn (_loc, (String.capitalize x))), e) : 
               Astf.exp )
           | (("nativeint'"|"int'"|"int32'"|"int64'"|"chr'"|"str'"|"flo'"
               |"bool'" as x),_) ->
               let v =
                 match List.assoc x stringnize with
                 | Some x ->
                     let x = FanAstN.fill_exp _loc x in
                     (`App (_loc, x, e) : Astf.exp )
                 | None  -> e in
               let s =
                 (String.sub x 0 ((String.length x) - 1)) |>
                   String.capitalize in
               (`App (_loc, (`Vrn (_loc, s)), v) : Astf.exp )
           | _ -> super#exp e)
      | e -> super#exp e
  end
