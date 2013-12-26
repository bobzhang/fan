open Astf
let stringnize _loc e =
  [("nativeint'",
     (`App
        (_loc,
          (`Dot
             (_loc, (`Uid (_loc, "Nativeint")), (`Lid (_loc, "to_string")))),
          e) : Astf.exp ));
  ("int'", (`Lid (_loc, "string_of_int") : Astf.exp ));
  ("int32'",
    (`Dot (_loc, (`Uid (_loc, "Int32")), (`Lid (_loc, "to_string"))) : 
    Astf.exp ));
  ("int64'",
    (`Dot (_loc, (`Uid (_loc, "Int63")), (`Lid (_loc, "to_string"))) : 
    Astf.exp ));
  ("chr'",
    (`Dot (_loc, (`Uid (_loc, "Char")), (`Lid (_loc, "escaped"))) : Astf.exp ));
  ("str'",
    (`Dot (_loc, (`Uid (_loc, "String")), (`Lid (_loc, "escaped"))) : 
    Astf.exp ));
  ("flo'", (`Lid (_loc, "string_of_float") : Astf.exp ))]
let antiquot_expander ~parse_pat  ~parse_exp  =
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
           | ("nativeint'",_) ->
               let e: Astf.exp =
                 `App
                   (_loc,
                     (`Dot
                        (_loc, (`Uid (_loc, "Nativeint")),
                          (`Lid (_loc, "to_string")))), e) in
               (`App (_loc, (`Vrn (_loc, "Nativeint")), e) : Astf.exp )
           | ("int'",_) ->
               let e: Astf.exp =
                 `App (_loc, (`Lid (_loc, "string_of_int")), e) in
               (`App (_loc, (`Vrn (_loc, "Int")), e) : Astf.exp )
           | ("int32'",_) ->
               let e: Astf.exp =
                 `App
                   (_loc,
                     (`Dot
                        (_loc, (`Uid (_loc, "Int32")),
                          (`Lid (_loc, "to_string")))), e) in
               (`App (_loc, (`Vrn (_loc, "Int32")), e) : Astf.exp )
           | ("int64'",_) ->
               let e: Astf.exp =
                 `App
                   (_loc,
                     (`Dot
                        (_loc, (`Uid (_loc, "Int64")),
                          (`Lid (_loc, "to_string")))), e) in
               (`App (_loc, (`Vrn (_loc, "Int64")), e) : Astf.exp )
           | ("chr'",_) ->
               let e: Astf.exp =
                 `App
                   (_loc,
                     (`Dot
                        (_loc, (`Uid (_loc, "Char")),
                          (`Lid (_loc, "escaped")))), e) in
               (`App (_loc, (`Vrn (_loc, "Chr")), e) : Astf.exp )
           | ("str'",_) ->
               let e: Astf.exp =
                 `App
                   (_loc,
                     (`Dot
                        (_loc, (`Uid (_loc, "String")),
                          (`Lid (_loc, "escaped")))), e) in
               (`App (_loc, (`Vrn (_loc, "Str")), e) : Astf.exp )
           | ("flo'",_) ->
               let e: Astf.exp =
                 `App (_loc, (`Lid (_loc, "string_of_float")), e) in
               (`App (_loc, (`Vrn (_loc, "Flo")), e) : Astf.exp )
           | ("bool'",_) ->
               (`App (_loc, (`Vrn (_loc, "Bool")), e) : Astf.exp )
           | _ -> super#exp e)
      | e -> super#exp e
  end
