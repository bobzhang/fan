open Ast

open FanUtil

let antiquot_expander ~parse_pat  ~parse_exp  =
  object 
    inherit  Objs.map as super
    method! pat (x : pat) =
      match x with
      | `Ant (_loc,{ cxt; sep; decorations; content = code }) ->
          let e = parse_pat _loc code in
          (match (decorations, cxt, sep) with
           | (("uid"|"lid"|"par"|"seq"|"flo"|"int"|"int32"|"int64"
               |"nativeint"|"chr"|"str" as x),_,_)
             |(("vrn" as x),("exp"|"pat"),_) ->
               (`App (_loc, (`Vrn (_loc, (String.capitalize x))), e) : 
               Ast.pat )
           | _ -> super#pat e)
      | e -> super#pat e
    method! exp (x : exp) =
      match x with
      | `Ant (_loc,{ cxt; sep; decorations; content = code }) ->
          let e = parse_exp _loc code in
          (match (decorations, cxt, sep) with
           | (("uid"|"lid"|"par"|"seq"|"flo"|"int"|"int32"|"int64"
               |"nativeint"|"chr"|"str" as x),_,_)
             |(("vrn" as x),("exp"|"pat"),_) ->
               (`App (_loc, (`Vrn (_loc, (String.capitalize x))), e) : 
               Ast.exp )
           | ("`nativeint",_,_) ->
               let e: Ast.exp =
                 `App
                   (_loc,
                     (`Dot
                        (_loc, (`Uid (_loc, "Nativeint")),
                          (`Lid (_loc, "to_string")))), e) in
               (`App (_loc, (`Vrn (_loc, "Nativeint")), e) : Ast.exp )
           | ("`int",_,_) ->
               let e: Ast.exp =
                 `App (_loc, (`Lid (_loc, "string_of_int")), e) in
               (`App (_loc, (`Vrn (_loc, "Int")), e) : Ast.exp )
           | ("`int32",_,_) ->
               let e: Ast.exp =
                 `App
                   (_loc,
                     (`Dot
                        (_loc, (`Uid (_loc, "Int32")),
                          (`Lid (_loc, "to_string")))), e) in
               (`App (_loc, (`Vrn (_loc, "Int32")), e) : Ast.exp )
           | ("`int64",_,_) ->
               let e: Ast.exp =
                 `App
                   (_loc,
                     (`Dot
                        (_loc, (`Uid (_loc, "Int64")),
                          (`Lid (_loc, "to_string")))), e) in
               (`App (_loc, (`Vrn (_loc, "Int64")), e) : Ast.exp )
           | ("`chr",_,_) ->
               let e: Ast.exp =
                 `App
                   (_loc,
                     (`Dot
                        (_loc, (`Uid (_loc, "Char")),
                          (`Lid (_loc, "escaped")))), e) in
               (`App (_loc, (`Vrn (_loc, "Chr")), e) : Ast.exp )
           | ("`str",_,_) ->
               let e: Ast.exp =
                 `App
                   (_loc,
                     (`Dot
                        (_loc, (`Uid (_loc, "String")),
                          (`Lid (_loc, "escaped")))), e) in
               (`App (_loc, (`Vrn (_loc, "Str")), e) : Ast.exp )
           | ("`flo",_,_) ->
               let e: Ast.exp =
                 `App
                   (_loc,
                     (`Dot
                        (_loc, (`Uid (_loc, "FanUtil")),
                          (`Lid (_loc, "float_repres")))), e) in
               (`App (_loc, (`Vrn (_loc, "Flo")), e) : Ast.exp )
           | ("`bool",_,_) ->
               (`App
                  (_loc, (`Vrn (_loc, "Lid")),
                    (`IfThenElse
                       (_loc, e, (`Str (_loc, "true")),
                         (`Str (_loc, "false"))))) : Ast.exp )
           | _ -> super#exp e)
      | e -> super#exp e
  end