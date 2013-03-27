include AstLoc
let match_pre =
  object (self)
    inherit  Objs.map
    method! case =
      function
      | `Case (_loc,p,e) ->
          `Case
            (_loc, p,
              (`Fun
                 (_loc, (`Case (_loc, (`Id (_loc, (`Uid (_loc, "()")))), e)))))
      | `CaseWhen (_loc,p,e,e1) ->
          `CaseWhen
            (_loc, p, e,
              (`Fun
                 (_loc,
                   (`Case (_loc, (`Id (_loc, (`Uid (_loc, "()")))), e1)))))
      | `Bar (_loc,a1,a2) -> `Bar (_loc, (self#case a1), (self#case a2))
      | `Ant (_loc,x) -> `Ant (_loc, (FanUtil.add_context x "lettry"))
  end