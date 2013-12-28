let () =
  let d = Ns.lang in
  let f (loc : Locf.t) _meta _content =
    (`App
       (loc, (`Lid (loc, "ref")),
         (`Fun
            (loc,
              (`Case
                 (loc, (`Any loc),
                   (`App
                      (loc,
                        (`App
                           (loc,
                             (`App
                                (loc,
                                  (`Dot
                                     (loc, (`Uid (loc, "Util")),
                                       (`Lid (loc, "failwithf")))),
                                  (`Str (loc, "%s.%s not implemented ")))),
                             (`Lid (loc, "__MODULE__")))),
                        (`Lid (loc, "__BIND__"))))))))) :>Astf.exp) in
  Ast_quotation.add { domain = d; name = "undef" } Dyn_tag.exp f
