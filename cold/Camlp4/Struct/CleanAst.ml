module Make =
 functor (Ast : Sig.Camlp4Ast) ->
  struct
   class clean_ast =
    object
     inherit Ast.map as super
     method with_constr =
      fun wc ->
       (match (super#with_constr wc) with
        | (Ast.WcAnd (_, Ast.WcNil (_), wc)
           | Ast.WcAnd (_, wc, Ast.WcNil (_))) ->
           wc
        | wc -> wc)
     method expr =
      fun e ->
       (match (super#expr e) with
        | (((((Ast.ExLet (_, _, Ast.BiNil (_), e)
               | Ast.ExRec (_, Ast.RbNil (_), e))
              | Ast.ExCom (_, Ast.ExNil (_), e))
             | Ast.ExCom (_, e, Ast.ExNil (_)))
            | Ast.ExSem (_, Ast.ExNil (_), e))
           | Ast.ExSem (_, e, Ast.ExNil (_))) ->
           e
        | e -> e)
     method patt =
      fun p ->
       (match (super#patt p) with
        | ((((((Ast.PaAli (_, p, Ast.PaNil (_))
                | Ast.PaOrp (_, Ast.PaNil (_), p))
               | Ast.PaOrp (_, p, Ast.PaNil (_)))
              | Ast.PaCom (_, Ast.PaNil (_), p))
             | Ast.PaCom (_, p, Ast.PaNil (_)))
            | Ast.PaSem (_, Ast.PaNil (_), p))
           | Ast.PaSem (_, p, Ast.PaNil (_))) ->
           p
        | p -> p)
     method match_case =
      fun mc ->
       (match (super#match_case mc) with
        | (Ast.McOr (_, Ast.McNil (_), mc) | Ast.McOr (_, mc, Ast.McNil (_))) ->
           mc
        | mc -> mc)
     method binding =
      fun bi ->
       (match (super#binding bi) with
        | (Ast.BiAnd (_, Ast.BiNil (_), bi)
           | Ast.BiAnd (_, bi, Ast.BiNil (_))) ->
           bi
        | bi -> bi)
     method rec_binding =
      fun rb ->
       (match (super#rec_binding rb) with
        | (Ast.RbSem (_, Ast.RbNil (_), bi)
           | Ast.RbSem (_, bi, Ast.RbNil (_))) ->
           bi
        | bi -> bi)
     method module_binding =
      fun mb ->
       (match (super#module_binding mb) with
        | (Ast.MbAnd (_, Ast.MbNil (_), mb)
           | Ast.MbAnd (_, mb, Ast.MbNil (_))) ->
           mb
        | mb -> mb)
     method ctyp =
      fun t ->
       (match (super#ctyp t) with
        | (((((((((((((((((Ast.TyPol (_, Ast.TyNil (_), t)
                           | Ast.TyAli (_, Ast.TyNil (_), t))
                          | Ast.TyAli (_, t, Ast.TyNil (_)))
                         | Ast.TyArr (_, t, Ast.TyNil (_)))
                        | Ast.TyArr (_, Ast.TyNil (_), t))
                       | Ast.TyOr (_, Ast.TyNil (_), t))
                      | Ast.TyOr (_, t, Ast.TyNil (_)))
                     | Ast.TyOf (_, t, Ast.TyNil (_)))
                    | Ast.TyAnd (_, Ast.TyNil (_), t))
                   | Ast.TyAnd (_, t, Ast.TyNil (_)))
                  | Ast.TySem (_, t, Ast.TyNil (_)))
                 | Ast.TySem (_, Ast.TyNil (_), t))
                | Ast.TyCom (_, Ast.TyNil (_), t))
               | Ast.TyCom (_, t, Ast.TyNil (_)))
              | Ast.TyAmp (_, t, Ast.TyNil (_)))
             | Ast.TyAmp (_, Ast.TyNil (_), t))
            | Ast.TySta (_, Ast.TyNil (_), t))
           | Ast.TySta (_, t, Ast.TyNil (_))) ->
           t
        | t -> t)
     method sig_item =
      fun sg ->
       (match (super#sig_item sg) with
        | (Ast.SgSem (_, Ast.SgNil (_), sg)
           | Ast.SgSem (_, sg, Ast.SgNil (_))) ->
           sg
        | Ast.SgTyp (loc, Ast.TyNil (_)) -> (Ast.SgNil (loc))
        | sg -> sg)
     method str_item =
      fun st ->
       (match (super#str_item st) with
        | (Ast.StSem (_, Ast.StNil (_), st)
           | Ast.StSem (_, st, Ast.StNil (_))) ->
           st
        | Ast.StTyp (loc, Ast.TyNil (_)) -> (Ast.StNil (loc))
        | Ast.StVal (loc, _, Ast.BiNil (_)) -> (Ast.StNil (loc))
        | st -> st)
     method module_type =
      fun mt ->
       (match (super#module_type mt) with
        | Ast.MtWit (_, mt, Ast.WcNil (_)) -> mt
        | mt -> mt)
     method class_expr =
      fun ce ->
       (match (super#class_expr ce) with
        | (Ast.CeAnd (_, Ast.CeNil (_), ce)
           | Ast.CeAnd (_, ce, Ast.CeNil (_))) ->
           ce
        | ce -> ce)
     method class_type =
      fun ct ->
       (match (super#class_type ct) with
        | (Ast.CtAnd (_, Ast.CtNil (_), ct)
           | Ast.CtAnd (_, ct, Ast.CtNil (_))) ->
           ct
        | ct -> ct)
     method class_sig_item =
      fun csg ->
       (match (super#class_sig_item csg) with
        | (Ast.CgSem (_, Ast.CgNil (_), csg)
           | Ast.CgSem (_, csg, Ast.CgNil (_))) ->
           csg
        | csg -> csg)
     method class_str_item =
      fun cst ->
       (match (super#class_str_item cst) with
        | (Ast.CrSem (_, Ast.CrNil (_), cst)
           | Ast.CrSem (_, cst, Ast.CrNil (_))) ->
           cst
        | cst -> cst)
    end

  end
