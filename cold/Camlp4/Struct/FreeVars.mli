module Make :
 functor (Ast : Sig.Camlp4Ast) ->
  sig
   module S : (Set.S with type  elt = string)

   val fold_binding_vars :
    ((string -> ('accu -> 'accu)) -> (Ast.binding -> ('accu -> 'accu)))

   class ['accu] c_fold_pattern_vars :
    (string -> ('accu -> 'accu)) ->
     'accu -> object inherit Ast.fold val acc : 'accu method acc : 'accu end

    val fold_pattern_vars :
     ((string -> ('accu -> 'accu)) -> (Ast.patt -> ('accu -> 'accu)))

    class ['accu] fold_free_vars :
     (string -> ('accu -> 'accu)) ->
      ?env_init : S.t ->
       'accu ->
        object ('self_type)
         inherit Ast.fold
         val free : 'accu
         val env : S.t
         method free : 'accu
         method set_env : (S.t -> 'self_type)
         method add_atom : (string -> 'self_type)
         method add_patt : (Ast.patt -> 'self_type)
         method add_binding : (Ast.binding -> 'self_type)
        end

     val free_vars : (S.t -> (Ast.expr -> S.t))

    end
