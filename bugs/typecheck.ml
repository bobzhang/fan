
type ant = [`Ant of string]
type 'a mlist =
    [ `LNil 
    | `LCons of ('a * 'a mlist )
    | ant]
let a = object(self)
  method ant : ant -> ant=
    fun (`Ant a)  ->
      `Ant a
  method mlist: 'all_a0 'all_b0 .
      ('self_type -> 'all_a0 -> 'all_b0) ->
        'all_a0 mlist -> 'all_b0 mlist=
          fun  mf_a  ->
            function
              | `LNil ->  `LNil
              | `LCons (a0,a1) ->
                  let a0 = mf_a self a0 in
                  let a1 = self#mlist mf_a a1 in `LCons (a0, a1)
                    
              | #ant as a0 -> ( self#ant a0 : ant :>  _ mlist)
              (* | #ant as a0 -> (((\* Obj.magic *\) self#ant a0) :> (\* 'all_b0 *\) t mlist) *)
end





















