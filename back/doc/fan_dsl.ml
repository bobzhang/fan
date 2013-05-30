{
 variant:
   {need: ty_exprs;
    code: (fun
          [[] -> self
          |ls -> reduce_right (fun v acc -> << let self=$v$ in $acc$ >> )])
          ty_exprs;
    };
 tuple: {code: mk_variant "";}
 record: {code: mk_variant "";};
 kind: Fold;
 arity:2
 default: << (_,_) -> invalid_arg "failure" >> 
}



















