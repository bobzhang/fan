type ant = [ `Ant of FanUtil.anti_cxt] 
type f = [ `App of (f* f) | `U of g | ant] 
and g = [ `E of g | `U of g | ant] 
type f1 = [ `App of (f1* f1) | `U of g1] 
and g1 = [ `E of g1 | `U of g1] 
let f (x : f) = match x with | #f1 as y -> y | _ -> assert false