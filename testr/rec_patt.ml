
type u = {v:int;u:int};
let f =   fun
  [{v;_} -> v];
let g = fun
  [{v;u} -> u];

let f1 = fun
  [{v;} -> v];
