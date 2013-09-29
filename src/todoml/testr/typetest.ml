

{:extend.create|Gram
  (a "a" Gram.t (list FanSig.token))
|};


{:extend|Gram
  a:[ L0 "match"{ls} -> ls ]
|};
