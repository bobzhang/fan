open Format
  
type op = Plus | Times
type expr = Op of op * expr list | I of int
let v = Op (Plus,
            [I 3;
             Op (Times,
                 [I 5; I 7; Op (Plus, [I 4; I 2])]);
             I 2])
