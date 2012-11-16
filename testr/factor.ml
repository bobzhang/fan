let c = Gram.mk "c";
let b = Gram.mk "b";
let test f =   
  {:extend|Gram
    c:
    [ $f ; "end"
    | b ; "in"] |};

test b;  
