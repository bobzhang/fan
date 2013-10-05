


let (<+) x y = x + y


let id x = [x] 

let u =
  id @@ 3 <+ 4 
  
