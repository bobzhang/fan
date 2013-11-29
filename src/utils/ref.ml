

let protect r v body =
  let old = !r in
  try begin 
    r := v;
    let res = body() in
    (r := old;
     res)
  end with x -> (r := old; raise x)
      
let save r body =
  let old = !r in
  Util.finally ~action:(fun () -> r:=old) () body 
    
let protect2 (r1,v1) (r2,v2) body =
  let o1 = !r1 and o2 = !r2 in
  try begin
    r1:= v1; r2:=v2;
    let res = body () in
    (r1:=o1; r2:=o2;
     res)
  end
  with  e -> begin
    r1:=o1; r2:=o2;
    raise e
  end

      
let save2 r1 r2 body =
  let o1 = !r1 and o2 = !r2 in
  Util.finally ~action:(fun () -> (r1:=o1; r2:=o2)) () body 
    
let protects refs vs body =
  let olds = List.map (fun x-> !x ) refs in 
  try begin
    List.iter2 (fun ref v -> ref:=v) refs vs;
    let res = body () in
    (List.iter2 (fun ref v -> ref:=v) refs olds;
     res)   
  end
  with e -> 
    (List.iter2 (fun ref v -> ref:=v) refs olds;
     raise e)

      (* The state [itself] should be [persistent],
         otherwise it does not make sense to restore
       *)      
let saves (refs: 'a ref list ) body =
  let olds = List.map (fun x -> !x) refs in
  Util.finally ~action:(fun () ->   List.iter2 (fun ref x -> ref :=x ) refs olds) () body 


let post r f =
  let old = !r in 
  (r := f old; old)

let pre r f =
  (r := f !r; !r)

let swap a b =
  let buf = !a in
  (a := !b; b := buf)
    
let modify x f =
  x := f !x


      
