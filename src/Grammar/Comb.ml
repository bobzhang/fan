open LibUtil;
(* the output is reversed, you have to reverse the list output
   if you care about the order
 *)  
let slist0 ~f ps  = 
  let rec loop al = parser
    [ [< a = ps; 's >] -> loop [a :: al] s
    | [< >] -> al ] in
  parser [< a = loop [] >] -> f a ;

let slist1 ~f ps =
  let rec loop al = parser
    [[< a = ps; 's>]  -> loop [a::al] s
    |[<>] -> al ] in
  parser [< a = ps; 's >] -> f (loop [a] s);
    
let slist0sep ~err ~f s sep  =
  let rec kont al = parser
    [ [< v = sep; a = s?? err v; 's >] ->
      kont [a::al] s
    | [<>] -> al ] in
  parser
    [[< a = s; 's >] -> f (kont [a] s)
    |[< >] -> f []];

(* let slist1sep ~err ~f s sep = *)
  
let opt ps ~f = parser
  [ [< a = ps >] -> f (Some a)
  | [< >] -> f None ];

 let tryp ps strm =
  let strm' = Stream.dup strm in
  let r =
    try ps strm'
    with
    [ Stream.Error _ | FanLoc.Exc_located _ (Stream.Error _) ->
        raise Stream.Failure
    | exc -> raise exc ] in begin 
        Stream.njunk (Stream.count strm') strm ;
        r;
    end;
