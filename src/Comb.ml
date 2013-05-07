open LibUtil
(* the output is reversed, you have to reverse the list output
   if you care about the order
 *)  
let slist0 ~f ps  = 
  let rec loop al = parser
    [ [< a = ps; 's >] -> loop [a :: al] s
    | [< >] -> al ] in
  parser [< a = loop [] >] -> f a 

let slist1 ~f ps =
  let rec loop al = parser
    [[< a = ps; 's>]  -> loop [a::al] s
    |[<>] -> al ] in
  parser [< a = ps; 's >] -> f (loop [a] s)
    
let slist0sep ~err ~f s sep  =
  let rec kont al = parser
    [ [< v = sep; a = s?? err v; 's >] ->
      kont [a::al] s
    | [<>] -> al ] in
  parser
    [[< a = s; 's >] -> f (kont [a] s)
    |[< >] -> f []]

let slist1sep ~err ~f s sep =
  let rec kont al = parser
      [ [< v = sep; a = parser
          [ [< a = s >] -> a
          | [< >] ->
             raise (XStream.Error (err v (* Failed.symb_failed entry v sep symb *))) ];
           's >] ->kont [a :: al] s
      | [< >] -> al ] in
  parser [< a = s ; 's >] ->
    f (kont [a] s)
    (* Action.mk (List.rev (kont [a] s)) *)


      
(* let slist1sep ~err ~f s sep = *)
  
let opt ps ~f = parser
  [ [< a = ps >] -> f (Some a)
  | [< >] -> f None ]

let tryp ps strm =
  let strm' = XStream.dup strm in
  let r =
    try ps strm'
    with
    [ XStream.Error _ | FanLoc.Exc_located (_, (XStream.Error _)) ->
        raise XStream.Failure
    | exc -> raise exc ] in begin 
        XStream.njunk (XStream.count strm') strm ;
        r;
    end
  
let peek ps strm =
  let strm' = XStream.dup strm in
  let r =
    try ps strm'
    with
    | XStream.Error _ | FanLoc.Exc_located (_, (XStream.Error _)) ->
        raise XStream.Failure
    | exc -> raise exc  in 
  r

let orp ?(msg="") p1 p2 = parser
  [ [< a = p1>] -> a
  | [< a = p2 >] -> a
  | [<>] -> raise (XStream.Error msg) ]

