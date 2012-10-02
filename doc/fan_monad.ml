open Format;
module OptionMonad = struct
  type t 'a = option 'a;
  value return a = Some a ;
  value (>>=) ma f = match ma with
    [Some v -> f v
    |None -> None];
end;
module StateMonad = struct
  type t 'a 's = 's -> ('a*'s);
  value return a = fun s -> (a,s);
  value (>>=) ma f = fun s -> 
    let (a,s) = ma s  in
    f a s ;
end;


















