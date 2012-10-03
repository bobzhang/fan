



value meta_int _loc i =  << .$`int:i$. >>;

value meta_int32 _loc i =  << .$`int32:i$. >>;

value meta_int64 _loc i =  << .$`int64:i$. >>;
  
value meta_nativeint _loc i =  << .$`nativeint:i$. >>;
  
value meta_float _loc i = << .$`flo:i$. >>;
  
value meta_string _loc i = << .$`str:i$.  >> ;
  
value meta_char _loc i = << .$`chr:i$.  >>;
value meta_unit _loc _ = << () >>  ;
value meta_bool _loc = fun [True -> << True>> | False -> <<False>> ];

(* ( << {val = 3} >>  |> e2s ); *)
value meta_ref mf_a _loc i =  << {val = .$ mf_a _loc i.val $. } >> ; 
value rec meta_list mf_a _loc  ls =
  List.map (fun x -> mf_a _loc x ) ls |> mk_list ;
value rec meta_array mf_a _loc ls =
  Array.map (fun x -> mf_a _loc x) ls |> mk_array;
  
value meta_option mf_a _loc  = fun
  [ None -> << None >>
  | Some x -> << Some .$mf_a _loc x$. >>  ];

value meta_arrow
    (mf_a: Loc.t -> 'a -> t)
    (mf_b:Loc.t -> 'b ->t)
    (_loc:Loc.t)  (x:'a -> 'b) = invalid_arg "meta_arrow not implemented";



















