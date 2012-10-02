
type m_list 'a =
  [ M_nil
  | M_cons of 'a and m_list 'a ]
;

type twice 'a = 'a
;

type tree 'a  =
  [ Leaf
  | Node of m_list (tree 'a) and (twice  (m_list (tree 'a) )  ) and 'a ]

;

class base =
  object method int : int -> int = fun x -> x;
    method float : float -> float = fun x -> x;
    method string : string -> string = fun x -> x;
  end;
  
module EQ =
  struct
    class map =
      object ((self : 'self_type))
        inherit base;
        method unknown : ! 'a. 'a -> 'a = fun x -> x;
        method tree :
          ! 'a0 'b0. ('self_type -> 'a0 -> 'b0) -> tree 'a0 -> tree 'b0 =
          (*
type a = string ;

type b =
    [ = `Atr of (string * a )];
*)
          fun mf_a ->
            fun
            [ Leaf -> Leaf
            | Node a0 a1 a2 ->
                Node (self#m_list (fun self -> self#tree mf_a) a0)
                  (self#twice
                     (fun self -> self#m_list (fun self -> self#tree mf_a))
                     a1)
                  (mf_a self a2) ];
        method twice :
          ! 'a0 'b0. ('self_type -> 'a0 -> 'b0) -> twice 'a0 -> twice 'b0 =
          fun mf_a -> mf_a self;
        method m_list :
          ! 'a0 'b0. ('self_type -> 'a0 -> 'b0) -> m_list 'a0 -> m_list 'b0 =
          fun mf_a ->
            fun
            [ M_nil -> M_nil
            | M_cons a0 a1 -> M_cons (mf_a self a0) (self#m_list mf_a a1) ];
      end;
  end;





















