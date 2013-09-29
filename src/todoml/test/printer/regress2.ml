class map =
  object (o : 'self_type)
    method string : string  -> string = o#unknown
    method list :
        'a_out 'a .
        ('self_type -> 'a -> 'a_out) -> 'a list  -> 'a_out list =
          fun _f_a ->
            function
              | [] -> []
              | _x::_x_i1 ->
                  let _x = _f_a o _x in
                  let _x_i1 = (o#list) _f_a _x_i1 in _x::_x_i1
  end
