
(** duplicated xs can not be detected *)
  %extend{
a:

    [("Inf" as v); "@"; Lid@lloc l; "("; a_int as x;","; Lid@xloc x ; ")" %{
     { text = Token(_loc,
                     %exp{({descr =  {tag = $vrn:v; word = Level $z (* int:level *); tag_name = $str:v}}:Tokenf.pattern)});
       styp = %ctyp'{Tokenf.op};
       bounds = [((lloc,l),Some "loc"); ((xloc,x),Some "txt")]; outer_pattern = None  }}]}
