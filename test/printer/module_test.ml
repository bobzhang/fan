module Ast = struct
  include Ast
  let safe_string_escaped (s) =
      if
        (( (( (String.length s) ) > 2) ) && (
        (( (( s.[0] ) = '\\') ) && ( (( s.[1] ) = '$') )) ))
      then begin s
      end else begin (String.escaped s)
      end
end


module U = F.Make(S)

















