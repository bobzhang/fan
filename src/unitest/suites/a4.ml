
%create{Gramf pos_exps};;

%extend{primitve@Inline :
       [ Str s %{ `Str (_loc, s)}]};;
let make_pat exp =
  %extend{
       pat:
       {"simple"
        [| @primitve] }}
let () =
  begin
        %extend{
       exp :
       {"simple"
        [| @primitve ]}
      }

  end
