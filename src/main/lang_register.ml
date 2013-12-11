



%create{register}  ;;

let compile pairs = 
  
%extend{
  register:
  [L1 pair Sep ";" as pairs %{ compile pairs }]
  pair@Local:
  [ Lid@xloc x; ":"; Lid@yloc y %{((xloc,x),(yloc,y))} ]
};;
