
(* DEFINE Z = "y" ; *)

IFDEF DNE THEN
  DEFINE Z = "ko2xxx, should never happen" ^Z;
ELSE
  DEFINE Z = "ok2" ^ Z;
ENDIF;

Z;

pouet;

