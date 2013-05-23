type position = Lexing.position = {
  pos_fname : string;
  pos_lnum : int;
  pos_bol : int;
  pos_cnum : int;
}

type t = Location.t = {
  loc_start: position;
  loc_end: position;
  loc_ghost: bool;
}
