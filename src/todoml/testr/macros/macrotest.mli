DEFINE A;
DEFINE B;

IFDEF A THEN
  val a_should_be_present : int;
ENDIF;

IFNDEF C THEN
  val b_should_be_present : int;
ENDIF;

IFNDEF C THEN
  val c_should_be_present : int;
ELSE
  val a_should_NOT_be_present : int;
END;

IFDEF C THEN
  val b_should_NOT_be_present : int;
ELSE
  val d_should_be_present : int;
  val e_should_be_present : int;
ENDIF;

val f_should_be_present : int;
