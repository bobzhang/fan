(** Test programs for HLVM. *)

open Printf
open Hlvm
open Expr

let ( |> ) x f = f x
let floatOfInt x = FloatOfInt(`Float, x)
(*
let forloopN = ref 1

let forloop i0 i1 body : Hlvm.t list =
  let f = sprintf "for%d" !forloopN in
  incr forloopN;
  [ `Function
      (f, ["i0", `Int; "i2", `Int], `Unit,
       If(Var "i0" =: Var "i2", Unit,
	  If(Var "i0" +: Int 1 =: Var "i2", body (Var "i0"),
	     Let("i1", (Var "i0" +: Var "i2") /: Int 2,
		 compound
		   [ Apply(Var f, [Var "i0"; Var "i1"]);
		     Apply(Var f, [Var "i1"; Var "i2"]) ])))) ]
*)
(** Correctness test for the Boehm GC. *)
let boehm : Hlvm.t list =
  let n = 1048576 in
  [ `Function
      ("fill", [ "a", `Array `Int;
		 "rand", `Int;
		 "i", `Int ], `Unit,
       If(Var "i" <: Int n,
	  compound
	    [ Set(Var "a", Var "i", Var "rand");
	      Apply(Var "fill",
		    [Var "a";
		     Var "rand" *: Int 1664525 +: Int 1013904223;
		     Var "i" +: Int 1]) ],
	  Unit));

    `Function("loop", [ "i", `Int ], `Unit,
	      If(Var "i" <: Int 1, Unit,
		 Let("", Alloc(Var "i", Byte 0),
		     Apply(Var "loop", [ Var "i" -: Int 1 ]))));

    `Expr
      (compound
	 [ Let("p", Alloc(Int n, Int 0),
	       compound
		 [ Apply(Var "fill", [Var "p"; Int 1; Int 0]);
		   Apply(Var "loop", [Int 32768]) ]) ]) ]

(** Integer Fibonacci benchmark *)
let fib ns : Hlvm.t list =
  let n = Var "n" in
  [ `Function
      ("fib", ["n", `Int], `Int,
       If(n >: Int 1,
          Apply(Var "fib", [n -: Int 2]) +: Apply(Var "fib", [n -: Int 1]),
          n)) ] @
    List.map
    (fun n ->
       `Expr
	 (compound
            [ Printf("\nInteger Fibonacci function: fib(%d)\n", [Int n]);
              Apply(Var "fib", [Int n]) ]))
    ns

(** Float-based Fibonacci benchmark *)
let ffib ns : Hlvm.t list =
  let n = Var "n" in
  [ `Function
      ("ffib", ["n", `Float], `Float,
       If(n >: Float 1.0,
	  Apply(Var "ffib", [n -: Float 2.0]) +:
	    Apply(Var "ffib", [n -: Float 1.0]),
	  n)) ] @
    List.map
    (fun n ->
       let n = Float n in
       `Expr
	 (compound
            [ Printf("\nFloating-point Fibonacci function: fib(%f)\n", [n]);
              Apply(Var "ffib", [n]) ]))
    ns

let fill ty =
  [`Function
     ("fill", [ "a", `Array ty;
		"x", ty;
		"i", `Int ], `Unit,
      If(Var "i" <: Length(Var "a"),
	 compound
	   [ Set(Var "a", Var "i", Var "x");
	     Apply(Var "fill", [Var "a"; Var "x"; Var "i" +: Int 1]) ],
	 Unit))]

(** Sieve of Eratosthenes. *)
let sieve is : Hlvm.t list =
  fill `Bool @
    [ `Function
	("last", ["a", `Array `Bool; "i", `Int], `Int,
	 If(Get(Var "a", Var "i"), Var "i",
	    Apply(Var "last", [Var "a"; Var "i" -: Int 1])));
      
      `Function
	("loop2", ["a", `Array `Bool; "i", `Int; "di", `Int], `Unit,
	 If(Var "i" >=: Length(Var "a"), Unit,
	    compound
	      [ Set(Var "a", Var "i", Bool false);
		Apply(Var "loop2",
		      [Var "a"; Var "i" +: Var "di"; Var "di"]) ]));

      `Function
	("loop1", ["a", `Array `Bool; "i", `Int], `Unit,
	 If(Var "i" =: Length(Var "a"), Unit,
	    compound
	      [ If(Get(Var "a", Var "i"),
		   Apply(Var "loop2", [Var "a"; Int 2 *: Var "i"; Var "i"]),
		   Unit);
		Apply(Var "loop1", [Var "a"; Var "i" +: Int 1]) ])) ] @
    List.map
    (fun i ->
       `Expr(Let("a", Alloc(Int i, Bool false),
		 compound
		   [ Printf("\nSieve of Eratosthenes\n", []);
                     Apply(Var "fill", [Var "a"; Bool true; Int 0]);
		     Apply(Var "loop1", [Var "a"; Int 2]);
		     Apply(Var "last",
			   [Var "a"; Length(Var "a") -: Int 1]) ])))
    is

(** Render the Mandelbrot set with inlined complex arithmetic. *)
let mandelbrot ns : Hlvm.t list =
  [ `Function
      ("pixel", ["n", `Int;
		 "zr", `Float; "zi", `Float;
		 "cr", `Float; "ci", `Float], `Unit,
       If(Var "n" =: Int 65536, Printf(" ", []),
	  If(Var "zr" *: Var "zr" +: Var "zi" *: Var "zi" >=: Float 4.0,
	     Printf(".", []),
	     Apply(Var "pixel",
		   [Var "n" +: Int 1;
		    Var "zr" *: Var "zr" -:
		      Var "zi" *: Var "zi" +: Var "cr";
		    Float 2.0 *: Var "zr" *: Var "zi" +: Var "ci";
		    Var "cr"; Var "ci"]))));
    
    `Function
      ("row", ["i", `Int; "j", `Int; "n", `Int], `Unit,
       If(Var "i" >: Var "n", Unit,
	  compound
	    [ Apply(Var "pixel",
		    [Int 0;
		     Float 0.0; Float 0.0;
		     Float 2.0 *: floatOfInt(Var "i") /: floatOfInt(Var "n") -:
		       Float 1.5;
		     Float 2.0 *: floatOfInt(Var "j") /: floatOfInt(Var "n") -:
		       Float 1.0]);
	      Apply(Var "row", [Var "i" +: Int 1; Var "j"; Var "n"])]));

    `Function
      ("col", ["j", `Int; "n", `Int], `Unit,
       If(Var "j" >: Var "n", Unit,
	  compound
	    [ Apply(Var "row", [Int 0; Var "j"; Var "n"]);
	      Printf("\n", []);
	      Apply(Var "col", [Var "j" +: Int 1; Var "n"])])) ] @
    List.map
    (fun n ->
       `Expr
	 (compound
            [ Printf("\nMandelbrot with inline complex arithmetic\n", []);
              Apply(Var "col", [Int 0; Int n]) ]))
    ns

(** Render the Mandelbrot set without inlined arithmetic operations on complex
    numbers as structs. *)
let mandelbrot2 ns : Hlvm.t list =
  let complex = `Struct[`Float; `Float] in
  let re z = GetValue(Var z, 0) in
  let im z = GetValue(Var z, 1) in
  [ `Function("znorm2", ["z", complex], `Float,
              re "z" *: re "z" +: im "z" *: im "z");

    `Function("zsqr", ["z", complex], complex,
              Struct[re "z" *: re "z" -: im "z" *: im "z";
                     Float 2.0 *: re "z" *: im "z"]);

    `Function("zadd", ["z1", complex; "z2", complex], complex,
              Struct[re "z1" +: re "z2"; im "z1" +: im "z2"]);

    `Function
      ("pixel", ["n", `Int; "z", complex; "c", complex], `Unit,
       If(Var "n" =: Int 65536, Printf(" ", []),
          If(Apply(Var "znorm2", [Var "z"]) >=: Float 4.0,
             Printf(".", []),
	     Apply(Var "pixel",
                   [Var "n" +: Int 1;
                    Apply(Var "zadd", [Apply(Var "zsqr", [Var "z"]); Var "c"]);
                    Var "c"]))));

    `Function
      ("row", ["i", `Int; "j", `Int; "n", `Int], `Unit,
       If(Var "i" >: Var "n", Unit,
          compound
            [ Apply(Var "pixel",
                    [Int 0;
                     Struct[Float 0.0; Float 0.0];
                     Struct[Float 2.0 *: floatOfInt(Var "i") /:
                              floatOfInt(Var "n") -: Float 1.5;
                            Float 2.0 *: floatOfInt(Var "j") /:
                              floatOfInt(Var "n") -: Float 1.0]]);
              Apply(Var "row", [Var "i" +: Int 1; Var "j"; Var "n"])]));

    `Function
      ("col", ["j", `Int; "n", `Int], `Unit,
       If(Var "j" >: Var "n", Unit,
	  compound
	    [ Apply(Var "row", [Int 0; Var "j"; Var "n"]);
	      Printf("\n", []);
	      Apply(Var "col", [Var "j" +: Int 1; Var "n"])])) ] @
    List.map
    (fun n ->
       `Expr
	 (compound
            [ Printf("\nMandelbrot with complex arithmetic functions\n", []);
              Apply(Var "col", [Int 0; Int n]) ]))
    ns

(** Test tail call elimination by passing one function as an argument to
    another higher-order function that calls it in tail position, mutually
    recursively. *)
let tco n : Hlvm.t list =
  [ `Function("even", ["odd", `Function([`Int], `Int); "n", `Int], `Int,
              Apply(Var "odd", [Var "n" +: Int 1]));

    `Function("odd", ["n", `Int], `Int,
	      If(Var "n" <: Int n,
		 Apply(Var "even", [Var "odd"; Var "n" +: Int 1]),
		 Var "n"));

    `Expr
      (compound
         [ Printf("\nTesting TCO across a higher-order function\n", []);
           Apply(Var "even", [Var "odd"; Int 0]) ])]

(** Test HLVM's struct representation of tuples. *)
let tuples : Hlvm.t list =
  [ `Function("id", ["s", `Struct[`Float; `Int]], `Struct[`Float; `Int],
	      Var "s");

    `Function("id2", ["s", `Struct[`Float; `Int]], `Struct[`Float; `Int],
	      Apply(Var "id", [Var "s"]));

    `Function("rev", ["s", `Struct[`Int; `Float]], `Struct[`Float; `Int],
	      Apply(Var "id", [Struct[GetValue(Var "s", 1);
				      GetValue(Var "s", 0)]]));

    `Expr
      (compound
         [ Printf("\nTesting structs (should give (3.4, 2))\n", []);
           Apply(Var "rev", [Struct[Int 2; Float 3.4]]) ]) ]

(** Test the FFI by calling some trig functions from libc. *)
let trig : Hlvm.t list =
  let triple = `Struct[`Float; `Float; `Float] in
  [ `Extern("sin", [`Float], `Float);

    `Extern("cos", [`Float], `Float);

    `Function("test", ["f", `Function([`Float], `Float)], triple,
	      Struct[Apply(Var "f", [Float 0.1]);
		     Apply(Var "f", [Float 0.2]);
		     Apply(Var "f", [Float 0.3])]);

    `Expr
      (compound
         [ Printf("\nTesting FFI\n", []);
           Print(Apply(Var "test", [Var "sin"]));
	   Print(Apply(Var "test", [Var "cos"])) ]) ]

(** Create and fold over a large array. *)
let fold ns : Hlvm.t list =
  let fold ty1 ty2 =
    [ `Function("Array.fold_aux", ["n", `Int;
			     "f", `Function([ty1; ty2], ty1);
			     "y", ty1;
			     "xs", `Array ty2], ty1,
		If(Var "n" <: Length(Var "xs"),
		   Apply(Var "Array.fold_aux",
			 [Var "n" +: Int 1;
			  Var "f";
			  Apply(Var "f", [Var "y"; Get(Var "xs", Var "n")]);
			  Var "xs"]),
		   Var "y"));

      `Function("Array.fold", ["f", `Function([ty1; ty2], ty1);
			       "y", ty1;
			       "xs", `Array ty2], ty1,
		Apply(Var "Array.fold_aux",
		      [Int 0; Var "f"; Var "y"; Var "xs"])) ]
  in

  fold (`Struct[`Float; `Float]) `Float @
    fill `Float @
    [ `Function("f", ["x", `Struct[`Float; `Float];
		      "y", `Float], `Struct[`Float; `Float],
		Struct[GetValue(Var "x", 0) +:
			 Var "y" /: (Float 1.0 +: GetValue(Var "x", 1));
		       GetValue(Var "x", 1) +: Float 1.]) ] @
    List.map
    (fun n ->
       `Expr
	 (Let("xs", Alloc(Int n, Float 1.0),
	      compound
		[ Printf("\nArray.fold over %d elements\n", [Int n]);
		  Apply(Var "Array.fold",
			[Var "f"; Struct[Float 0.; Float 0.]; Var "xs"]) ])))
    ns

(** Type of a list. *)
let ty_list ty =
  [ `Type("Cons", `Struct[ty; `Reference]);
    `Type("Nil", `Unit) ]

let nil = Construct("Nil", Unit)
let cons h t = Construct("Cons", Struct[h; t])

(** Pattern match over empty or non-empty list. *)
let cond_list list h t k_nil k_cons =
  If(IsType(Var list, "Nil"), k_nil,
     Let(h^t, Cast(Var list, "Cons"),
	 Let(h, GetValue(Var (h^t), 0),
	     Let(t, GetValue(Var (h^t), 1),
		 k_cons))))

(** Polymorphic List.fold_left in HLVM. *)
let list_fold_left a b =
  `Function("List.fold_left", ["f", `Function([a; b], a);
			       "x", a;
			       "list", `Reference], a,
	    cond_list "list" "h" "t"
	      (Var "x")
	      (Apply(Var "List.fold_left",
		     [Var "f";
		      Apply(Var "f", [Var "x"; Var "h"]);
		      Var "t"])))

(** Initialize and fold over a long linked list. *)
(* Not practically relevant but interesting for comparison with OCaml. *)
let list ns : Hlvm.t list =
  ty_list `Int @
    [ `Function("add", ["n", `Int; "m", `Int], `Int, Var "n" +: Var "m");
      
      `Function("List.init", ["t", `Reference; "n", `Int], `Reference,
		Let("t", cons (Var "n") (Var "t"),
		    If(Var "n" =: Int 0, Var "t",
		       Apply(Var "List.init", [Var "t"; Var "n" -: Int 1]))));
      
      list_fold_left `Int `Int;
      
      `Expr(Apply(Var "List.init", [nil; Int 10])) ] @
    List.map
    (fun n ->
       `Expr
	 (compound
	    [ Printf("\nList.init and fold over %d elements\n", [Int n]);
	      Let("list", Apply(Var "List.init", [nil; Int n]),
		  Apply(Var "List.fold_left",
			[Var "add"; Int 0; Var "list"])) ]))
    ns
       
(** Type of a closure. *)
let ty_closure ty_env (ty1, ty2) =
  `Struct[`Function([ty_env; ty1], ty2); ty_env]

(** Apply a closure. *)
let apply(f, x) =
  Apply(GetValue(f, 0), [GetValue(f, 1); x])

(** Test curried function application with a uniform representation of closure
    environments. *)
let curry : Hlvm.t list =
  let ty_ret = `Struct[`Int; `Float] in
  [ `Function("f_uncurried", ["x", `Int; "y", `Float], ty_ret,
	      Struct[Var "x"; Var "y"]);

    `Type("Int", `Int);

    `Function("f_apply_2", ["env", `Reference; "y", `Float], ty_ret,
	      Apply(Var "f_uncurried", [Cast(Var "env", "Int"); Var "y"]));

    `Function("f_apply_1", ["x", `Int], ty_closure `Reference (`Float, ty_ret),
	      Struct[Var "f_apply_2"; Construct("Int", Var "x")]);

    `Expr
      (compound
	 [ Printf("\nTest partial application of curried functions\n", []);
	   Let("g", Apply(Var "f_apply_1", [Int 3]),
	       Struct[apply(Var "g", Float 2.3);
		      apply(Var "g", Float 3.4)]) ]) ]
     
let list_filter ty_env ty : Hlvm.t =
  `Function("filter", ["pred", ty_closure ty_env (ty, `Bool);
		       "list", `Reference], `Reference,
	    cond_list "list" "h" "t"
	      (Var "list")
	      (Let("t", Apply(Var "filter", [Var "pred"; Var "t"]),
		   If(apply(Var "pred", Var "h"),
		      cons (Var "h") (Var "t"),
		      Var "t"))))

let list_length ty : Hlvm.t =
  `Function("length", ["list", `Reference], `Int,
	    cond_list "list" "h" "t"
	      (Int 0)
	      (Int 1 +: Apply(Var "length", [Var "t"])))

(** Solve the n-queens problem using linked lists. *)
let queens ns =
  let x1 = Var "x1" and x2 = Var "x2" and y1 = Var "y1" and y2 = Var "y2" in
  let ty_pos = `Struct[`Byte; `Byte] in
  let rec init n f = if n=0 then [] else f(n-1) :: init (n-1) f in
  ty_list ty_pos @
    [ list_length ty_pos;

      `Function("safe", ["p1", ty_pos; "p2", ty_pos], `Bool,
		Let("x1", GetValue(Var "p1", 0),
		    Let("y1", GetValue(Var "p1", 1),
			Let("x2", GetValue(Var "p2", 0),
			    Let("y2", GetValue(Var "p2", 1),
				(x1 <>: x2) &&:
				  (y1 <>: y2) &&:
				  (x2 -: x1 <>: y2 -: y1) &&:
				  (x1 -: y2 <>: x2 -: y1))))));

(* This implementation boxes the closure's environment but that is actually
   unnecessary in HLVM.

      `Type("IntInt", ty_pos);

      list_filter `Reference ty_pos;

      `Function("safe_2", ["env", `Reference; "p2", ty_pos], `Bool,
		Let("p1", Cast(Var "env", "IntInt"),
		    Apply(Var "safe", [Var "p1"; Var "p2"])));

      `Function("safe_1", ["p1", ty_pos],
                ty_closure `Reference (ty_pos, `Bool),
		Struct[Var "safe_2"; Construct("IntInt", Var "p1")]);

      `Function("search", ["f", `Function([`Reference; `Int], `Int);
			   "n", `Int;
			   "qs", `Reference;
			   "ps", `Reference;
			   "accu", `Int], `Int,
		cond_list "ps" "q" "ps"
		  (If(Apply(Var "length", [Var "qs"]) =: Var "n",
		      Apply(Var "f", [Var "qs"; Var "accu"]),
		      Var "accu"))
		  (Apply(Var "search",
			 [Var "f";
			  Var "n";
			  cons (Var "q") (Var "qs");
			  Apply(Var "filter",
				[Apply(Var "safe_1", [Var "q"]);
				 Var "ps"]);
			  Apply(Var "search",
				[Var "f";
				 Var "n";
				 Var "qs";
				 Var "ps";
				 Var "accu"])])));
*)

      list_filter ty_pos ty_pos;

      `Function("safe_1", ["p1", ty_pos], ty_closure ty_pos (ty_pos, `Bool),
		Struct[Var "safe"; Var "p1"]);

      `Function("search", ["f", `Function([`Reference; `Int], `Int);
			   "n", `Int;
			   "qs", `Reference;
			   "ps", `Reference;
			   "accu", `Int], `Int,
		cond_list "ps" "q" "ps"
		  (If(Apply(Var "length", [Var "qs"]) =: Var "n",
		      Apply(Var "f", [Var "qs"; Var "accu"]),
		      Var "accu"))
		  (Apply(Var "search",
			 [Var "f";
			  Var "n";
			  cons (Var "q") (Var "qs");
			  Apply(Var "filter",
				[Apply(Var "safe_1", [Var "q"]);
				 Var "ps"]);
			  Apply(Var "search",
				[Var "f";
				 Var "n";
				 Var "qs";
				 Var "ps";
				 Var "accu"])])));

      `Function("ps", [ "n", `Byte;
			"i", `Byte;
			"j", `Byte ], `Reference,
		If(Var "i" =: Var "n",
		   If(Var "j" =: Var "n" -: Byte 1,
		      nil,
		      Apply(Var "ps", [Var "n"; Byte 0; Var "j" +: Byte 1])),
		   cons (Struct[Var "i"; Var "j"])
		     (Apply(Var "ps", [Var "n"; Var "i" +: Byte 1; Var "j"]))));

      `Function("f", ["", `Reference; "n", `Int], `Int, Var "n" +: Int 1)] @
    List.map
    (fun n ->
       `Expr
	 (compound
	    [ Printf("\nSolve %d-queens problem using lists\n", [Int n]);
	      Apply(Var "search",
		    [Var "f";
		     Int n;
		     nil;
		     Apply(Var "ps", [Byte n; Byte 0; Byte 0]);
		     Int 0]) ]))
    ns

(** Hash table benchmark derived from HLVM's original GC. *)
let gc ns =
  let append ty =
    [ `Function("aux", ["a", `Array ty;
			"b", `Array ty;
			"i", `Int;
			"x", ty], `Array ty,
		If(Var "i" <: Length(Var "a"),
		   compound
		     [ Set(Var "b", Var "i", Get(Var "a", Var "i"));
		       Apply(Var "aux", [Var "a";
					 Var "b";
					 Var "i" +: Int 1;
					 Var "x"]) ],
		   Var "b"));

      `Function("append", ["a", `Array ty; "x", ty], `Array ty,
		Apply(Var "aux", [Var "a";
				  Alloc(Length(Var "a") +: Int 1, Var "x");
				  Int 0;
				  Var "x"])) ] in
  let q = 16381 in
  let ty_bkt = `Array(`Struct[`Reference; `Bool]) in
  append (`Struct[`Reference; `Bool]) @
    fill(`Struct[`Int; ty_bkt]) @
    [ `Type("Int", `Int);

      `Function
	("clear1", [ "a", ty_bkt; "i", `Int; "n", `Int ], `Unit,
	 If(Var "i" =: Var "n", Unit,
	    compound
	      [ Let("x", Get(Var "a", Var "i"),
		    Set(Var "a", Var "i",
			Struct[GetValue(Var "x", 0); Bool false]));
		Apply(Var "clear1", [Var "a"; Var "i" +: Int 1; Var "n"]) ]));

      `Function("clear", [ "a", `Array(`Struct[`Int; ty_bkt]);
			   "i", `Int ], `Unit,
		If(Var "i" =: Length(Var "a"), Unit,
		   compound
		     [ Let("nb", Get(Var "a", Var "i"),
			   Apply(Var "clear1", [ GetValue(Var "nb", 1);
						 Int 0;
						 GetValue(Var "nb", 0) ]));
		       Apply(Var "clear", [Var "a"; Var "i" +: Int 1]) ]));

      `Function("abs", ["n", `Int], `Int,
		If(Var "n" >=: Int 0, Var "n", Int 0 -: Var "n"));

      (* Add the reference to the hash table. *)
      `Function("add", [ "a", `Array(`Struct[`Int; ty_bkt]);
			 "p", `Reference ], `Unit,
		Let("h", Apply(Var "abs", [Cast(Var "p", "Int") %: Int q]),
		    Set(Var "a", Var "h",
			Let("nb", Get(Var "a", Var "h"),
			    Struct
			      [ GetValue(Var "nb", 0) +: Int 1;
				Apply(Var "append",
				      [ GetValue(Var "nb", 1);
					Struct[Var "p"; Bool false ] ])]))));

      `Function
	("mark1", [ "a", ty_bkt;
		    "p", `Reference;
		    "i", `Int ], `Bool,
	 Let("n", Length(Var "a"),
	     If(Var "i" =: Var "n",
		compound
		  [ Printf("WARNING: Pointer not found: ", []);
		    Print(Cast(Var "p", "Int"));
		    Printf("\n", []);
		    Bool false ],
		Let("p2", Get(Var "a", Var "i"),
		    If(Cast(GetValue(Var "p2", 0), "Int") =:
			Cast(Var "p", "Int"),
		       If(GetValue(Var "p2", 1), Bool false,
			  compound
			    [ Set(Var "a", Var "i",
				  Struct[GetValue(Var "p2", 0);
					 Bool true]);
			      Bool true ]),
		       Apply(Var "mark1", [ Var "a";
					    Var "p";
					    Var "i" +: Int 1 ]))))));

      `Function("mark0", [ "a", `Array(`Struct[`Int; ty_bkt]);
			   "p", `Reference ], `Bool,
		Let("h", Apply(Var "abs", [Cast(Var "p", "Int") %: Int q]),
		    Apply(Var "mark1", [ GetValue(Get(Var "a", Var "h"), 1);
					 Var "p";
					 Int 0 ])));

      `Function
	("sweep1", [ "a", ty_bkt; "i", `Int; "n", `Int ], `Struct[`Int; ty_bkt],
	 If(Var "i" =: Var "n", Struct[Var "n"; Var "a"],
	    Let("p", Get(Var "a", Var "i"),
		compound
		  [ If(GetValue(Var "p", 1),
		       Apply(Var "sweep1", [ Var "a";
					     Var "i" +: Int 1;
					     Var "n" ]),
		       compound
			 [ Print(GetValue(Var "p", 0));
			   Printf("\n", []);
			   Set(Var "a", Var "i",
			       Get(Var "a", Length(Var "a") -: Int 1));
			   Apply(Var "sweep1", [ Var "a";
						 Var "i";
						 Var "n" -: Int 1 ]) ])])));

      `Function("sweep", [ "a", `Array(`Struct[`Int; ty_bkt]);
			   "i", `Int ], `Unit,
		If(Var "i" =: Length(Var "a"), Unit,
		   compound
		     [ Set(Var "a", Var "i",
			   Let("nb", Get(Var "a", Var "i"),
			       Apply(Var "sweep1",
				     [ GetValue(Var "nb", 1);
				       Int 0;
				       GetValue(Var "nb", 0) ])));
		       Apply(Var "sweep", [ Var "a";
					    Var "i" +: Int 1 ]) ]));

      `Function("loop1", [ "a", `Array(`Struct[`Int; ty_bkt]);
			   "b", `Array `Reference;
			   "n", `Int ], `Unit,
		If(Var "n" =: Length(Var "b"), Unit,
		   Let("x", Construct("Int", Var "n"),
		       compound
			 [ Apply(Var "add", [Var "a"; Var "x"]);
			   Set(Var "b", Var "n", Var "x");
			   Apply(Var "loop1",
				 [ Var "a";
				   Var "b";
				   Var "n" +: Int 1 ])])));

      `Function("loop2", [ "a", `Array(`Struct[`Int; ty_bkt]);
			   "b", `Array `Reference;
			   "i", `Int ], `Unit,
		If(Var "i" =: Length(Var "b"), Unit,
		   compound
		     [ Apply(Var "mark0", [ Var "a";
					    Get(Var "b", Var "i") ]);
		       Apply(Var "loop2", [ Var "a";
					    Var "b";
					    Var "i" +: Int 1 ]) ])) ] @
    List.map
    (fun n ->
       `Expr(Let("a", Alloc(Int q, Struct[Int 0;
					  Alloc(Int 0,
						Struct[Construct("Int", Int 0);
						       Bool false])]),
		 Let("b", Alloc(Int n, Construct("Int", Int 0)),
		     compound
		       [ Printf("\nHash table benchmark: n=%d\n", [Int n]);
			 Apply(Var "add",
			       [Var "a"; Construct("Int", Int(-1))]);
			 Print(Var "a");
			 Printf("\n", []);
			 Apply(Var "loop1", [ Var "a";
					      Var "b";
					      Int 0 ]);
			 Apply(Var "add",
			       [Var "a"; Construct("Int", Int(-2))]);
			 Apply(Var "clear", [ Var "a"; Int 0 ]);
			 Apply(Var "loop2", [ Var "a";
					      Var "b";
					      Int 0 ]);
			 Apply(Var "sweep", [ Var "a"; Int 0 ]);
		       ]))))
    ns

(** Bubble sort floating point numbers. *)
let bubble_sort ns =
  let ty = `Float in
  [ `Function
      ("bubble_sort_loop2", ["a", `Array ty; "i", `Int; "j", `Int], `Unit,
       If(Var "j" >: Var "i", Unit,
          Let("aj0", Get(Var "a", Var "j"),
              Let("aj1", Get(Var "a", Var "j" +: Int 1),
                  compound
                    [ If(Var "aj0" >=: Var "aj1", Unit,
                         compound
                           [ Set(Var "a", Var "j", Var "aj1");
                             Set(Var "a", Var "j" +: Int 1, Var "aj0") ]);
                      Apply(Var "bubble_sort_loop2",
                            [Var "a"; Var "i"; Var "j" +: Int 1]) ]))));

    `Function
      ("bubble_sort_loop1", ["a", `Array ty; "i", `Int], `Unit,
       If(Var "i" <: Int 0, Unit,
          compound
            [ Apply(Var "bubble_sort_loop2", [Var "a"; Var "i"; Int 0]);
              Apply(Var "bubble_sort_loop1", [Var "a"; Var "i" -: Int 1]) ]));

    `Function
      ("bubble_sort", ["a", `Array ty], `Unit,
       Apply(Var "bubble_sort_loop1", [Var "a"; Length(Var "a") -: Int 2]));

    `Extern("sin", [`Float], `Float);

    `Function
      ("init", ["a", `Array ty; "i", `Int], `Array ty,
       If(Var "i" =: Length(Var "a"), Var "a",
          compound
            [ Set(Var "a", Var "i",
		  Apply(Var "sin",
			[Float 3.0 *: (floatOfInt(Var "i") /:
					 floatOfInt(Length(Var "a")))]));
              Apply(Var "init", [Var "a"; Var "i" +: Int 1]) ])) ] @
    List.map
    (fun n ->
       `Expr
	 (compound
	    [ Printf("\nBubble sort benchmark: n=%d\n", [Int n]);
	      Apply(Var "bubble_sort",
		    [Apply(Var "init", [Alloc(Int n, Float 0.0); Int 0])]) ]))
    ns

(** Solve the 8-queens problems "n" times in parallel. *)
let threads n =
  [ `Function
      ("worker", ["id", `Int], `Unit,
	 let n = 8 in
	 compound
	   [ Printf("Queens\n", []);
	     Printf("%d\n", [Apply(Var "search",
				   [Var "f";
				    Int n;
				    nil;
				    Apply(Var "ps", [Int n; Int 0; Int 0]);
				    Int 0])]) ]);
    
    `Function
      ("mk_thread", ["ij", `Struct[`Int; `Int]], `Unit,
       Let("i", GetValue(Var "ij", 0),
	   Let("j", GetValue(Var "ij", 1),
	       If(Var "i" =: Var "j", Unit,
		  If(Var "i" +: Int 1 =: Var "j",
		     Apply(Var "worker", [Int n]),
		     Let("m", Var "i" +: (Var "j" -: Var "i") /: Int 2,
			 Let("thread",
			     CreateThread(Var "mk_thread",
					  Struct[Var "m"; Var "j"]),
			     compound
			       [ Apply(Var "mk_thread",
				       [Struct[Var "i"; Var "m"]]);
				 JoinThread(Var "thread") ])))))));

    `Expr
      (compound
	 [ Printf("%dx %d-queens\n", [Int n; Int 8]);
	   Apply(Var "mk_thread", [Struct[Int 0; Int n]]) ]) ]

(** Increment an atomic counter from two threads simultaneously. *)
let atomic =
  [ `Function
      ("worker", ["args", `Struct[`Int; `Array `Int; `Int]], `Unit,
       Let("m", GetValue(Var "args", 0),
	   Let("a", GetValue(Var "args", 1),
	       Let("i", GetValue(Var "args", 2),
		   compound
		     [ lockMutex(Var "m");
		       Set(Var "a", Int 0, Int 1 +: Get(Var "a", Int 0));
		       UnlockMutex(Var "m");
		       If(Var "i" =: Int 100000, Unit,
			  Apply(Var "worker",
				[Struct[Var "m";
					Var "a";
					Var "i" +: Int 1]]))]))));

    `Expr(Let("m", CreateMutex,
	      Let("a", Alloc(Int 1, Int 0),
		  compound
		    [ Printf("Incrementing atomic counter\n", []);
		      Let("args", Struct[Var "m"; Var "a"; Int 1],
			  Let("t1", CreateThread(Var "worker", Var "args"),
			      Let("t2", CreateThread(Var "worker", Var "args"),
				  compound
				    [ JoinThread(Var "t1");
				      JoinThread(Var "t2") ])));
		      Printf("n=%d\n", [Get(Var "a", Int 0)]) ]))) ]

let ray args : Hlvm.t list =
  let rec lets = function
    | [], x -> x
    | (var, expr)::xs, x -> Let(var, expr, lets(xs, x)) in
  let of_string str =
    let copy i = Set(Var "s", Int i, Byte(Char.code str.[i])) in
    Let("s", Alloc(Int(String.length str + 1), Byte 0),
	compound(List.init (String.length str) copy @
		   [ AddressOf(Var "s") ])) in
  let string = `Int in
  let stream = `Int in
  let ( *| ) s r =
    Struct(List.init 3 (fun i -> s *: GetValue(r, i))) in
  let ( +| ) a b =
    Struct(List.init 3 (fun i -> GetValue(a, i) +: GetValue(b, i))) in
  let ( -| ) a b =
    Struct(List.init 3 (fun i -> GetValue(a, i) -: GetValue(b, i))) in
  let dot a b =
    lets(["a", a; "b", b],
	  GetValue(Var "a", 0) *: GetValue(Var "b", 0) +:
	    GetValue(Var "a", 1) *: GetValue(Var "b", 1) +:
	    GetValue(Var "a", 2) *: GetValue(Var "b", 2)) in
  let length r =
    Let("r", r, Apply(Var "sqrt", [dot (Var "r") (Var "r")])) in
  let unitise a =
    Let("a", a, Float 1.0 /: length(Var "a") *| Var "a") in
  let max(a, b) =
    lets(["a", a; "b", b],
	 If(Var "a" >=: Var "b", Var "a", Var "b")) in
  let sqr x = Let("x", x, Var "x" *: Var "x") in
  let zero = Struct[Float 0.0; Float 0.0; Float 0.0] in
  let nohit = Struct[Float infinity; zero] in
  let ss = 4 in
(*
  let array (x, xs) =
    Let("arr", Alloc(Int(1 + List.length xs), x),
	compound
	  (List.mapi (fun i x -> Set(Var "arr", Int(1+i), x)) xs @
	     [Var "arr"])) in
*)
  let vec3 = `Struct[`Float; `Float; `Float] in
  let scene = `Struct[vec3; `Float; `Reference] in
  [ `Extern("sqrt", [`Float], `Float);
    `Extern("fopen", [string; string], stream);
    `Extern("fputc", [`Int; stream], `Unit);
    `Extern("fputs", [`Int; stream], `Unit);
    `Extern("fclose", [stream], `Unit);

    `Type("Sphere", `Unit);
    `Type("Group", `Struct[scene; scene; scene; scene; scene]);

    `Function
      ("intersect", [ "d", vec3;
		      "hit", `Struct[`Float; vec3];
		      "scene", scene ],
       `Struct[`Float; vec3],
       lets([ "l", GetValue(Var "hit", 0);
	      "c", GetValue(Var "scene", 0);
	      "r2", GetValue(Var "scene", 1);
	      "s", GetValue(Var "scene", 2);
	      "disc2", dot (Var "c") (Var "c") -: Var "r2" ],
	    If(Var "disc2" <: Float 0.0, Var "hit",
	       lets([ "b", dot (Var "c") (Var "d");
		      "b2", sqr(Var "b") ],
		    If(Var "b2" <: Var "disc2", Var "hit",
		       lets([ "disc", Apply(Var "sqrt",
					    [Var "b2" -: Var "disc2"]);
			      "t1", Var "b" -: Var "disc" ],
			    Let("l'", If(Var "t1" >: Float 0.0,
					 Var "t1",
					 Var "b" -: Var "disc"),
				If(Var "l'" >=: Var "l", Var "hit",
				   If(IsType(Var "s", "Sphere"),
				      Struct[ Var "l'";
					      unitise(Var "l'" *| Var "d" -|
							  Var "c") ],
				      Let("g", Cast(Var "s", "Group"),
					  List.fold_left
					    (fun hit scene ->
					       Apply(Var "intersect",
						     [Var "d"; hit; scene]))
					    (Var "hit")
					    (List.init 5
					       (fun i -> GetValue(Var "g", i)))))))))))));

    `Function
      ("intersect'", [ "o", vec3;
		       "d", vec3;
		       "scene", scene ],
       `Bool,
       lets([ "c", GetValue(Var "scene", 0);
	      "r2", GetValue(Var "scene", 1);
	      "s", GetValue(Var "scene", 2);
	      "v", Var "c" -| Var "o";
              "b", dot (Var "v") (Var "d");
	      "disc2", sqr(Var "b") -: dot (Var "v") (Var "v") +: Var "r2" ],
	    (Var "disc2" >=: Float 0.0) &&:
	      (Var "b" +: Apply(Var "sqrt", [Var "disc2"]) >=: Float 0.0) &&:
	      (IsType(Var "s", "Sphere") ||:
		 Let("g", Cast(Var "s", "Group"),
		     Apply(Var "intersect'",
			   [Var "o"; Var "d"; GetValue(Var "g", 0)]) ||:
		     Apply(Var "intersect'",
			   [Var "o"; Var "d"; GetValue(Var "g", 1)]) ||:
		     Apply(Var "intersect'",
			   [Var "o"; Var "d"; GetValue(Var "g", 2)]) ||:
		     Apply(Var "intersect'",
			   [Var "o"; Var "d"; GetValue(Var "g", 3)]) ||:
		     Apply(Var "intersect'",
			   [Var "o"; Var "d"; GetValue(Var "g", 4)])))));

    `Function
      ("bound", [ "b", `Struct[vec3; `Float];
		  "scene", scene ], `Struct[vec3; `Float],
       lets([ "c'", GetValue(Var "scene", 0);
	      "r'", GetValue(Var "scene", 1);
	      "scene", GetValue(Var "scene", 2) ],
	    If(IsType(Var "scene", "Sphere"),
	       lets([ "c", GetValue(Var "b", 0);
		      "r", GetValue(Var "b", 1) ],
		    Struct[Var "c";
			   max(Var "r",
			       length(Var "c" -| Var "c'") +: Var "r'")]),
	       Let("g", Cast(Var "scene", "Group"),
		   let f b i = Apply(Var "bound", [b; GetValue(Var "g", i)]) in
		   f (f (f (f (f (Var "b") 0) 1) 2) 3) 4))));

    `Function
      ("create", ["level", `Int; "c", vec3; "r", `Float], scene,
       Let("obj", Struct[Var "c"; sqr(Var "r"); Construct("Sphere", Unit)],
	   If(Var "level" =: Int 1, Var "obj",
	      Let("a", Float(3.0 /. sqrt 12.0) *: Var "r",
		  let a = Var "a" in
		  let aux x' z' =
		    Apply(Var "create", [ Var "level" -: Int 1;
					  Var "c" +| Struct[x'; a; z'];
					  Float 0.5 *: Var "r" ]) in
		  lets([ "g",
			 Construct("Group",
				   Struct[Var "obj";
					  aux (~-: a) (~-: a);
					  aux a (~-: a);
					  aux (~-: a) a;
					  aux a a]);
			 "scene",
			 Struct[Var "c"; Float 0.0; Var "g"] ],
		       Let("b",
			   Apply(Var "bound",
				 [Struct
				    [Var "c" +|
					 Struct[Float 0.0; Var "r"; Float 0.0];
				     Float 3.0 *: Var "r"];
				  Var "scene"]),
			   Struct[GetValue(Var "b", 0);
				  sqr(GetValue(Var "b", 1));
				  Var "g"]))))));

    `Function
      ("ray_trace", ["scene", scene; "light", vec3; "dir", vec3], `Float,
       lets([ "ln", Apply(Var "intersect", [ Var "dir"; nohit; Var "scene" ]);
	      "l", GetValue(Var "ln", 0) ],
	    If(Var "l" =: Float infinity, Float 0.0,
	       lets([ "n", GetValue(Var "ln", 1);
		      "g", dot (Var "n") (Var "light")],
		    If(Var "g" <=: Float 0.0, Float 0.0,
		       Let("p", Var "l" *| Var "dir" +|
			       Float(sqrt epsilon_float) *| Var "n",
			   let hit =
			     Apply(Var "intersect'",
				   [ Var "p"; Var "light"; Var "scene" ]) in
			   If(hit, Float 0.0, Var "g")))))));

    `Function
      ("loop_x", [ "light", vec3;
		   "scene", scene;
		   "n", `Int;
		   "out", `Array `Byte;
		   "y", `Int;
		   "x", `Int ], `Unit,
       let aux x d =
	 floatOfInt x -: floatOfInt(Var "n") /: Float 2.0 +: 
	   Float(float d) /: Float(float ss) in
       let ray =
	 let expr = ref(Float 0.0) in
	 for dx=0 to ss-1 do
	   for dy=0 to ss-1 do
	     expr := !expr +:
	       Apply(Var "ray_trace",
		     [Var "scene";
		      Var "light";
		      unitise(Struct[ aux (Var "x") dx;
				      aux (Var "y") dy;
				      floatOfInt(Var "n") ])])
	   done
	 done;
	 !expr in
       If(Var "x" =: Var "n", Unit,
	  compound
	    [ Set(Var "out", Var "x" +: Var "n" *:
		    (Var "n" -: Var "y" -: Int 1),
		  IntOfFloat
		    (`Byte,
		     Float 0.5 +: Float 255.0 /: Float(float(ss*ss)) *: ray));
	      Apply(Var "loop_x", [ Var "light";
				    Var "scene";
				    Var "n";
				    Var "out";
				    Var "y";
				    Var "x" +: Int 1 ]) ]));

    `Function
      ("worker", [ "args", `Struct[ `Int;
				    `Array `Int;
				    `Struct[ vec3;
					     scene;
					     `Int;
					     `Array `Byte ] ] ], `Unit,
       compound
	 [ lockMutex(GetValue(Var "args", 0));
	   Let("a", GetValue(Var "args", 1),
	       Let("y", Get(Var "a", Int 0),
		   compound
		     [ If(Var "y" <: Int 0, Unit,
			  Set(Var "a", Int 0, Var "y" -: Int 1));
		       UnlockMutex(GetValue(Var "args", 0));
		       If(Var "y" <: Int 0, Unit,
			  compound
			    [ Let("args", GetValue(Var "args", 2),
				  Apply(Var "loop_x",
					[ GetValue(Var "args", 0);
					  GetValue(Var "args", 1);
					  GetValue(Var "args", 2);
					  GetValue(Var "args", 3);
					  Var "y";
					  Int 0 ]));
			      Apply(Var "worker", [Var "args"]) ]) ])) ]);
    
    `Extern("fwrite", [`Int; `Int; `Int; stream], `Unit) ] @
    
    (List.map
       (fun (file, n_threads, level, n) ->
	  [ `Function
	      ("loop_y", [ "light", vec3;
			   "scene", scene;
			   "n", `Int;
			   "out", stream;
			   "y", `Int ], `Unit,
	       Let("image", Alloc(Var "n" *: Var "n", Byte 0),
		   Let("args", Struct [ CreateMutex;
					Alloc(Int 1, Var "y");
					Struct[ Var "light";
						Var "scene";
						Var "n";
						Var "image" ] ],
		       let rec mk_threads = function
			 | 0 -> Unit
			 | n ->
			     Let("t", CreateThread(Var "worker", Var "args"),
				 compound
				   [ mk_threads(n-1);
				     JoinThread(Var "t") ]) in
		       compound
			 [ mk_threads n_threads;
			   Apply(Var "fwrite", [ AddressOf(Var "image");
						 Int 1;
						 Var "n" *: Var "n";
						 Var "out" ]) ])));
	    
	    (* FIXME: Taking the address of a byte array and passing it to C
	       as a string is naughty because the GC might collect it
	       (although the current one will not). *)
	    `Expr
	      (Let("out", Apply(Var "fopen", [of_string file; of_string "w"]),
		   compound
		     [ Printf("Ray trace: %d threads %dx%d %d levels\n",
			      [Int n_threads; Int n; Int n; Int level]);
		       (*
			 Print(Apply(Var "create",
			 [Int 3;
			 Struct[Float 0.0; Float(-1.0); Float 4.0];
			 Float 1.0]));
		       *)
		       Apply(Var "fputs",
			     [of_string(sprintf "P5\n%d %d\n255\n" n n);
					   Var "out"]);
		       Apply(Var "loop_y",
			     [unitise
				(Struct[Float 1.0; Float 3.0; Float(-2.0)]);
			      Apply(Var "create",
				    [Int level;
				     Struct[Float 0.0; Float(-1.0); Float 4.0];
				     Float 1.0]);
			      Int n;
			      Var "out";
			      Int(n-1)]);
		       Apply(Var "fclose", [Var "out"]) ])) ])
       args
    |> List.flatten)

(** Main program. *)
let () =
  let defs =
    if !Options.tco then
      queens [8; 8; 9; 10; 11] @
(*
      tco 100000000 @
	boehm @
	queens [8; 8; 9; 10; 11] @
	threads 8 @
	tuples @
	trig @
	curry @
	fib [10; 40] @
	ffib [10.0; 40.0] @
	sieve [1000; 100000000] @
	mandelbrot [1; 77] @
	mandelbrot2 [1; 77] @
	fold [1000; 100000000] @
	queens [8;8;9;10] @
	bubble_sort [100; 10000] @
	gc [1000; 1000000] @
	list [1000; 3000000] @
	ray(List.init 24 (fun i -> "image_11_2048.pgm", 8-i/3, 11, 2048)) @
*)
	[]
    else
      boehm @
      queens [8; 8; 9; 10; 11] @
	threads 8 @
	tuples @
	trig @
	curry @
	fib [10; 40] @
	ffib [10.0; 40.0] @
	sieve [1000] @
	mandelbrot [1; 77] @
	mandelbrot2 [1; 77] @
	fold [1000] @
	queens [8;8;9;10] @
	bubble_sort [100; 10000] @
	gc [1000] @
	list [1000] @
	ray(List.init 24 (fun i -> "image_11_2048.pgm", 8-i/3, 11, 2048)) @
	[]
  in
  List.iter Hlvm.eval defs;
  Hlvm.save()
