(* Simplest possible use for embedded Prolog
 *
 * Compile with:
 *   ocamlc -pp '../../fan.run -keep -plugin plc' -o demo1 demo1.ml
 *
 * Add -dsource to command to see what actually gets compiled
 * Executing ./demo1 should output cheese
 *)

%plc{
likes(wallace,cheese).
likes(grommit,cheese).
};;

let _ = likes_oc (fun b -> print_endline (string_of_plval b)) (Cheese);;
