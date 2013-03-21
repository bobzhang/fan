open Format
open LibUtil
open Ast
exception Unhandled of ctyp
exception Finished of exp
let _loc = FanLoc.ghost
let unit_literal = `Id (_loc, (`Uid (_loc, "()")))
let x ?(off= 0)  (i : int) =
  if off > 25
  then invalid_arg "unsupported offset in x "
  else
    (let base = let open Char in ((code 'a') + off) |> chr in
     "_" ^ ((String.of_char base) ^ (string_of_int i)))
let xid ?(off= 0)  (i : int) = `Lid (_loc, (x ~off i))
let allx ?(off= 0)  i = "all" ^ (x ~off i)
let allxid ?(off= 0)  i = `Lid (_loc, (allx ~off i))
let check_valid str =
  let len = String.length str in
  if
    not
      ((len > 1) &&
         ((not (Char.is_digit (str.[1]))) &&
            (not (String.starts_with str "all_"))))
  then
    (eprintf "%s is not a valid name" str;
     eprintf
       "For valid name its length should be more than 1\ncan not be a-[digit], can not start with [all_]";
     exit 2)
  else ()
let conversion_table: (string,string) Hashtbl.t = Hashtbl.create 50
