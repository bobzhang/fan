

include module type of String
val init : int -> (int -> char) -> string
val is_empty : string -> bool
val not_empty : string -> bool
val starts_with : string -> string -> bool
val ends_with : string -> string -> bool
val of_char : char -> string
val drop_while : (char -> bool) -> string -> string
val neg : string -> string
val map : (char -> char) -> string -> string
val lowercase : string -> string
val find_from : string -> int -> string -> int
val find : string -> string -> int
val split : string -> string -> string * string
val rfind_from : string -> int -> string -> int
val rfind : string -> string -> int
val nsplit : string -> string -> string list



val edit_distance : string -> string -> int -> int option
(** [edit_distance a b cutoff] computes the edit distance between
    strings [a] and [b]. To help efficiency, it uses a cutoff: if the
    distance [d] is smaller than [cutoff], it returns [Some d], else
    [None].

    The distance algorithm currently used is Damerau-Levenshtein: it
    computes the number of insertion, deletion, substitution of
    letters, or swapping of adjacent letters to go from one word to the
    other. The particular algorithm may change in the future.

    Borrowed from [Misc] module in compiler
*)
    
