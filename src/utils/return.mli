
type 'a t 

(**
   Example:
   {[
   let sum_until_first_negative list =
      Return.cc @@ fun r ->
        List.fold list ~init:0
        ~f:(fun acc x -> if x >= 0 then acc + x else Return.k r acc)
   ]}

   This module has its dynamic semantics, in general, [Return.k] throws an exception
   (if executed) and would be caught by the nearest [Return.cc].

   Note that, you should avoid use [try with _] to catch the exception unintentionally.

   Could be simplified using a DDSL.
   {[
   {:cc@id|
   begin 
   for i = 0 to el - 1 do
     if get str (diff + i) <> get p i then
     {:ret@id| false |}
   done;
   true
   end
   |}
   ]}

   This could do more error check, like typo checks
 *)      
val k : 'a t -> 'a -> 'b

val cc : ('a t -> 'a) -> 'a


    
