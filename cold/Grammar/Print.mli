module Make (Structure : Structure.S) : sig
  value flatten_tree : Structure.tree -> list (list Structure.symbol);
  value print_symbol : Format.formatter -> Structure.symbol -> unit;
  value print_meta :
    Format.formatter -> string -> list Structure.symbol -> unit;
  value print_symbol1 : Format.formatter -> Structure.symbol -> unit;
  value print_rule : Format.formatter -> list Structure.symbol -> unit;
  value print_level :
    Format.formatter ->
    (Format.formatter -> unit -> unit) ->
    list (list Structure.symbol) -> unit;
  value levels : Format.formatter -> list Structure.level -> unit;
  value entry : Format.formatter -> Structure.internal_entry -> unit;
end;

module MakeDump (Structure : Structure.S) : sig
  value print_symbol : Format.formatter -> Structure.symbol -> unit;
  value print_meta :
    Format.formatter -> string -> list Structure.symbol -> unit;
  value print_symbol1 : Format.formatter -> Structure.symbol -> unit;
  value print_rule : Format.formatter -> list Structure.symbol -> unit;
  value print_level :
    Format.formatter ->
    (Format.formatter -> unit -> unit) ->
    list (list Structure.symbol) -> unit;
  value levels : Format.formatter -> list Structure.level -> unit;
  value entry : Format.formatter -> Structure.internal_entry -> unit;
end;
