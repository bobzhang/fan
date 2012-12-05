open LibUtil;
(** A type for stream filters. *)
type  stream_filter 'a 'loc =
     Stream.t ('a *  'loc)-> Stream.t ('a*  'loc);
 (** The type for this filter chain.
     A basic implementation just store the [is_keyword] function given
     by [mk] and use it in the [filter] function. *)




