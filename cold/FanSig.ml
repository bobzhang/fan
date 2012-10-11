type ('a, 'loc) stream_filter =
 (('a * 'loc) Stream.t -> ('a * 'loc) Stream.t)

type quotation = {
                                                                    q_name:
                                                                    string;
                                                                    q_loc:
                                                                    string;
                                                                    q_shift:
                                                                    int;
                                                                    q_contents:
                                                                    string}


type token =
 [ `KEYWORD of string | `SYMBOL of string | `LIDENT of string |
  `UIDENT of string | `ESCAPED_IDENT of string | `INT of (int * string) |
  `INT32 of (int32 * string) | `INT64 of (int64 * string) |
  `NATIVEINT of (nativeint * string) | `FLOAT of (float * string) |
  `CHAR of (char * string) | `STRING of (string * string) |
  `LABEL of string | `OPTLABEL of string | `QUOTATION of quotation |
  `ANTIQUOT of (string * string) | `COMMENT of string | `BLANKS of string |
  `NEWLINE | `LINE_DIRECTIVE of (int * string option) | `EOI ]

type token_filter =
                                                                 (token,
                                                                  FanLoc.t) stream_filter


type filter = {is_kwd:(string -> bool); mutable filter:token_filter}


module type SEntry =
 sig
  type 'a t

  type token_stream

  type internal_entry

  type extend_statment

  type delete_statment

  type action

  type symbol

  type ('a, 'b, 'c) fold

  type ('a, 'b, 'c) foldsep

  type token

  val mk : (string -> 'a t)

  val of_parser : (string -> ((token_stream -> 'a) -> 'a t))

  val setup_parser : ('a t -> ((token_stream -> 'a) -> unit))

  val name : ('a t -> string)

  val print : (Format.formatter -> ('a t -> unit))

  val dump : (Format.formatter -> ('a t -> unit))

  val obj : ('a t -> internal_entry)

  val clear : ('a t -> unit)

  val extend : ('a t -> (extend_statment -> unit))

  val delete_rule : ('a t -> (delete_statment -> unit))

  val srules : ('a t -> ((symbol list * action) list -> symbol))

  val sfold0 : (('a -> ('b -> 'b)) -> ('b -> (_, 'a, 'b) fold))

  val sfold1 : (('a -> ('b -> 'b)) -> ('b -> (_, 'a, 'b) fold))

  val sfold0sep : (('a -> ('b -> 'b)) -> ('b -> (_, 'a, 'b) foldsep))

  val parse : ('a t -> (FanLoc.t -> (char Stream.t -> 'a)))

  val parse_string : ('a t -> (FanLoc.t -> (string -> 'a)))

  val filter_and_parse_tokens : ('a t -> ((token * FanLoc.t) Stream.t -> 'a))

  val parse_origin_tokens : ('a t -> (token_stream -> 'a))

  val mk_action : ('a -> action)

  val string_of_token : (token -> string)

  val debug_origin_token_stream : ('a t -> (token Stream.t -> 'a))

  val debug_filtered_token_stream : ('a t -> (token Stream.t -> 'a))

 end

module type DEntry =
       sig
        include SEntry

        type gram

        val mk : (gram -> (string -> 'a t))

        val of_parser : (gram -> (string -> ((token_stream -> 'a) -> 'a t)))

       end

type assoc = [ `NA | `RA | `LA ]

type position =
                                               [ `First | `Last |
                                                `Before of string |
                                                `After of string |
                                                `Level of string ]

module Grammar =
                                                                    struct
                                                                    module type Action =
                                                                    sig
                                                                    type t

                                                                    val mk :
                                                                    ('a -> t)

                                                                    val get :
                                                                    (t -> 'a)

                                                                    val getf :
                                                                    (t ->
                                                                    ('a ->
                                                                    'b))

                                                                    val getf2 :
                                                                    (t ->
                                                                    ('a ->
                                                                    ('b ->
                                                                    'c)))

                                                                    end

                                                                    module type Structure =
                                                                    sig
                                                                    module
                                                                    Action :
                                                                    Action

                                                                    type gram = 
                                                                    {
                                                                    gfilter:
                                                                    filter;
                                                                    gkeywords:
                                                                    (string,
                                                                    int ref) Hashtbl.t;
                                                                    glexer:
                                                                    (FanLoc.t
                                                                    ->
                                                                    (char Stream.t
                                                                    ->
                                                                    (token *
                                                                    FanLoc.t) Stream.t));
                                                                    warning_verbose:
                                                                    bool ref;
                                                                    error_verbose:
                                                                    bool ref}

                                                                    type internal_entry
                                                                    

                                                                    type tree

                                                                    type token_pattern =
                                                                    ((token
                                                                    -> 
                                                                    bool) *
                                                                    string)

                                                                    type token_info
                                                                    

                                                                    type token_stream =
                                                                    (token *
                                                                    token_info) Stream.t

                                                                    val token_location :
                                                                    (token_info
                                                                    ->
                                                                    FanLoc.t)

                                                                    type symbol =
                                                                    [ 
                                                                    `Smeta of
                                                                    (string *
                                                                    symbol list *
                                                                    Action.t) |
                                                                    `Snterm of
                                                                    internal_entry |
                                                                    `Snterml of
                                                                    (internal_entry *
                                                                    string) |
                                                                    `Slist0 of
                                                                    symbol |
                                                                    `Slist0sep of
                                                                    (symbol *
                                                                    symbol) |
                                                                    `Slist1 of
                                                                    symbol |
                                                                    `Slist1sep of
                                                                    (symbol *
                                                                    symbol) |
                                                                    `Sopt of
                                                                    symbol |
                                                                    `Stry of
                                                                    symbol |
                                                                    `Sself |
                                                                    `Snext |
                                                                    `Stoken of
                                                                    token_pattern |
                                                                    `Skeyword of
                                                                    string |
                                                                    `Stree of
                                                                    tree ]

                                                                    type production_rule =
                                                                    (symbol list *
                                                                    Action.t)

                                                                    type single_extend_statment =
                                                                    (string option *
                                                                    assoc option *
                                                                    production_rule list)

                                                                    type extend_statment =
                                                                    (position option *
                                                                    single_extend_statment list)

                                                                    type delete_statment =
                                                                    symbol list

                                                                    type 
                                                                    ('a, 'b,
                                                                    'c) fold =
                                                                    (internal_entry
                                                                    ->
                                                                    (symbol list
                                                                    ->
                                                                    (('a Stream.t
                                                                    -> 'b) ->
                                                                    ('a Stream.t
                                                                    -> 'c))))

                                                                    type 
                                                                    ('a, 'b,
                                                                    'c) foldsep =
                                                                    (internal_entry
                                                                    ->
                                                                    (symbol list
                                                                    ->
                                                                    (('a Stream.t
                                                                    -> 'b) ->
                                                                    (('a Stream.t
                                                                    -> 
                                                                    unit) ->
                                                                    ('a Stream.t
                                                                    -> 'c)))))

                                                                    end

                                                                    module type Dynamic =
                                                                    sig
                                                                    include
                                                                    Structure

                                                                    val mk :
                                                                    (unit ->
                                                                    gram)

                                                                    include
                                                                    (DEntry
                                                                    with type
                                                                     token_stream :=
                                                                    token_stream
                                                                    and type
                                                                     internal_entry :=
                                                                    internal_entry
                                                                    and type
                                                                     gram :=
                                                                    gram
                                                                    and type
                                                                     action :=
                                                                    Action.t
                                                                    and type
                                                                     extend_statment :=
                                                                    extend_statment
                                                                    and type
                                                                     delete_statment :=
                                                                    delete_statment
                                                                    and type
                                                                     symbol :=
                                                                    symbol
                                                                    and type
                                                                    ('a, 'b,
                                                                    'c) fold :=
                                                                    ('a, 'b,
                                                                    'c) fold
                                                                    and type
                                                                    ('a, 'b,
                                                                    'c) foldsep :=
                                                                    ('a, 'b,
                                                                    'c) foldsep
                                                                    and type
                                                                     token =
                                                                    token)

                                                                    val get_filter :
                                                                    (gram ->
                                                                    token)

                                                                    val lex :
                                                                    (gram ->
                                                                    (FanLoc.t
                                                                    ->
                                                                    (char Stream.t
                                                                    ->
                                                                    (token *
                                                                    FanLoc.t) Stream.t)))

                                                                    val lex_string :
                                                                    (gram ->
                                                                    (FanLoc.t
                                                                    ->
                                                                    (string
                                                                    ->
                                                                    (token *
                                                                    FanLoc.t) Stream.t)))

                                                                    val filter :
                                                                    (gram ->
                                                                    ((token *
                                                                    FanLoc.t) Stream.t
                                                                    ->
                                                                    token_stream))

                                                                    end

                                                                    module type Static =
                                                                    sig
                                                                    include
                                                                    Structure

                                                                    val trace_parser :
                                                                    bool ref

                                                                    val gram :
                                                                    gram

                                                                    include
                                                                    (SEntry
                                                                    with type
                                                                     token_stream :=
                                                                    token_stream
                                                                    and type
                                                                     internal_entry :=
                                                                    internal_entry
                                                                    and type
                                                                     action :=
                                                                    Action.t
                                                                    and type
                                                                     extend_statment :=
                                                                    extend_statment
                                                                    and type
                                                                     delete_statment :=
                                                                    delete_statment
                                                                    and type
                                                                     symbol :=
                                                                    symbol
                                                                    and type
                                                                    ('a, 'b,
                                                                    'c) fold :=
                                                                    ('a, 'b,
                                                                    'c) fold
                                                                    and type
                                                                    ('a, 'b,
                                                                    'c) foldsep :=
                                                                    ('a, 'b,
                                                                    'c) foldsep
                                                                    and type
                                                                     token =
                                                                    token)

                                                                    val get_filter :
                                                                    (unit ->
                                                                    filter)

                                                                    val lex :
                                                                    (FanLoc.t
                                                                    ->
                                                                    (char Stream.t
                                                                    ->
                                                                    (token *
                                                                    FanLoc.t) Stream.t))

                                                                    val lex_string :
                                                                    (FanLoc.t
                                                                    ->
                                                                    (string
                                                                    ->
                                                                    (token *
                                                                    FanLoc.t) Stream.t))

                                                                    val filter :
                                                                    ((token *
                                                                    FanLoc.t) Stream.t
                                                                    ->
                                                                    token_stream)

                                                                    end

                                                                    end
