(** Module shared between Oloop and the toploop program oloop-top to
    ensure type communication. *)

(**
 * Library → toploop
 *)

type top_input =
  | Phrase of string
  | Init of string (** [""] if the default .ocamlinit must be sought. *)

val read : in_channel -> top_input

(**
 * Toploop → library
 *)

type serializable_out_value
(** Type equivalent to [Outcometree.out_value] except that it is
    serializable. *)

(** Type equivalent to [Outcometree.out_phrase] except that [out_value]
    is replaced by the above serializable version.
    [Exception_string] is used in case the exception cannot be
    serialized (because of the values it carries) and is then
    converted to a string.
    [Exception_Stack_overflow] is used so that [Stack_overflow] is
    preserved across serialization (so one can catch it with the
    standard exception). *)
type serializable_out_phrase =
  | Eval of serializable_out_value * Outcometree.out_type
  | Signature of (Outcometree.out_sig_item * serializable_out_value option) list
  | Exception of (exn * serializable_out_value)
  | Exception_string of (string * serializable_out_value)
  | Exception_Stack_overflow of serializable_out_value

val empty : serializable_out_phrase
(** Out phrase that indicates that no particular value is returned by
    the toploop. *)

val of_outcometree_phrase : Outcometree.out_phrase -> serializable_out_phrase
(** Transform the usual [Outcometree.out_phrase] to a serializable
    version. *)

val to_outcometree_value : serializable_out_value -> Outcometree.out_value

val to_outcometree_sig :
  Outcometree.out_sig_item * serializable_out_value option ->
  Outcometree.out_sig_item * Outcometree.out_value option

type serializable_typedecl_error
type serializable_typeclass_error

(** Enumeration of errors. *)
type serializable_error =
  [ `Lexer of Location.t * Lexer.error
  | `Syntaxerr of Syntaxerr.error
  | `Typedecl of Location.t * serializable_typedecl_error
  | `Typetexp of Location.t * Env.summary * Typetexp.error
  | `Typecore of Location.t * Env.summary * Typecore.error
  | `Typeclass of Location.t * Env.summary * serializable_typeclass_error
  | `Symtable of Symtable.error
  | `Internal_error of string ]

val serialize_typedecl_error : Typedecl.error -> serializable_typedecl_error

val deserialize_typedecl_error :
  env_of_summary:(Env.summary -> Env.t) -> serializable_typedecl_error ->
  Typedecl.error

val serialize_typeclass_error : Typeclass.error -> serializable_typeclass_error

val deserialize_typeclass_error :
  env_of_summary:(Env.summary -> Env.t) -> serializable_typeclass_error ->
  Typeclass.error

type out_phrase_or_error =
  | Ok of serializable_out_phrase * bool * (Location.t * Warnings.t) list
  | Error of (serializable_error * string)

val send_out_phrase_or_error : out_channel -> out_phrase_or_error -> unit
(** [send_out_phrase_or_error ch p] serialize and send the phrase or
    error [p] on the channel [ch].  The channel is flushed after. *)

val end_output : char
(** Char indicating the end of stdout and stderr for this command. *)


(**
 * Answer to {!Init} requests
 *)

type init_output = { init_ok: bool;
                     init_out: string }

val send_init_outcome : out_channel -> init_output -> unit
