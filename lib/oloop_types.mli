(** Module shared between Oloop and the toploop program oloop-top to
    ensure type communication. *)

type serializable_out_value
(** Type equivalent to [Outcometree.out_value] except that it is
    serializable. *)

(** Type equivalent to [Outcometree.out_phrase] except that [out_value]
    is replaced by the above serializable version. *)
type serializable_out_phrase =
  | Eval of serializable_out_value * Outcometree.out_type
  | Signature of (Outcometree.out_sig_item * serializable_out_value option) list
  | Exception of (exn * serializable_out_value)

val empty : serializable_out_phrase
(** Out phrase that indicates that no particular value is returned by
    the toploop. *)

val of_outcometree_phrase : Outcometree.out_phrase -> serializable_out_phrase
(** Transform the usual [Outcometree.out_phrase] to a serializable
    version. *)

val to_outcometree_phrase : serializable_out_phrase -> Outcometree.out_phrase
(** Recover the origina outcometree from the serialized version (with
    a slight approximation for custom printers).  *)

type serializable_typedecl_error

(** Enumeration of errors. *)
type serializable_error =
  [ `Lexer of Location.t * Lexer.error
  | `Syntaxerr of Syntaxerr.error
  | `Typedecl of Location.t * serializable_typedecl_error
  | `Typetexp of Location.t * Env.summary * Typetexp.error
  | `Typecore of Location.t * Env.summary * Typecore.error
  | `Symtable of Symtable.error
  | `Internal_error of string ]

val serialize_typedecl_error : Typedecl.error -> serializable_typedecl_error

val deserialize_typedecl_error :
  env_of_summary:(Env.summary -> Env.t) -> serializable_typedecl_error ->
  Typedecl.error

type out_phrase_or_error =
  | Ok of serializable_out_phrase * bool * (Location.t * Warnings.t) list
  | Error of (serializable_error * string)

val send_out_phrase_or_error : out_channel -> out_phrase_or_error -> unit
(** [send_out_phrase_or_error ch p] serialize and send the phrase or
    error [p] on the channel [ch].  The channel is flushed after. *)

val end_output : char
(** Char indicating the end of stdout and stderr for this command. *)
