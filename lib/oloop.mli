open Core_kernel.Std
open Async_kernel.Std

type 'a t
(** A handle to a toploop. *)

module Output : sig
    type 'a t
    (** Stdout and stderr output of the toploop. *)

    type separate (** Stdout and stderr are collected separately. *)
    type merged   (** Stderr is redirected to stdout. *)

    val stdout : _ t -> string Queue.t

    val stderr : separate t -> string Queue.t

    type 'a kind
    (** Specify whether one wants separate stdout and stderr or not. *)

    val separate : separate kind
    (** Specify that stdout and stderr must be collected separately. *)

    val merged : merged kind
    (** Specify that stdout and stderr must be collected in a single
        queue, interleaving them as they appear on the terminal.  This
        mode does not allow to distinguish between the output of each
        channel unfortunately. *)
  end

val create : ?prog: string ->
             ?include_dirs: string list ->
             ?init: string ->
             ?no_app_functors: bool ->
             ?principal: bool ->
             ?rectypes: bool ->
             ?short_paths: bool ->
             ?strict_sequence: bool ->
             ?msg_with_location: bool ->
             'a Output.kind -> 'a t Or_error.t Deferred.t
(** Create a new toploop.

    The optional arguments [include_dirs] (-I), [init],
    [no_app_functors], [principal], [rectypes], [short_paths] and
    [strict_sequence] correspond activate toploop flags.  By default,
    they are not provided.

    [msg_with_location] make error messages returned by {!eval}
    contain the location of the error (default: [false]).  The
    location is always accessible using {!location_of_error} which can
    be used to highlight the problematic part of the phrase.

    [prog] is the specially customized toploop that you want to run
    (if for example it is at an unusual location). *)

val close : _ t -> unit Deferred.t
(** Terminates the toplevel. *)


type error =
  [ `Lexer of Lexer.error * Location.t
  | `Syntaxerr of Syntaxerr.error
  | `Typedecl of Location.t * Typedecl.error
  | `Typetexp of Location.t * Typetexp.error
  | `Typecore of Location.t * Typecore.error
  | `Internal_error of exn ]

val eval : 'a t -> string
           -> (Outcometree.out_phrase * 'a Output.t,
              error * string) Deferred.Result.t
(** [eval t phrase] evaluate [phrase] in the toploop [t] and returns a
    (deferred) couple [(res, out)] where [res] is the result of
    evaluating the [phrase], an {!Outcometree.out_phrase} if [phrase]
    evaluated normally or raised an exception and an [Error(e, msg)]
    if [phrase] did not evaluate correctly be it for a syntax error, a
    type error,..., where [e] ie the error and [msg] is the error
    message displayed in the toplevel.  In both case, [out] is the
    text outputed on stdout and stderr. *)


val location_of_error : error -> Location.t option

val print_out_signature :
  Format.formatter -> Outcometree.out_sig_item list -> unit
val print_out_phrase :
  Format.formatter -> Outcometree.out_phrase -> unit
