open Core_kernel.Std
open Async_kernel.Std

type 'a t
(** A handle to a toploop. *)

(** Specifying how output should be handled. *)
module Output : sig
    type 'a t
    (** Stdout and stderr output of the toploop. *)

    type separate (** Stdout and stderr are collected separately. *)
    type merged   (** Stderr is redirected to stdout. *)

    val stdout : _ t -> string

    val stderr : separate t -> string

    val stdout_queue : _ t -> string Queue.t

    val stderr_queue : separate t -> string Queue.t

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
             ?no_app_functors: unit ->
             ?principal: unit ->
             ?rectypes: unit ->
             ?short_paths: unit ->
             ?strict_sequence: unit ->
             ?msg_with_location: unit ->
             ?silent_directives: unit ->
             ?determine_deferred: unit ->
             ?determine_lwt: unit ->
             'a Output.kind -> 'a t Or_error.t Deferred.t
(** Create a new toploop.

    The optional arguments [include_dirs] (-I), [init],
    [no_app_functors], [principal], [rectypes], [short_paths] and
    [strict_sequence] correspond activate toploop flags.  By default,
    they are not provided.

    @param msg_with_location if provided, make error messages returned
    by {!eval} contain the location of the error.  The
    location is always accessible using {!location_of_error} which can
    be used to highlight the problematic part of the phrase.

    @param silent_directives if set, the toplevel directives (existing
    ones or new ones) will return an empty structure — thus
    [Oprint.out_phrase] will print nothing.

    @param determine_deferred Automatically determine anonymous
    [Deferred.t] values as Utop does.

    @param determine_lwt Automatically determine anonymous [Lwt.t]
    values as Utop does.

    @param prog is full path to the specially customized toploop that
    you want to run (if for example it is at an unusual location). *)

val close : _ t -> unit Deferred.t
(** Terminates the toplevel. *)

val with_toploop :
  ?prog: string ->
  ?include_dirs: string list ->
  ?init: string ->
  ?no_app_functors: unit ->
  ?principal: unit ->
  ?rectypes: unit ->
  ?short_paths: unit ->
  ?strict_sequence: unit ->
  ?msg_with_location: unit ->
  ?silent_directives: unit ->
  ?determine_deferred: unit ->
  ?determine_lwt: unit ->
  'a Output.kind -> f:('a t -> 'b Deferred.Or_error.t) -> 'b Deferred.Or_error.t
(** [with_toploop kind f] will run [f], closing the toploop and
    freeing its resources whether [f] returns a result or an error.
    This is convenient in order use to the bind operator [>>=?] to
    chain computations in [f]. *)

type eval_error =
  [ `Lexer of Lexer.error * Location.t
  | `Syntaxerr of Syntaxerr.error
  | `Typedecl of Location.t * Typedecl.error
  | `Typetexp of Location.t * Typetexp.error
  | `Typecore of Location.t * Typecore.error
  | `Symtable of Symtable.error
  ] with sexp

type error = [ eval_error | `Internal_error of exn ]

val eval :
  'a t -> string
  -> (Outcometree.out_phrase * 'a Output.t, error * string) Deferred.Result.t
(** [eval t phrase] evaluate [phrase] in the toploop [t] and returns a
    (deferred) couple [(res, out)] where [res] is the result of
    evaluating the [phrase], an {!Outcometree.out_phrase} if [phrase]
    evaluated normally or raised an exception and an [Error(e, msg)]
    if [phrase] did not evaluate correctly be it for a syntax error, a
    type error,..., where [e] ie the error and [msg] is the error
    message displayed in the toplevel.  In both case, [out] is the
    text outputed on stdout and stderr. *)

val eval_or_error :
  'a t -> string -> (Outcometree.out_phrase * 'a Output.t) Deferred.Or_error.t
(** Same as {!eval} except that the error is transformed into an
   [Error.t] using the function {!to_error}. *)


val location_of_error : error -> Location.t option
(** [location_of_error e] returns the error location if any is present. *)

val to_error : error * string -> Error.t
(** [to_error(e, msg)] returns an [Error.t] value corresponding to
    the error [e] with message [msg]. *)

val phrase_remove_underscore_names :
  Outcometree.out_phrase -> Outcometree.out_phrase

val signatures_remove_underscore_names :
  Outcometree.out_sig_item list -> Outcometree.out_sig_item list

(** Copy of the compiler [Location] module, enriched with convetions
    from and to sexp. *)
module Location : sig
    include module type of Location with type t = Location.t

    val sexp_of_t : t -> Sexp.t
    val t_of_sexp : Sexp.t -> t
  end
