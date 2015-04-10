open Core_kernel.Std
open Async_kernel.Std

module Script : module type of Oloop_script

module Output : module type of Oloop_output
  with type 'a t = 'a Oloop_output.t
  with type separate = Oloop_output.separate
  with type merged = Oloop_output.merged
  with type 'a kind = 'a Oloop_output.kind

module Outcome : module type of Oloop_outcome

type 'a t
(** A handle to a toploop. *)

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

val eval : 'a t -> string -> 'a Outcome.t Deferred.t
(** [eval t phrase] evaluates [phrase] in the toploop [t]. *)

val eval_or_error :
  'a t -> string -> (Outcometree.out_phrase * 'a Output.t) Deferred.Or_error.t
(** Same as {!eval} except that the error is transformed into an
   [Error.t] using the function {!to_error}. *)


(** {2 Miscellaneous} *)

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
