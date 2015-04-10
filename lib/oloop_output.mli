(** Stdout/stderr handling. Type ['a t] represents the content printed
    out to stdout and stderr by some process. The ['a] parameter can
    be one of the following phantom types:

    - [separate] - Indicates that stdout and stderr are captured
    separately. In this case, you lose information about the relative
    order in which content was printed to stdout vs stderr.

    - [merged] - Indicates that stderr is redirected to stdout, and
    thus you can only get the stdout. In this case, you lose
    information about whether content was printed to stdout or to
    stderr.

    One can imagine a 3rd possibility [interleaved], in which no
    information is lost. This would provide a sequence of content in
    the order printed out, and tagged as being to stdout or to stderr.
    This has not been implemented.
*)
open Core_kernel.Std

type 'a t
(** Type representing the contents of stdout and stderr. *)

type separate (** Stdout and stderr are collected separately. *)
type merged   (** Stderr is redirected to stdout. *)

val stdout : _ t -> string
val stderr : separate t -> string
val stdout_queue : _ t -> string Queue.t
val stderr_queue : separate t -> string Queue.t

type 'a kind
(** Specify whether one wants separate stdout and stderr or not. *)

val separate : separate kind
val merged : merged kind
val kind : _ kind -> [`Separate | `Merged]

val make_unsafe : stdout:(string Queue.t) -> stderr:(string Queue.t) -> _ t
(** It is unsafe because you can provide [stderr] even if you treat
    the result as a [merged t]. In this case, the [stderr] will be
    ignored when you ask for [stdout], which is probably not what
    you expect. *)
