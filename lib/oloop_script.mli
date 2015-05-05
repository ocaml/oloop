(** A toplevel script. Conceptually the same as a script accepted by
    the OCaml toplevel, so toplevel directives are
    allowed. Differences are:

    - There is no requirement that the script be syntactically valid
    or well typed. You may well be using Oloop to demonstrate a syntax
    or type error. OCaml returning an error is a valid result from
    Oloop's perspective.

    - We detect comments in the form {v (* part X *) v}, allowing to
    split a file into multiple parts. The beginning of the file is
    part 0 by default, and you must not explicitly write {v (* part 0
    *) v}. Part numbers must be in order and are treated as floats to
    allow easily adding parts between existing parts.

    - Our parser is not as smart as the one in the compiler. We
    require phrases to be terminated by double semicolons, except for
    the last phrase in each part. This is also deliberate due to the
    fact that we want to simulate what is done interactively in the
    toplevel, as the next point explains.

    - Our notion of phrase is not identical to the compiler's
    [toplevel_phrase]. In Oloop, [type t = int type u = string;;] is a
    single phrase while the compiler parses this as 2
    [toplevel_phrase]s. Our goal in defining a single phrase is to let
    you control when output is printed, just as you can when
    interactively entering text in the toplevel. Providing [type t =
    int;; type u = string;;] will cause Oloop to evaluate [type t =
    int], print the toplevel's output, evaluate [type u = string], and
    print the toplevel's result. In contrast, [type t = int type u =
    string;;] will cause both type declarations to be evaluated, and
    then print out the result once.
*)
open Core.Std
open Async.Std

(** A single part consists of a part number and its content. Almost
    surely you want to call [phrases_of_string] on the
    content. However, we provide the plain content here because you
    may also want to display exactly what was input. *)
type part = {
  number : float;
  content : string;
}

(** A full script is a sequence of parts, guaranteed to be in order by
    part number. *)
type t = private part list

(** Parse given file. *)
val of_file : string -> t Or_error.t Deferred.t

(** Parse given file contents. The [filename] is only for error
    messages. *)
val of_string : filename:string -> string -> t Or_error.t

(** Split string into phrases, which are assumed to be terminated by a
    double semicolon at the end of a line. It is okay for the last
    phrase to not end with a double semicolon. *)
val phrases_of_string : string -> string list

(** Evaluated scripts. *)
module Evaluated : sig

  (** A phrase and its outcome. *)
  type phrase = {
    phrase : string;
    outcome : Oloop_outcome.merged Oloop_outcome.t;
  }

  (** A part and all of its evaluated phrases. *)
  type part = {
    number : float;
    content : string;
    phrases : phrase list;
  }

  type t = part list

  val to_plain_text : t -> string

end
