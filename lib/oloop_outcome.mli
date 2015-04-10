(** Outcome of evaluating a phrase. Attempting to evaluate an
    arbitrary string as an OCaml phrase can lead to a variety of
    outcomes:

    - There can be a syntax or type error, preventing
    evaluation. Nonetheless, we consider information about the error
    to be the outcome.

    - The phrase type checks, the OCaml toplevel evaluates it, but an
    exception occurs. The outcome is information about the exception,
    and also any possible output printed to stdout/stderr.

    - The phrase type checks, the OCaml toplevel evaluates it, and
    there is no exception. The outcome is any possible output printed
    to stdout/stderr and the semantic result, e.g. "2+3" evaluates to
    "5".

*)
open Core_kernel.Std

type eval_error = [
| `Lexer of Lexer.error * Location.t
| `Syntaxerr of Syntaxerr.error
| `Typedecl of Location.t * Typedecl.error
| `Typetexp of Location.t * Env.t * Typetexp.error
| `Typecore of Location.t * Env.t * Typecore.error
| `Symtable of Symtable.error
] with sexp

type error = [
| eval_error
| `Internal_error of Exn.t
]

val deserialize_to_error : Oloop_types.serializable_error -> error

val location_of_error : error -> Location.t option
(** [location_of_error e] returns the error location if any is present. *)

val report_error : ?msg_with_location: bool ->
                   Format.formatter -> error -> unit
(** [report_error ppf e] write an error message corresponding to [e]
    to the formatter [ppf] just as the toploop would do it. *)

val to_error : error * string -> Error.t
(** [to_error(e, msg)] returns an [Error.t] value corresponding to
    the error [e] with message [msg]. *)

