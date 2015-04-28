(** Outcome of trying to evaluating a phrase. Attempting to evaluate
    an arbitrary string as an OCaml phrase can lead to a variety of
    outcomes, all of which are captured by [t]:

    - [`Eval (Ophr_eval | Ophr_signature)] - The phrase type checks,
    and the OCaml toplevel successfully evaluates it. We provide the
    semantic result as an OCaml [out_phrase], and any output that may
    have been printed to stdout/stderr.

    - [`Eval Ophr_exception] - The phrase type checks, the OCaml
    toplevel evaluates it, but an exception is raised. We provide
    information about the exception as an OCaml [out_phrase], and any
    output that may have been printed to stdout/stderr.

    - [`Uneval invalid_phrase] - The phrase contains a syntax or type
    error, preventing evaluation. We provide compiler constructs
    representing the various possible errors, and also a string with a
    human readable explanation of the error.

    - [`Uneval `Internal_error] - The phrase could not be evaluated
    because the OCaml toploop raised an exception not captured by
    the [invalid_phrase] enumeration.
*)
open Core_kernel.Std

type invalid_phrase = [
| `Lexer of Location.t * Lexer.error
| `Syntaxerr of Syntaxerr.error
| `Typedecl of Location.t * Typedecl.error
| `Typetexp of Location.t * Env.t * Typetexp.error
| `Typecore of Location.t * Env.t * Typecore.error
| `Symtable of Symtable.error
] with sexp

type uneval = [
| invalid_phrase
| `Internal_error of Exn.t
]

type 'a t = [
| `Eval of Outcometree.out_phrase * 'a Oloop_output.t
| `Uneval of uneval * string
]

val deserialize_to_uneval : Oloop_types.serializable_error -> uneval

val location_of_uneval : uneval -> Location.t option
(** [location_of_uneval e] returns the error location if any is present. *)

val report_uneval : ?msg_with_location: bool ->
                   Format.formatter -> uneval -> unit
(** [report_error ppf e] write an error message corresponding to [e]
    to the formatter [ppf] just as the toploop would do it. *)

val uneval_to_error : uneval * string -> Error.t
(** [uneval_to_error(e, msg)] treats [e] as an error, thus making it
    meaningful to convert it to an [Error.t]. Depending on your usage
    of Oloop, this may or may not be correct. Perhaps you are trying
    to demonstrate a syntax error, in which case getting an [uneval]
    is not wrong. *)
