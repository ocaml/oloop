(** Raw scripts are not meant to be evaluated. They consist of a
    sequence of alternating input phrases and the corresponding
    output. This is useful to represent content in some customized way
    that auto-evaluation won't work for. For example:

    {v
    # module Make(String : module type of String) = struct ... end;;
    module Make :
      functor
        (String : ...) ->
        sig ... end
    v}

    We have ellided code in the input phrase and output. This could be
    useful to include in educational material, where the accompanying
    text mentions that some details are ellided. However, there would
    be no systematic way to send the input to OCaml, nor to express
    which part of the output should be ellided. Writing out the full
    code manually is the best option.

    All we provide here is a simple parser for code in the above
    format. A '#' character indicates the start of an input phrase and
    a double semicolon indicates its end. Text following a double
    semicolon is the output, up until the next '#' character or the
    end of input. Unlike [Outcome]s, the output is not split into
    OCaml's out phrase vs content printed by evaluating the input.

    The parser is not too smart, e.g. a double semicolon should be the
    last item on a line or you will get unexpected results. The idea
    is to support content that is copy/paste'd from toplevel output
    and then modified manually.

    Unlike the [Scripts] module, there is no notion of [parts],
    although that wouldn't be difficult to add.
*)
open Core.Std
open Async.Std

type item = {
  input : string;
  output : string;
}

type t = item list

val of_file : string -> t Or_error.t Deferred.t
(** Parse a file. *)

val of_string : filename:string -> string -> t Or_error.t
(** Parse a string. The [filename] is only for error messages. *)
