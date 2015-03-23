(* Types share between the Oloop module and the oloop-top executable
   to ensure typed communication. *)

type error =
  [ `Lexer of Lexer.error * Location.t
  | `Syntaxerr of Syntaxerr.error
  | `Typedecl of Location.t * Typedecl.error
  | `Typetexp of Location.t * Typetexp.error
  | `Typecore of Location.t * Typecore.error
  | `Symtable of Symtable.error
  | `Internal_error of exn ]

(** Outcome of evaluating toplevel phrases.
    - [OK] means that the phrase was correctly evaluated (if the
      phrase raises an exception, it is reported this way too).
    - [Error (e, s)] means that the phrases were not evaluated
      correctly because of a syntax error [e = `Syntaxerr _], a
      type error [e = `Typecore _], an unbound module [e =
      `Typetexp _], etc.
      The string is the explanation the toplevel would display
      (or an explanation of the error in case of [`Internal_error]). *)
type out_phrase_or_error =
  | Ok of Outcometree.out_phrase
  | Error of (error * string)

let send_out_phrase_or_error ch (o: out_phrase_or_error) =
  output_value ch o;
  flush ch
