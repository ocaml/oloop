open Core_kernel.Std

(* Same as Oloop_types.error but with SEXP convertion. *)
type invalid_phrase = [
| `Lexer of Oloop_ocaml.Location.t * Oloop_ocaml.lexer_error
| `Syntaxerr of Oloop_ocaml.syntaxerr_error
| `Typedecl of Oloop_ocaml.Location.t * Oloop_ocaml.Typedecl.error
| `Typetexp of Oloop_ocaml.Location.t * Oloop_ocaml.Env.t
               * Oloop_ocaml.Typetexp.error
| `Typecore of Oloop_ocaml.Location.t * Oloop_ocaml.Env.t
               * Oloop_ocaml.Typecore.error
| `Symtable of Oloop_ocaml.Symtable.error
] with sexp

type uneval = [
| invalid_phrase
| `Internal_error of Exn.t
]

type 'a t = [
| `Eval of Outcometree.out_phrase * 'a Oloop_output.t
| `Uneval of uneval * string
]

let env_of_summary = Oloop_ocaml.Env.of_summary

let deserialize_to_uneval : Oloop_types.serializable_error -> uneval =
  function
  | (`Lexer _ | `Syntaxerr _ | `Symtable _ | `Internal_error _) as e -> e
  | `Typedecl(loc, e) ->
     `Typedecl(loc, Oloop_types.deserialize_typedecl_error
                      ~env_of_summary e)
  | `Typetexp(loc, env, e) -> `Typetexp(loc, env_of_summary env, e)
  | `Typecore(loc, env, e) -> `Typecore(loc, env_of_summary env, e)

let location_of_uneval : uneval -> Location.t option = function
  | `Lexer(l, _) | `Typedecl(l, _) | `Typetexp(l, _, _)
  | `Typecore(l, _, _) -> Some l
  | `Syntaxerr _ | `Symtable _ | `Internal_error _ -> None

let report_uneval ?(msg_with_location=false) ppf e =
  (* Do the reverse than the conversion in oloop-top in order to be able
     to use the compiler reporting functions.  The difference is that
     all environments are empty (they cannot be serialized). *)
  let exn = match e with
    | `Lexer(l, e) -> Lexer.Error(e, l)
    | `Syntaxerr e -> Syntaxerr.Error e
    | `Typedecl(l, e) -> Typedecl.Error(l, e)
    | `Typetexp(l, env, e) -> Typetexp.Error(l, env, e)
    | `Typecore(l, env, e) -> Typecore.Error(l, env, e)
    | `Symtable e -> Symtable.Error e
    | `Internal_error e -> e in
  if msg_with_location then
    Errors.report_error ppf exn
  else (
    (* The location of the error is reported because the terminal is
       detected as dumb.  Remove it "manually". *)
    let b = Buffer.create 64 in
    Errors.report_error (Format.formatter_of_buffer b) exn;
    let len = Buffer.length b in
    let loc_present =
      Buffer.(len > 3 && nth b 0 = 'C' && nth b 1 = 'h' && nth b 2 = 'a') in
    let ofs =
      if loc_present then (
        (* Skip the first line. *)
        let ofs = ref 0 in
        while !ofs < len && Buffer.nth b !ofs <> '\n' do incr ofs done;
        !ofs + 1
      )
      else 0 in
    let err = try Buffer.sub b ofs (len - ofs)
              with Invalid_argument _ -> "" in
    Format.pp_print_string ppf err
  )

let uneval_to_error (e, msg) =
  match e with
  | `Internal_error exn ->
     Error.tag (Error.of_exn exn) msg
  | #invalid_phrase as e ->
     let here = match location_of_uneval e with
       | Some loc -> Some loc.Location.loc_start
       | None -> None in
     Error.create ?here msg e sexp_of_invalid_phrase
