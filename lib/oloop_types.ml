(* Types shared between the Oloop module and the oloop-top executable
   to ensure typed communication. *)

type pp_directive =
  | String of string
  | Newline
  | Spaces of int

let recorder l =
  let out_string s ofs len = l := String(String.sub s ofs len) :: !l in
  let out_flush () = () in
  let out_newline () = l := Newline :: !l in
  let out_spaces n = l := Spaces n :: !l in
  let fmt = Format.make_formatter out_string out_flush in
  Format.pp_set_formatter_out_functions
    fmt { Format.out_string;  out_flush;  out_newline;  out_spaces };
  fmt

let record (pp: Format.formatter -> unit) =
  let l = ref [] in
  let fmt = recorder l in
  pp fmt;
  Format.pp_print_flush fmt ();
  List.rev !l

let play_directive fmt = function
  | String s -> Format.pp_print_string fmt s
  | Newline -> Format.pp_force_newline fmt ()
  | Spaces n -> for _i = 1 to n do Format.pp_print_space fmt () done

let replay l fmt =
  Format.pp_open_hvbox fmt 0;
  List.iter (play_directive fmt) l;
  Format.pp_close_box fmt ()

(* Outcometree.out_phrase contains a closure (which cannot be
   marshalled) to handle arbitrary pretty printers.  Play it on a
   recording formatter to keep most of it. *)

type serializable_out_value =
  | Array of serializable_out_value list
  | Char of char
  | Constr of Outcometree.out_ident * serializable_out_value list
  | Ellipsis
  | Float of float
  | Int of int
  | Int32 of int32
  | Int64 of int64
  | Nativeint of nativeint
  | List of serializable_out_value list
  | Printer of pp_directive list
  | Record of (Outcometree.out_ident * serializable_out_value) list
  | String of string
  | Stuff of string
  | Tuple of serializable_out_value list
  | Variant of string * serializable_out_value option

type serializable_out_phrase =
  | Eval of serializable_out_value * Outcometree.out_type
  | Signature of (Outcometree.out_sig_item * serializable_out_value option) list
  | Exception of (exn * serializable_out_value)

let empty = Signature []

let rec of_outcometree_value =
  let open Outcometree in
  function
  | Oval_array l -> Array(List.map of_outcometree_value l)
  | Oval_char c -> Char c
  | Oval_constr(oi, ov) -> Constr(oi, List.map of_outcometree_value ov)
  | Oval_ellipsis -> Ellipsis
  | Oval_float f -> Float f
  | Oval_int i -> Int i
  | Oval_int32 i -> Int32 i
  | Oval_int64 i -> Int64 i
  | Oval_nativeint i -> Nativeint i
  | Oval_list l -> List(List.map of_outcometree_value l)
  | Oval_printer p -> Printer(record p)
  | Oval_record l -> Record(List.map of_outcometree_record l)
  | Oval_string s -> String s
  | Oval_stuff s -> Stuff s
  | Oval_tuple ov -> Tuple(List.map of_outcometree_value ov)
  | Oval_variant(s, None) -> Variant(s, None)
  | Oval_variant(s, Some ov) -> Variant(s, Some(of_outcometree_value ov))
and of_outcometree_record (oi, ov) =
  (oi, of_outcometree_value ov)

let of_outcometree_sig (si, maybe_ov) =
  match maybe_ov with
  | None -> (si, None)
  | Some ov -> (si, Some(of_outcometree_value ov))

let of_outcometree_phrase =
  let open Outcometree in
  function
  | Ophr_eval(ov, ot) -> Eval(of_outcometree_value ov, ot)
  | Ophr_signature l -> Signature(List.map of_outcometree_sig l)
  | Ophr_exception(e, ov) -> Exception(e, of_outcometree_value ov)


let rec to_outcometree_value =
  let open Outcometree in
  function
  | Array l -> Oval_array(List.map to_outcometree_value l)
  | Char c -> Oval_char c
  | Constr(oi, ov) -> Oval_constr(oi, List.map to_outcometree_value ov)
  | Ellipsis -> Oval_ellipsis
  | Float f -> Oval_float f
  | Int i -> Oval_int i
  | Int32 i -> Oval_int32 i
  | Int64 i -> Oval_int64 i
  | Nativeint i -> Oval_nativeint i
  | List l -> Oval_list(List.map to_outcometree_value l)
  | Printer p -> Oval_printer(replay p)
  | Record l -> Oval_record(List.map to_outcometree_record l)
  | String s -> Oval_string s
  | Stuff s -> Oval_stuff s
  | Tuple ov -> Oval_tuple(List.map to_outcometree_value ov)
  | Variant(s, None) -> Oval_variant(s, None)
  | Variant(s, Some ov) -> Oval_variant(s, Some(to_outcometree_value ov))
and to_outcometree_record (oi, ov) =
  (oi, to_outcometree_value ov)

let to_outcometree_sig (si, maybe_ov) =
  match maybe_ov with
  | None -> (si, None)
  | Some ov -> (si, Some(to_outcometree_value ov))

let to_outcometree_phrase =
  let open Outcometree in
  function
  | Eval(ov, ot) -> Ophr_eval(to_outcometree_value ov, ot)
  | Signature l -> Ophr_signature(List.map to_outcometree_sig l)
  | Exception(e, ov) -> Ophr_exception(e, to_outcometree_value ov)

(** Enumeration of errors. *)
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
  | Ok of serializable_out_phrase
  | Error of (error * string)

let send_out_phrase_or_error ch (o: out_phrase_or_error) =
  output_value ch o;
  flush ch
