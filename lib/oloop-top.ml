(* Toplevel that communicates with the main Oloop module.  This
   program should depend on the minimal amount of libraries so as to
   minimize conflicts when #load'ing modules. *)

open Oloop_types
module Rule = Oloop_rule

(* Force the linking of some modules for, say, #load "camlp4o.cma" *)
module S___ = Stream
module Q___ = Queue
module C___ = CamlinternalOO
module D___ = Dynlink
module St__ = StdLabels
module Cp__ = Complex
module Sc__ = Scanf
module Gc__ = Gc
module ML__ = MoreLabels
module CiM_ = CamlinternalMod
module Gl__ = Genlex
module Oo__ = Oo
module So__ = Sort (* Deprecated but used by ocamlfind anyway *)
module Ll__ = ListLabels
module Sl__ = StringLabels
module Al__ = ArrayLabels

let add_dir_from_env env =
  try Topdirs.dir_directory (Sys.getenv env)
  with Not_found -> ()

let initialize_toplevel ~redirect_stderr =
  Sys.interactive := true;
  Toploop.set_paths ();
  Toploop.initialize_toplevel_env();
  Toploop.input_name := "//toplevel//";
  Location.input_name := "//toplevel//";
  Toploop.max_printer_steps := 20;
  add_dir_from_env "OCAML_TOPLEVEL_PATH";
  add_dir_from_env "CAML_LD_LIBRARY_PATH";
  if redirect_stderr then
    Unix.dup2 Unix.stdout Unix.stderr;
  (* Add #load *)
  let load cma = Topdirs.dir_load Format.str_formatter cma in
  Toploop.(Hashtbl.add directive_table "load" (Directive_string load))


(* This special toplevel will not print the result of the evaluation
   of phrases but store it to transmit it in its original form in a
   special channel. *)
let out_phrase = ref Oloop_types.empty (* dummy *)
let () =
  Toploop.print_out_phrase
  := fun _fmt phrase ->
     out_phrase := Oloop_types.of_outcometree_phrase phrase

let rec is_prefix_loop pre s i len_pre =
  i >= len_pre || (pre.[i] = s.[i] && is_prefix_loop pre s (i + 1) len_pre)

let is_prefix pre s =
  if String.length pre > String.length s then false
  else is_prefix_loop pre s 0 (String.length pre)

let remove_location s =
  if is_prefix "Characters " s then (
    let i = String.index s '\n' + 1 in
    String.sub s i (String.length s - i)
  )
  else s

let eval ~msg_with_location lexbuf =
  try
    Location.init lexbuf "//toplevel//";
    let phrase = !Toploop.parse_toplevel_phrase lexbuf in
    let phrase = Rule.rewrite phrase in
    Env.reset_cache_toplevel ();
    ignore(Toploop.execute_phrase true Format.str_formatter phrase);
    ignore(Format.flush_str_formatter ()); (* fill [out_phrase] *)
    let warnings = [] in (* for future compilers *)
    let is_directive = match phrase with
      | Parsetree.Ptop_def _ -> false
      | Parsetree.Ptop_dir _ -> true in
    Ok(!out_phrase, is_directive, warnings)
  with
  | End_of_file -> exit 0
  | e ->
     let backtrace_enabled = Printexc.backtrace_status () in
     if not backtrace_enabled then Printexc.record_backtrace true;
     let msg = try Errors.report_error Format.str_formatter e;
                   Format.flush_str_formatter ()
               with _ -> "" in
     if not backtrace_enabled then Printexc.record_backtrace false;
     let err = match e with
       | Lexer.Error(e, l) -> `Lexer(l, e)
       | Syntaxerr.Error e -> `Syntaxerr e
       | Typedecl.Error(l, e) ->
          `Typedecl(l, Oloop_types.serialize_typedecl_error e)
       | Typetexp.Error(l, env, e) -> `Typetexp(l, Env.summary env, e)
       | Typecore.Error(l, env, e) -> `Typecore(l, Env.summary env, e)
       | Typeclass.Error(l, env, e) ->
          `Typeclass(l, Env.summary env,
                     Oloop_types.serialize_typeclass_error e)
       | Symtable.Error e -> `Symtable e
       (* FIXME: add more *)
       | _ -> `Internal_error(Printexc.to_string e) in
     let msg = if msg_with_location then msg else remove_location msg in
     Error(err, msg)


let read_phrase_exn ch =
  let len = int_of_string(input_line ch) in
  let phrase = Bytes.create len in
  really_input ch phrase 0 len;
  phrase

let main ~msg_with_location ~redirect_stderr ~sock_name =
  initialize_toplevel ~redirect_stderr;
  let ch = (* or exn *)
    let fd = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
    Unix.connect fd (Unix.ADDR_UNIX sock_name);
    Unix.out_channel_of_descr fd in
  while true do
    Location.reset();
    let outcome =
      try let phrase = read_phrase_exn stdin in
          eval ~msg_with_location (Lexing.from_string(phrase ^ ";;"))
      with e -> Error(`Internal_error(Printexc.to_string e),
                      "Exception raised during the phrase evaluation") in
    (* Sending a special ASCII char to indicate the end of the output
       is not 100% robust but the alternative consisting of reading as
       much as is available does not work well. *)
    if redirect_stderr then (
      (* If stderr is redirected to stdout, we do not want to send the
         separating char twice on stdout.  Flush both formatters first
         to ensure that [end_output] is at the end. *)
      Format.pp_print_flush Format.std_formatter ();
      Format.pp_print_flush Format.err_formatter ();
      Format.pp_print_char Format.std_formatter end_output;
      Format.pp_print_flush Format.std_formatter ();
    )
    else (
      Format.pp_print_char Format.std_formatter end_output;
      Format.pp_print_flush Format.std_formatter ();
      Format.pp_print_char Format.err_formatter end_output;
      Format.pp_print_flush Format.err_formatter ();
    );
    (try send_out_phrase_or_error ch outcome
     with Invalid_argument msg as e ->
       (* The marshalling failed *)
       let msg = "oloop-top: sending the phrase eval outcome failed \
                  because: " ^ msg in
       let err = Error(`Internal_error(Printexc.to_string e), msg) in
       send_out_phrase_or_error ch err);
  done

let () =
  let sock_name = ref "" in
  let msg_with_location = ref false in
  let redirect_stderr = ref false in
  (* These options must correspond to optional arguments in [Oloop.create]. *)
  let specs = [
      ("--sock", Arg.Set_string sock_name,
       "<name> the name of the Unix socket on which to send eval outcome");
      ("-I", Arg.String(fun p -> Clflags.include_dirs
                              := p :: !Clflags.include_dirs),
       "<dir> Add <dir> to the list of include directories");
      ("--init", Arg.String(fun f -> Clflags.init_file := Some f),
       "<file> Load <file> instead of default init file");
      ("--no-app-funct", Arg.Clear Clflags.applicative_functors,
       " Deactivate applicative functors");
      ("--principal", Arg.Set Clflags.principal,
       " Check principality of type inference");
      ("--rectypes", Arg.Set Clflags.recursive_types,
       " Allow arbitrary recursive types");
      ("--short-paths", Arg.Clear Clflags.real_paths,
       " Shorten paths in types");
      ("--strict-sequence", Arg.Set Clflags.strict_sequence,
       " Left-hand part of a sequence must have type unit");
      ("--thread", Arg.Set Clflags.use_threads,
       " Generate code that supports the system threads library");
      ("--msg-with-location", Arg.Set msg_with_location,
       " Add the source location to error messages");
      ("--redirect-stderr", Arg.Set redirect_stderr,
       " Redirect stderr to stdout");
      ("--determine-deferred", Arg.Unit(fun () -> Rule.(enable async)),
       " Determine anonymous Deferred.t values (as Utop does)");
      ("--determine-lwt", Arg.Unit(fun () -> Rule.(enable lwt)),
       " Determine anonymous Lwt.t values (as Utop does)");
    ] in
  let specs = Arg.align specs in
  let anon_fun _ = raise(Arg.Bad "No anomymous argument") in
  Arg.parse specs anon_fun "oloop-top [options]";
  main ~msg_with_location:!msg_with_location
       ~redirect_stderr:!redirect_stderr
       ~sock_name:!sock_name
