(* Toplevel that communicates with the main Oloop module.  This
   program should depend on the minimal amount of libraries so as to
   minimize conflicts when #load'ing modules. *)

open Oloop_types

let initialize_toplevel ~redirect_stderr =
  Sys.interactive := true;
  Toploop.set_paths ();
  Toploop.initialize_toplevel_env();
  Toploop.input_name := "//toplevel//";
  Location.input_name := "//toplevel//";
  Toploop.max_printer_steps := 20;
  (try Topdirs.dir_directory (Sys.getenv "OCAML_TOPLEVEL_PATH")
   with Not_found -> ());
  if redirect_stderr then
    Unix.dup2 Unix.stdout Unix.stderr;
  (* Add #load *)
  let load cma = Topdirs.dir_load Format.str_formatter cma in
  Toploop.(Hashtbl.add directive_table "load" (Directive_string load))


(* This special toplevel will not print the result of the evaluation
   of phrases but store it to transmit it in its original form in a
   special channel. *)
let out_phrase = ref(Outcometree.Ophr_signature []) (* dummy *)
let () =
  let std_print_out_phrase = !Toploop.print_out_phrase in
  Toploop.print_out_phrase
  := fun _fmt phrase ->
     out_phrase := phrase

let eval ~msg_with_location lexbuf =
  try
    Location.init lexbuf "//toplevel//";
    if not msg_with_location then
      Location.input_lexbuf := Some lexbuf;
    let phrase = !Toploop.parse_toplevel_phrase lexbuf in
    ignore(Toploop.execute_phrase true Format.err_formatter phrase);
    ignore(Format.flush_str_formatter ()); (* fill [out_phrase] *)
    Ok !out_phrase
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
       | Lexer.Error(e, l) -> `Lexer(e, l)
       | Syntaxerr.Error e -> `Syntaxerr e
       | Typedecl.Error(l, e) -> `Typedecl(l, e)
       | Typetexp.Error(l, _env, e) -> `Typetexp(l, e)
       | Typecore.Error(l, _env, e) -> `Typecore(l, e)
       (* FIXME: add more *)
       | _ -> `Internal_error e in
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
      with e -> Error(`Internal_error e, "Exception raised during the \
                                         phrase evaluation") in
    Format.pp_print_flush Format.std_formatter ();
    flush stdout;
    Format.pp_print_flush Format.err_formatter ();
    flush stderr;
    (* FIXME: Outcometree.out_value may contain a closure in Oval_printer,
       we must filter that out. *)
    send_out_phrase_or_error ch outcome;
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
      ("--msg-with-location", Arg.Set msg_with_location,
       " Add the source location to error messages");
      ("--redirect-stderr", Arg.Set redirect_stderr,
       " Redirect stderr to stdout");
    ] in
  let specs = Arg.align specs in
  let anon_fun _ = raise(Arg.Bad "No anomymous argument") in
  Arg.parse specs anon_fun "oloop-top [options]";
  main ~msg_with_location:!msg_with_location
       ~redirect_stderr:!redirect_stderr
       ~sock_name:!sock_name