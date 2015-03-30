module Code = Oloop_code
open Core_kernel.Std
open Async.Std

let default_toplevel =
  ref(if Caml.Sys.file_exists "./oloop-top.byte" then
        "./oloop-top.byte" (* test *)
      else Filename.concat Oloop_conf.bindir "oloop-top")

type 'a t = {
    proc: Process.t;
    out: string Pipe.Reader.t;
    err: string Pipe.Reader.t;
    sock_path: string; (* unix socket name *)
    sock: Reader.t;    (* socket for the outcome of eval *)
  }

module Output = struct
    type 'a t = { stdout: string Queue.t;  stderr: string Queue.t }
    type 'a kind = bool (* redirect stderr? *)
    type separate
    type merged

    let separate = false
    let merged = true
    let stdout_queue t = t.stdout
    let stderr_queue t = t.stderr

    let string_of_queue q =
      let len = Queue.fold q ~init:0 ~f:(fun l s -> l + String.length s) in
      let buf = Bytes.create len in
      let pos = ref 0 in
      Queue.iter q ~f:(fun s -> let len = String.length s in
                             Bytes.blit s 0 buf !pos len;
                             pos := !pos + len);
      buf

    let stdout t = string_of_queue t.stdout
    let stderr t = string_of_queue t.stderr
  end

let present = function
  | Some () -> true
  | None -> false

let create ?(prog = !default_toplevel) ?(include_dirs=[]) ?init
           ?no_app_functors ?principal ?rectypes ?short_paths
           ?strict_sequence ?msg_with_location ?silent_directives
           ?determine_deferred ?determine_lwt
           output_merged =
  let sock_path = Filename.temp_file "oloop" ".fifo" in
  Unix.unlink sock_path >>= fun () ->
  let sock = Socket.create Socket.Type.unix in
  Socket.bind sock (`Unix sock_path) >>= fun sock ->
  (* Make sure [t.sock_path] is removed when the program exits: *)
  at_exit(fun () -> try Core.Std.Unix.unlink sock_path with _ -> ());
  let args = ["--sock"; sock_path] in
  let args = List.fold include_dirs ~init:args
                       ~f:(fun args dir -> "-I" :: dir :: args) in
  let args = match init with Some fn -> "--init" :: fn :: args
                           | None -> args in
  let args = if present no_app_functors then "--no-app-funct" :: args
             else args in
  let args = if present principal then "--principal" :: args else args in
  let args = if present rectypes then "--rectypes" :: args else args in
  let args = if present short_paths then "--short-paths" :: args else args in
  let args = if present strict_sequence then "--strict-sequence" :: args
             else args in
  let args = if present msg_with_location then "--msg-with-location" :: args
             else args in
  let args = if present silent_directives then "--silent-directives" :: args
             else args in
  let args = if present determine_deferred then "--determine-deferred" :: args
             else args in
  let args = if present determine_lwt then "--determine-lwt" :: args
             else args in
  let args = if output_merged then "--redirect-stderr" :: args else args in
  Process.create ~prog ~args () >>=? fun proc ->
  (* Wait for the oloop-top client to connect: *)
  Socket.accept (Socket.listen sock) >>= function
  | `Ok(sock, _) ->
     let sock = Reader.create (Socket.fd sock) in
     let out = Reader.pipe (Process.stdout proc)
     and err = Reader.pipe (Process.stderr proc) in
     return(Result.Ok { proc;  out; err;  sock_path;  sock })
  | `Socket_closed ->
     let msg = "Oloop.create: toplevel not started" in
     return(Result.Error(Error.of_string msg))


let close t =
  let top = Process.stdin t.proc in
  Writer.send top "exit 0;;"; (* exit the toploop *)
  Writer.close top >>= fun () ->
  Reader.close (Process.stdout t.proc) >>= fun () ->
  Reader.close (Process.stderr t.proc) >>= fun () ->
  Unix.unlink t.sock_path


let with_toploop ?prog ?include_dirs ?init ?no_app_functors ?principal
                 ?rectypes ?short_paths ?strict_sequence ?msg_with_location
                 ?silent_directives ?determine_deferred ?determine_lwt
                 output_merged ~f =
  create ?prog ?include_dirs ?init ?no_app_functors ?principal
         ?rectypes ?short_paths ?strict_sequence ?msg_with_location
         ?silent_directives ?determine_deferred ?determine_lwt
         output_merged
  >>= function
  | Result.Ok t -> f t
                  >>= fun r -> close t
                  >>= fun () -> return r
  | Result.Error _ as e -> return e


type eval_error =
  [ `Lexer of Oloop_ocaml.lexer_error * Oloop_ocaml.Location.t
  | `Syntaxerr of Oloop_ocaml.syntaxerr_error
  | `Typedecl of Oloop_ocaml.Location.t * Oloop_ocaml.Typedecl.error
  | `Typetexp of Oloop_ocaml.Location.t * Oloop_ocaml.Typetexp.error
  | `Typecore of Oloop_ocaml.Location.t * Oloop_ocaml.Typecore.error
  | `Symtable of Oloop_ocaml.Symtable.error
  ] with sexp

type error =
  [ eval_error
  | `Internal_error of Exn.t ]

let location_of_error : error -> Location.t option = function
  | `Lexer(_, l) | `Typedecl(l, _) | `Typetexp(l, _)
  | `Typecore(l, _) -> Some l
  | `Syntaxerr _ | `Symtable _ | `Internal_error _ -> None

let queue_of_pipe p =
  match Pipe.read_now' p with
  | `Eof | `Nothing_available -> Queue.create()
  | `Ok q -> q

let eval t phrase =
  let top = Process.stdin t.proc in
  Writer.write top (Int.to_string (String.length phrase) ^ "\n");
  Writer.write top phrase;
  Writer.flushed top
  (* FIXME: Maybe the output of the previous phrase was not yet
     collected.  Must use a queue to serialize phrase → outcome *)
  >>= fun () ->
  Reader.read_marshal t.sock
  >>= fun (out_phrase: Oloop_types.out_phrase_or_error Reader.Read_result.t) ->
  let o = { Output.stdout = queue_of_pipe t.out;
            Output.stderr = queue_of_pipe t.err } in
  match out_phrase with
  | `Ok(Oloop_types.Ok r) ->
     return(Result.Ok(Oloop_types.to_outcometree_phrase r, o))
  | `Ok(Oloop_types.Error e) ->
     (* When the code was not correclty evaluated, the [phrase] is
        outputted on stdout with terminal codes to underline the error
        location.  Since we have access to the location, this is useless. *)
     return(Result.Error(e: error * string))
  | `Eof ->
     return(Result.Error(`Internal_error End_of_file,
                         "The toploop did not return a result"))


let to_error (e, msg) =
  match e with
  | `Internal_error exn ->
     Error.tag (Error.of_exn exn) msg
  | #eval_error as e ->
     let here = match location_of_error e with
       | Some loc -> Some loc.Location.loc_start
       | None -> None in
     Error.create ?here msg e sexp_of_eval_error

let eval_or_error t phrase =
  eval t phrase >>| function
  | Result.Ok _ as r -> r
  | Result.Error err -> Result.Error(to_error err)

(*
 * Helper functions and modules
 *)

let signatures_remove_underscore_names =
  Oloop_ocaml.signatures_remove_underscore_names
let phrase_remove_underscore_names =
  Oloop_ocaml.phrase_remove_underscore_names


module Location = struct
    include Location

    let sexp_of_t = Oloop_ocaml.Location.sexp_of_t
    let t_of_sexp = Oloop_ocaml.Location.t_of_sexp
  end
;;
