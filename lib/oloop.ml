open Core_kernel.Std
open Async.Std

module Script = Oloop_script
module Output = Oloop_output
module Outcome = Oloop_outcome

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
  let args = match Output.kind output_merged with
    | `Merged -> "--redirect-stderr" :: args
    | `Separate -> args in
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
  Unix.wait (`Pid(Process.pid t.proc)) >>= fun _ ->
  Reader.close t.sock >>= fun () ->
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

let queue_of_pipe p =
  match Pipe.read_now' p with
  | `Eof | `Nothing_available -> Queue.create()
  | `Ok q -> q

let eval (t: 'a t) phrase =
  let top = Process.stdin t.proc in
  Writer.write top (Int.to_string (String.length phrase) ^ "\n");
  Writer.write top phrase;
  Writer.flushed top
  (* FIXME: Maybe the output of the previous phrase was not yet
     collected.  Must use a queue to serialize phrase → outcome *)
  >>= fun () ->
  Reader.read_marshal t.sock
  >>= fun (out_phrase: Oloop_types.out_phrase_or_error Reader.Read_result.t) ->
  let o = Output.make_unsafe
            ~stdout:(queue_of_pipe t.out)
            ~stderr:(queue_of_pipe t.err)
  in
  match out_phrase with
  | `Ok(Oloop_types.Ok r) ->
     return(`Eval(Oloop_types.to_outcometree_phrase r, o))
  | `Ok(Oloop_types.Error(e, msg)) ->
     (* When the code was not correclty evaluated, the [phrase] is
        outputted on stdout with terminal codes to underline the error
        location.  Since we have access to the location, this is useless. *)
     return(`Uneval(Outcome.deserialize_to_uneval e, msg))
  | `Eof ->
     return(`Uneval(`Internal_error End_of_file,
                         "The toploop did not return a result"))

let eval_or_error t phrase =
  eval t phrase >>| function
  | `Eval x -> Ok x
  | `Uneval err -> Error(Outcome.uneval_to_error err)


(******************************************************************************)
(* Miscellaneous                                                              *)
(******************************************************************************)
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
