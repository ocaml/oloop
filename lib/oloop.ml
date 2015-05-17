open Core_kernel.Std
open Async.Std

module Script = Oloop_script
module Outcome = Oloop_outcome

let default_toplevel =
  ref(if Caml.Sys.file_exists "./oloop-top.byte" then
        "./oloop-top.byte" (* test *)
      else Filename.concat Oloop_conf.bindir "oloop-top")

type 'a t = {
    proc: Process.t;
    sock_path: string; (* unix socket name *)
    sock: Reader.t;    (* socket for the outcome of eval *)
    silent_directives: bool;
    kind: 'a Outcome.kind;
  }

type 'a ocaml_args =
  ?include_dirs: string list ->
  ?init: string ->
  ?noinit: unit ->
  ?no_app_functors: unit ->
  ?principal: unit ->
  ?rectypes: unit ->
  ?short_paths: unit ->
  ?strict_sequence: unit ->
  ?thread: unit ->
  'a

type 'a args = (
  ?prog: string ->
  ?msg_with_location: unit ->
  ?silent_directives: unit ->
  ?determine_deferred: unit ->
  ?determine_lwt: unit ->
  'a
) ocaml_args

let present = function
  | Some () -> true
  | None -> false

let check_toploop_exists prog =
  (* Process.create hangs if prog does not exists.  We want to
     immediately report an error. *)
  Unix.access prog [`Exec] >>| function
  | Result.Ok() as r -> r
  | Result.Error e0 ->
     let e1 = Invalid_argument(sprintf "Toploop %S not executable" prog) in
     Result.Error(Error.of_list [Error.of_exn e1; Error.of_exn e0])

let create ?(include_dirs=[]) ?init ?noinit ?no_app_functors ?principal
           ?rectypes ?short_paths ?strict_sequence ?thread
           ?(prog = !default_toplevel) ?msg_with_location
           ?silent_directives ?determine_deferred ?determine_lwt
           output_merged =
  check_toploop_exists prog >>=? fun () ->
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
  let args = if present noinit then "--noinit" :: args
             else args in
  let args = if present no_app_functors then "--no-app-funct" :: args
             else args in
  let args = if present principal then "--principal" :: args else args in
  let args = if present rectypes then "--rectypes" :: args else args in
  let args = if present short_paths then "--short-paths" :: args else args in
  let args = if present strict_sequence then "--strict-sequence" :: args
             else args in
  let args = if present thread then "--thread" :: args
             else args in
  let args = if present msg_with_location then "--msg-with-location" :: args
             else args in
  let args = if present determine_deferred then "--determine-deferred" :: args
             else args in
  let args = if present determine_lwt then "--determine-lwt" :: args
             else args in
  let args = match Outcome.kind output_merged with
    | `Merged -> "--redirect-stderr" :: args
    | `Separate -> args in
  (* Make sure that the input phrase is not reprinted: *)
  let env = ["TERM", "norepeat"] in
  Process.create ~prog ~args ~env:(`Extend env) () >>=? fun proc ->
  (* Wait for the oloop-top client to connect: *)
  Socket.accept (Socket.listen sock) >>= function
  | `Ok(conn_sock, _) ->
     Unix.close (Socket.fd sock) >>= fun () ->
     let sock = Reader.create (Socket.fd conn_sock) in
     return(Result.Ok { proc;  sock_path;  sock;
                        silent_directives = present silent_directives;
                        kind = output_merged })
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

let with_toploop ?include_dirs ?init ?noinit ?no_app_functors ?principal
                 ?rectypes ?short_paths ?strict_sequence ?thread
                 ?prog ?msg_with_location
                 ?silent_directives ?determine_deferred ?determine_lwt
                 output_merged ~f =
  create ?prog ?include_dirs ?init ?noinit ?no_app_functors ?principal
         ?rectypes ?short_paths ?strict_sequence ?thread ?msg_with_location
         ?silent_directives ?determine_deferred ?determine_lwt
         output_merged
  >>= function
  | Result.Ok t -> (try f t
                    with e -> close t >>= fun () -> raise e)
                   >>= fun r -> close t
                   >>= fun () -> return r
  | Result.Error _ as e -> return e


let reader_to_string r =
  Reader.read_until r (`Char Oloop_types.end_output) ~keep_delim:false
  >>| function
  | `Eof -> ""
  | `Eof_without_delim s -> s
  | `Ok s -> s

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
  reader_to_string (Process.stdout t.proc) >>= fun stdout ->
  reader_to_string (Process.stderr t.proc) >>= fun stderr ->
  match out_phrase with
  | `Ok(Oloop_types.Ok(r, is_directive, warnings)) ->
     (* TODO: Parse stderr for warnings *)
     let out_phrase = Oloop_types.to_outcometree_phrase r in
     let result, stdout, stderr =
       if is_directive && t.silent_directives then
         (* Only silence the output if everything is fine. *)
         match out_phrase with
         | Outcometree.Ophr_exception _ -> (out_phrase, stdout, stderr)
         | Outcometree.Ophr_eval _
         | Outcometree.Ophr_signature _ ->
            (Outcometree.Ophr_signature [], "", "")
       else (out_phrase, stdout, stderr) in
     return(`Eval(Outcome.make_eval ~result ~stdout ~stderr ~warnings t.kind))
  | `Ok(Oloop_types.Error(e, msg)) ->
     (* When the code was not correclty evaluated, the [phrase] is
        outputted on stdout with terminal codes to underline the error
        location.  Since we have access to the location, this is useless. *)
     return(`Uneval(Outcome.deserialize_to_uneval e, msg))
  | `Eof ->
     return(`Uneval(`Internal_error End_of_file,
                         "The toploop did not return a result"))

let eval_script ?include_dirs ?init ?noinit ?no_app_functors ?principal
                ?rectypes ?short_paths ?strict_sequence ?thread
                ?prog ?msg_with_location
                ?silent_directives ?determine_deferred ?determine_lwt
                script =
  let eval_phrase oloop phrase : Script.Evaluated.phrase Deferred.t =
    eval oloop phrase >>| fun outcome ->
    {Script.Evaluated.phrase; outcome}
  in
  let eval_phrases oloop phrases : Script.Evaluated.phrase list Deferred.t =
    Deferred.List.fold phrases ~init:[] ~f:(fun accum phrase ->
      eval_phrase oloop phrase >>| fun x -> x::accum
    )
    >>| List.rev
  in
  let eval_part oloop part : Script.Evaluated.part Deferred.t =
    let {Script.number; content} = part in
    let phrases = Script.phrases_of_string content in
    eval_phrases oloop phrases >>| fun phrases ->
    {Script.Evaluated.number; content; phrases}
  in
  let parts = (script : Script.t :> Script.part list) in
  with_toploop ?include_dirs ?init ?noinit ?no_app_functors ?principal
               ?rectypes ?short_paths ?strict_sequence ?thread
               ?prog ?msg_with_location
               ?silent_directives ?determine_deferred ?determine_lwt
               Outcome.merged ~f:(fun t ->
    Deferred.List.fold parts ~init:[] ~f:(fun accum part ->
      eval_part t part >>| fun x -> x::accum
    )
    >>| List.rev
    >>| fun x -> Ok x
  )

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
