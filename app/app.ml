(* Syntax hightlight code and eval ocaml toplevel phrases.
 * Based on http://github.com/ocaml/ocaml.org
 * Modified by Anil Madhavapeddy for Real World OCaml and to use Core *)
open Core.Std
open Async.Std

type phrase = {
  input : string;
  output : string;
  stdout : string;
  stderr : string;
} with sexp

type error =
  { input: string;
    loc: Oloop.Location.t option;
    msg: string;
  } with sexp

let string_of_queue q =
  String.concat ~sep:"" (Queue.to_list q)

let toploop_eval t (phrase: string) =
  Oloop.eval t phrase >>| function
  | Result.Ok (out_phrase, o) ->
     let out_phrase = Oloop.phrase_remove_underscore_names out_phrase in
     let b = Buffer.create 1024 in
     !Oprint.out_phrase (Format.formatter_of_buffer b) out_phrase;
     Result.Ok { input = phrase;
                 output = Buffer.contents b;
                 stdout = Oloop.Output.stdout o;
                 stderr = Oloop.Output.stderr o }
  | Result.Error(e, msg) ->
     Result.Error {input = phrase; loc = Oloop.location_of_error e; msg}

let run ?out_dir ~open_core ~open_async ~inits ~msg_with_location ~pkgs
        filename =
  eprintf "C: %s\n%!" filename;
  let out_dir = match out_dir with
    | Some x -> x
    | None -> Filename.dirname filename
  in
  (* Phrases to eval before any code, in REVERSE order. *)
  let initial_phrases = [ "#use \"topfind\"" ] in
  let initial_phrases =
    if open_core || open_async then
      "open Core.Std" :: "#require \"core.top\""
      :: "#require \"core.syntax\"" :: "#require \"core\""
      :: "#thread" :: "#camlp4o" :: initial_phrases
    else initial_phrases in
  let initial_phrases =
    if open_async then
      "open Async.Std" :: "#require \"async\"" :: initial_phrases
    else initial_phrases in
  let require_pkgs =
    List.map pkgs ~f:(fun p -> sprintf "#require %S" p) in
  let initial_phrases = List.rev_append require_pkgs initial_phrases in
  let use_inits =
    List.map inits ~f:(fun fn -> sprintf "#use %S" fn) in
  let initial_phrases = List.rev_append use_inits initial_phrases in
  let initial_phrases = List.rev initial_phrases in
  let msg_with_location = if msg_with_location then Some() else None in
  Oloop.create Oloop.Output.separate ?msg_with_location >>= function
  | Result.Error e ->
     return(eprintf "Could not create a toploop for %S\nReason: %s\n"
                    filename (Error.to_string_hum e))
  | Result.Ok t ->
     Deferred.List.iter initial_phrases
                        ~f:(fun phrase ->
                            Oloop.eval t phrase >>| function
                            | Result.Ok _ -> ()
                            | Error(_, msg) -> eprintf "ERROR: %s\n%!" msg
                           ) >>= fun () ->
     let parts : (float * string list) list =
       ok_exn (Oloop.Script.of_string ~filename (In_channel.read_all filename))
       |> fun l -> List.map (l : Oloop.Script.t :> Oloop.Script.part list) ~f:(fun x ->
         Oloop.Script.(x.number, phrases_of_string x.content) )
     in
     let eval_part (number, phrases) =
       eprintf "X: %s, Part %g\n%S\n\n%!" filename number
         (String.concat ~sep:"\n" phrases);
       let data = phrases in
       Deferred.List.map data ~f:(toploop_eval t) >>| fun data ->
       let data = <:sexp_of< (phrase, error) Result.t list >> data
                  |> Sexp.to_string in
       let base = Filename.(basename filename |> chop_extension) in
       let out_file = sprintf "%s/%s.%f.txt" out_dir base number in
       Out_channel.write_all out_file ~data
     in
     Deferred.List.iter parts ~f:eval_part


let main =
  Command.basic
    ~summary:"Run files through the OCaml toplevel"
    Command.Spec.(
    empty
    +> flag "-o" (optional string)
            ~doc:"DIR Write files to directory DIR. Default is write to the \
                  same directory that FILE is in."
    +> flag "--pkg" (listed string)
            ~doc:"PKG Load the package PKG before evaluating any code"
    +> flag "--core" no_arg
            ~doc:" Open Core.Std before evaluating any code"
    +> flag "--async" no_arg
            ~doc:" Open Async.Std before evaluating any code"
    +> flag "--init" (listed string)
            ~doc:"FILE Execute FILE before evaluating any code"
    +> flag "--msg-with-location" no_arg
            ~doc:" Print the location in the phrase of errors"
    +> anon (sequence ("file" %: file))
    )
    (fun out_dir pkgs open_core open_async inits msg_with_location files () ->
     ignore(Deferred.List.iter
              files ~f:(run ?out_dir ~open_core ~open_async ~inits
                            ~msg_with_location ~pkgs)
            >>| fun () -> shutdown 0);
     never_returns(Scheduler.go()))

let () = Command.run main ~version:App_conf.version
