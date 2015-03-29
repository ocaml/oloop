(* Syntax hightlight code and eval ocaml toplevel phrases.
 * Based on http://github.com/ocaml/ocaml.org
 * Modified by Anil Madhavapeddy for Real World OCaml and to use Core *)
open Core.Std
open Async.Std
module Code = Oloop_code

(** Run these phrases silently before any code *)
let initial_phrases = [
  "#use \"topfind\"";
  "#camlp4o";
  "#thread";
  "#require \"core\"";
  "#require \"core.syntax\"";
  "#require \"core.top\""
]

type phrase = {
  input : string;
  output : string;
  stdout : string;
  stderr : string;
} with sexp

let string_of_queue q =
  String.concat ~sep:"" (Queue.to_list q)

let toploop_eval t (phrase: string) =
  Oloop.eval t phrase >>| function
  | Result.Ok (out_phrase, o) ->
     let out_phrase = Oloop.phrase_remove_underscore_names out_phrase in
     let b = Buffer.create 1024 in
     !Oprint.out_phrase (Format.formatter_of_buffer b) out_phrase;
     { input = phrase;
       output = Buffer.contents b;
       stdout = Oloop.Output.stdout o;
       stderr = Oloop.Output.stderr o }
  | Result.Error(_, msg) ->
     { input = phrase;
       output = msg;
       stdout = "";  stderr = "" }

let run ?out_dir ?open_core ?open_async filename =
  eprintf "C: %s\n%!" filename;
  let out_dir = match out_dir with
    | Some x -> x
    | None -> Filename.dirname filename
  in
  let initial_phrases = match open_core with
    | Some x ->
       if x then initial_phrases @ ["open Core.Std"]
       else initial_phrases
    | None -> initial_phrases
  in
  let initial_phrases = match open_async with
    | Some x ->
       if x then initial_phrases @ ["#require \"async\"";"open Async.Std"]
       else initial_phrases
    | None -> initial_phrases
  in
  Oloop.create Oloop.Output.separate >>= function
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
  let parts =
    Code.split_parts_exn ~filename (In_channel.read_all filename) in
  let eval_part (part, content) =
    eprintf "X: %s, part %f\n%S\n\n%!" filename part content;
    let data = ok_exn (Code.split_toplevel_phrases `Anywhere content) in
    Deferred.List.map data ~f:(toploop_eval t) >>| fun data ->
    let data = <:sexp_of< phrase list >> data
               |> Sexp.to_string in
    let base = Filename.(basename filename |> chop_extension) in
    let out_file = sprintf "%s/%s.%f.txt" out_dir base part in
    Out_channel.write_all out_file ~data
  in
  Deferred.List.iter parts ~f:eval_part


let main = Command.basic
  ~summary:"Run files through the Core toplevel"
  Command.Spec.(
    empty
    +> flag "-o" (optional string)
      ~doc:"DIR Write files to directory DIR. Default is write to the \
            same directory that FILE is in."
    +> flag "-c" (optional bool) ~doc:"CORE Do open Core.Std before \
                                      evaluating any code"
    +> flag "-a" (optional bool) ~doc:"ASYNC Do open Async.Std before \
                                      evaluating any code"
    +> anon (sequence ("file" %: file))
  )
  (fun out_dir open_core open_async files () ->
   ignore(Deferred.List.iter files ~f:(run ?out_dir ?open_core ?open_async)
          >>| fun () -> shutdown 0);
   never_returns(Scheduler.go()))

let () = Command.run main
