(* Test the quivalent to Utop behavior for Deferred.t values: evaluate
   them automatically. *)

open Core_kernel.Std
open Async.Std

let phrases = [
    "#use \"topfind\"";
    "#thread";
    "#require \"core\"";
    "#require \"async\"";
    "open Core.Std";
    "open Async.Std";

    "Reader.file_contents";
    "let contents = Reader.file_contents \"tests/test.txt\"";
    "Deferred.peek contents";
    "contents";
    "Deferred.peek contents";
  ]


let eval_phrases t =
  Deferred.Or_error.List.iter
    phrases
    ~f:(fun phrase ->
        Format.printf "# [32m%s[0m;;%!" phrase;
        Oloop.eval_or_error t phrase >>|? fun (out_phrase, _) ->
        Format.printf "@\n";
        !Oprint.out_phrase Format.std_formatter out_phrase;
        Format.printf "@?";
       )

let () =
  ignore(Oloop.with_toploop Oloop.Output.separate ~f:eval_phrases
                            ~silent_directives:()
                            ~determine_deferred:()
         >>| function
         | Ok _ -> shutdown 0
         | Error e -> eprintf "%s\n%!" (Error.to_string_hum e);
                     shutdown 1);
  never_returns(Scheduler.go())
