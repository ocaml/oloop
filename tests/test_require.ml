open Core.Std
open Async.Std
open Format

let eval t phrase =
  printf "# [32m%s[0m;;@\n%!" phrase;
  Oloop.eval_or_error t phrase >>=? fun e ->
  !Oprint.out_phrase std_formatter (Oloop.Outcome.result e);
  if Oloop.Outcome.stdout e <> "" then
    printf "OUT: %s\n" (Oloop.Outcome.stdout e);
  if Oloop.Outcome.stderr e <> "" then
    printf "ERR: %s\n" (Oloop.Outcome.stderr e);
  return(Result.Ok())

let main () =
  let eval_phrases t =
    eval t "#use \"topfind\"" >>=? fun () ->
    eval t "#thread" >>=? fun () ->
    eval t "#require \"lacaml\"" >>=? fun () ->
    eval t "open Lacaml.D\n  \
            let x = Vec.make0 3"
  in
  Oloop.with_toploop Oloop.Outcome.separate ~f:eval_phrases
                     (* ~silent_directives:() *)

let () =
  ignore(main() >>| function
         | Result.Ok () -> shutdown 0
         | Result.Error e -> eprintf "%s\n%!" (Error.to_string_hum e);
                            shutdown 1);
  never_returns(Scheduler.go())
