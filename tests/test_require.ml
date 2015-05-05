open Core.Std
open Async.Std
open Format


let main () =
  let eval_phrases t =
    Oloop.eval_or_error t "#use \"topfind\"" >>=? fun e ->
    !Oprint.out_phrase std_formatter (Oloop.Outcome.result e);
    Oloop.eval_or_error t "#require \"lacaml\"" >>=? fun e ->
    !Oprint.out_phrase std_formatter (Oloop.Outcome.result e);

    let phrase = "open Lacaml.D
                  let x = Vec.make0 3" in
    Oloop.eval_or_error t phrase >>=? fun e ->
    !Oprint.out_phrase std_formatter (Oloop.Outcome.result e);
    printf "OUTPUT: %s\n" (Oloop.Outcome.stdout e);
    return(Result.Ok())
  in
  Oloop.with_toploop Oloop.Outcome.separate ~f:eval_phrases
                     ~silent_directives:()

let () =
  ignore(main() >>| function
         | Result.Ok () -> shutdown 0
         | Result.Error e -> eprintf "%s\n%!" (Error.to_string_hum e);
                            shutdown 1);
  never_returns(Scheduler.go())
