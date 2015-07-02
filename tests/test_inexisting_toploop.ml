open Core.Std
open Async.Std
open Format

let main () =
  let eval_phrase t =
    Oloop.eval_or_error t "let x = 1" >>|? fun e ->
    Oloop.Outcome.print std_formatter (Oloop.Outcome.result e)
  in
  Oloop.with_toploop Oloop.Outcome.separate ~f:eval_phrase
                     ~prog:"non-existing-toploop"
  >>| function
  | Result.Ok() -> shutdown 0
  | Result.Error e ->
     eprintf "Error: %s\n" (Error.to_string_hum e);
     shutdown 1


let () =
  ignore(main());
  never_returns(Scheduler.go())

