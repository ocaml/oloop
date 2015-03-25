open Core.Std
open Async.Std
open Format


let main () =
  Oloop.create Oloop.Output.separate >>=? fun t ->
  Oloop.eval_or_error t "#use \"topfind\"" >>=? fun _ ->
  Oloop.eval_or_error t "#require \"lacaml\"" >>=? fun _ ->
  let phrase = "open Lacaml.D
                let x = Vec.make0 3" in
  Oloop.eval_or_error t phrase >>=? fun (out_phrase, o) ->
  !Oprint.out_phrase std_formatter out_phrase;
  printf "OUT: %s\n" (Oloop.Output.stdout o);

  Oloop.close t >>| fun () -> Result.Ok()

let () =
  ignore(main() >>| function
         | Result.Ok () -> shutdown 0
         | Result.Error e -> eprintf "%s\n%!" (Error.to_string_hum e);
                            shutdown 1);
  never_returns(Scheduler.go())
