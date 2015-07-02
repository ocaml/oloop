open Core_kernel.Std
open Async.Std

let phrases = [
    "1 +";
    "1 +. 1.";
    "type t = 'a";
    "let rec loop() = 1 + loop() in loop()";
  ]


let eval_phrases t =
  Deferred.Or_error.List.iter
    phrases
    ~f:(fun phrase ->
        Format.printf "# [32m%s[0m;;@\n%!" phrase;
        Oloop.eval t phrase >>| function
        | `Eval e ->
           Oloop.Outcome.print Format.std_formatter (Oloop.Outcome.result e);
           Format.printf "@?";
           Ok()
        | `Uneval(e, msg) ->
           (match Oloop.Outcome.location_of_uneval e with
            | Some l -> Location.print Format.std_formatter l
            | None -> ());
           Format.printf "[36m%s[0m" msg;
           Oloop.Outcome.report_uneval Format.std_formatter e;
           Ok()
       )

let () =
  ignore(Oloop.with_toploop Oloop.Outcome.merged ~f:eval_phrases
         >>| function
         | Ok _ -> shutdown 0
         | Error e -> eprintf "%s\n%!" (Error.to_string_hum e);
                     shutdown 1);
  never_returns(Scheduler.go())
