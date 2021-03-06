open Core_kernel.Std
open Async.Std

let phrases = [
    "let f ?(x=1) = x";
    "let g ?(x=1) = x";
    "let f ?(x=1) = \"\\e\" ";
    "let h =
       Printf.eprintf \"Hello\";
       fun ?(x=1) -> x";
    "let rec even n = match n with
       | 0 -> true
       | x -> odd (x-1)";
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
           Format.printf "OUT: %S\nERR: %S\n%!"
                         (Oloop.Outcome.stdout e)
                         (Oloop.Outcome.stderr e);
           Ok()
        | `Uneval(e, msg) ->
           Format.printf "[36m%s[0m" msg;
           (match Oloop.Outcome.location_of_uneval e with
            | Some l -> Location.print Format.std_formatter l
            | None -> ());
           Oloop.Outcome.report_uneval Format.std_formatter e;
           Ok()
       )

let () =
  ignore(Oloop.with_toploop Oloop.Outcome.separate ~f:eval_phrases
                            ~msg_with_location:()
         >>| function
         | Ok _ -> shutdown 0
         | Error e -> eprintf "%s\n%!" (Error.to_string_hum e);
                     shutdown 1);
  never_returns(Scheduler.go())
