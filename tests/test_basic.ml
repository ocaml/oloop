open Core.Std
open Async.Std
open Format

let eval t phrase =
  printf "phrase: %S\n%!" phrase;
  Oloop.eval t phrase >>| function
  | Result.Ok(out_phrase, o) ->
     let b = Buffer.create 1024 in
     !Oprint.out_phrase (formatter_of_buffer b) out_phrase;
     printf "OUTCOME: [%s]\n%!" (Buffer.contents b);
     printf "OUT: %S\nERR: %S\n%!" (Oloop.Output.stdout o)
                                   (Oloop.Output.stderr o)
     (* printf "OUT+ERR: %S\n" (Oloop.Output.stdout o) *)
  | Result.Error(e, msg) ->
     printf "ERROR: {|%s|}\n" msg;
     (match Oloop.Outcome.location_of_error e with
      | Some l -> printf "LOCATION: ";
                 Location.print_loc std_formatter l;
                 printf "\n"
      | None -> ())


include Lexer
let main () =
  let phrase1 = "open Printf\n\
                 let f x = x + 1\n\
                 let () = printf \"Hello\n\""
  and phrase2 = "let () = eprintf \"err\n%!\"; \n\
                 printf \"out\n%!\";\n\
                 eprintf \"err2\n%!\"; \n\
                 printf \"out2\n%!\"\n\
                 let x = 1." in
  Oloop.create Oloop.Output.separate >>= function
  (* Oloop.create Oloop.Output.merged >>= function *)
  | Result.Error e ->
     eprintf "%s\n" (Error.to_string_hum e);
     return(shutdown 1)
  | Result.Ok t ->
     eval t phrase1 >>= fun () ->
     eval t phrase2 >>= fun () ->
     Oloop.close t >>| fun () ->
     shutdown 0

let () =
  ignore(main());
  never_returns(Scheduler.go())
