open Core.Std
open Async.Std
open Format

let x () =
  printf "Hello\n";
  eprintf "Here\n%!";
  printf "Hello2\n"

let string_of_queue q =
  String.concat ~sep:"" (Queue.to_list q)

let eval t phrase =
  printf "phrase: %S\n%!" phrase;
  Oloop.eval t phrase >>| fun (res, o) ->
  (match res with
  | Result.Ok out_phrase ->
     let b = Buffer.create 1024 in
     !Oprint.out_phrase (formatter_of_buffer b) out_phrase;
     printf "OUTCOME: [%s]\n%!" (Buffer.contents b);
  | Result.Error(e, msg) ->
     printf "ERROR: {|%s|}\n" msg;
     (match Oloop.location_of_error e with
      | Some l -> printf "LOCATION: ";
                 Location.print_loc std_formatter l;
                 printf "\n"
      | None -> ()));
  printf "OUT: %S\nERR: %S\n%!" (string_of_queue(Oloop.Output.stdout o))
                              (string_of_queue(Oloop.Output.stderr o))
(* printf "OUT+ERR: %S\n" (string_of_queue(Oloop.Output.stdout o)) *)


include Lexer
let main () =
  let phrase1 = "open Printf\n\
                 let f x = x + 1 1\n\
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
