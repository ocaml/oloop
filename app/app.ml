open Core.Std
open Async.Std
open Oloop

let main = Command.async
  ~summary:"evaluate code through the OCaml toploop"
  Command.Spec.(
    empty +> anon ("file" %: file)
  )
  (fun file () ->
    Script.of_file file
    >>=? eval_script
    >>|? Script.Evaluated.to_plain_text
    >>|? print_endline
    >>| function
    | Ok () -> ()
    | Error e ->
      eprintf "%s\n" (Error.to_string_hum e)
  )

let () =
  try Command.run ~version:App_conf.version main
  with e -> eprintf "%s\n" (Exn.to_string e)
