open Core.Std
open Async.Std
open Oloop

let bool_to_unit = function true -> Some () | false -> None

let main = Command.async
  ~summary:"evaluate code through the OCaml toploop"
  Command.Spec.(
    empty
    +> map ~f:(function [] -> None | x -> Some x)
           (flag "-I" (listed file)
              ~doc:"<dir> Add <dir> to the list of include directories")
    +> flag "-init" (optional file)
       ~doc:"<file> Load <file> instead of default init file"
    +> map ~f:bool_to_unit
       (flag "-no-app-funct" no_arg ~doc:" Deactivate applicative functors")
    +> map ~f:bool_to_unit
       (flag "-principal" no_arg ~doc:" Check principality of type inference")
    +> map ~f:bool_to_unit
       (flag "-rectypes" no_arg ~doc:" Allow arbitrary recursive types")
    +> map ~f:bool_to_unit
       (flag "-short-paths" no_arg ~doc:" Shorten paths in types")
    +> map ~f:bool_to_unit
       (flag "-strict-sequence" no_arg
          ~doc:" Left-hand part of a sequence must have type unit")
    +> map ~f:bool_to_unit
       (flag "-thread" no_arg
          ~doc:" Generate code that supports the system threads library")
    +> flag "-prog" (optional string)
       ~doc:"<path> Full <path> to specially customized toploop"
    +> flag "-working-dir" (optional string)
       ~doc:"<path> Switch to <path> before running SCRIPT"
    +> map ~f:bool_to_unit
       (flag "-msg-with-location" no_arg
          ~doc:" Add the source location to error messages")
    +> map ~f:bool_to_unit
       (flag "-silent-directives" no_arg
          ~doc:" Make all toploop directives return an empty structure")
    +> map ~f:bool_to_unit
       (flag "-determine-deferred" no_arg
          ~doc:" Determine anonymous Deferred.t values (as Utop does)")
    +> map ~f:bool_to_unit
       (flag "-determine-lwt" no_arg
          ~doc:" Determine anonymous Lwt.t values (as Utop does)")
    +> anon ("script" %: file)
  )
  (fun include_dirs init no_app_functors principal rectypes
       short_paths strict_sequence thread
       prog working_dir msg_with_location silent_directives
       determine_deferred determine_lwt
       file () ->
    Script.of_file file
    >>=? eval_script ?include_dirs ?init ?no_app_functors
                     ?principal ?rectypes
                     ?short_paths ?strict_sequence ?thread
                     ?prog ?working_dir ?msg_with_location ?silent_directives
                     ?determine_deferred ?determine_lwt
    >>|? Script.Evaluated.to_text
    >>|? print_endline
    >>| function
    | Ok () -> ()
    | Error e ->
      eprintf "%s\n" (Error.to_string_hum e)
  )

let () =
  try Command.run ~version:App_conf.version main
  with e -> eprintf "%s\n" (Exn.to_string e)
