open Core_kernel.Std
open Async.Std

type 'a t = { stdout: string;  stderr: string }
type 'a kind = bool (* redirect stderr? *)
type separate
type merged

let reader_to_string r =
  Reader.read_until r (`Char Oloop_types.end_output)
                    ~keep_delim:false
  >>| function
  | `Eof -> ""
  | `Eof_without_delim s -> s
  | `Ok s -> s

let make_unsafe proc =
  reader_to_string (Process.stdout proc) >>= fun stdout ->
  reader_to_string (Process.stderr proc) >>= fun stderr ->
  return { stdout;  stderr }

let empty() = { stdout = "";  stderr = "" }

let separate = false
let merged = true
let kind x = if x then `Merged else `Separate

let stdout t = t.stdout
let stderr t = t.stderr
