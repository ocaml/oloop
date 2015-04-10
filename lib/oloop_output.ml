open Core_kernel.Std

type 'a t = { stdout: string Queue.t;  stderr: string Queue.t }
type 'a kind = bool (* redirect stderr? *)
type separate
type merged

let make_unsafe ~stdout ~stderr = {stdout; stderr}

let separate = false
let merged = true
let kind x = if x then `Merged else `Separate
let stdout_queue t = t.stdout
let stderr_queue t = t.stderr

let string_of_queue q =
  let len = Queue.fold q ~init:0 ~f:(fun l s -> l + String.length s) in
  let buf = Bytes.create len in
  let pos = ref 0 in
  Queue.iter q ~f:(fun s -> let len = String.length s in
                            Bytes.blit s 0 buf !pos len;
                            pos := !pos + len);
  buf

let stdout t = string_of_queue t.stdout
let stderr t = string_of_queue t.stderr
