open Core.Std
open Async.Std

(** Split given file contents into its parts. The [filename] is only
    for error messages. *)
val split_parts_exn
  :  filename:string
  -> string
  -> (float * string) list

(** Split given file into its parts. *)
val split_parts_of_file_exn : string -> (float * string) list Deferred.t

(** Split the given string on the delimiter ";;". The delimiter is
    retained in the output strings. Give [`Eol] to consider only ";;"
    occuring at the end of a line, or [`Anywhere] to allow it
    anywhere. *)
val split_toplevel_phrases
  :  [`Eol | `Anywhere]
  -> string
  -> string list
