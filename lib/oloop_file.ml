(* #require "core" *)
(* #require "compiler-libs" *)
open Core.Std

type item = {section:float;
             content:Parsetree.toplevel_phrase}

type t = item list

let make_code_chunks filename : t=
  let these_lines = In_channel.read_lines filename in
  let stripped_lines = List.map these_lines
      ~f:(fun line ->
          String.lstrip line |>
          String.rstrip ) in
  let stripped_lines = match List.hd_exn stripped_lines with
    | "" -> "(* Part 0.0 *)" :: stripped_lines
    | _ -> stripped_lines in
  let result = List.group stripped_lines
      ~break:(fun _ next ->
          if (String.prefix next 2) = "(*"
          then true
          else false
        ) in
  let prunned = List.map result ~f:(fun l -> List.filter l ~f:(fun i ->
      if i = ""
      then false
      else true)) in
  List.map prunned ~f:(fun l ->
      let chopped = String.split (List.hd_exn l) ~on:' ' in
      let sec = Float.of_string (List.nth_exn chopped 2) in
      let rest = List.slice l 1 (List.length l) in
      let code = String.concat ~sep:"\n" rest in
      {section = sec;
       content = !Toploop.parse_toplevel_phrase (Lexing.from_string code)})
