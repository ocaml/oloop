open Core.Std
open Oloop_core2
open Async.Std

type part = {
  number : float;
  content : string;
}

type t = part list

let phrases_of_string s : string list =
  let rec loop (phrase,phrases) = function
    | [] -> phrase :: phrases
    | line::lines ->
      let accum =
        if String.rstrip line |> String.is_suffix ~suffix:";;" then
          ([], (line::phrase)::phrases)
        else
          (line::phrase, phrases)
      in
      loop accum lines
  in
  let make_phrase l = String.concat ~sep:"\n" (List.rev l) in
  let phrases = loop ([],[]) (String.split_lines s) in
  List.rev_map phrases ~f:make_phrase
  (* The final phrase might be an empty string, which we do not
     consider an error. We simply filter it out. *)
  |> List.filter ~f:(fun x -> not (String.for_all x ~f:Char.is_whitespace))

(** [line_to_part_num ~filename line] parses the part number from a
    line. [None] is returned if the line does not indicate the start
    of a new part. [Error] is returned if the line does indicate start
    of a new part, but there is an error in the formatting. The
    [filename] is only for error messages. *)
let line_to_part_num ~filename line : float option Or_error.t =
  let lstripped = String.lstrip line in
  if String.is_prefix ~prefix:"(* Part " lstripped
    || String.is_prefix ~prefix:"(* part " lstripped then (
    try Ok (Some (Scanf.sscanf lstripped "(* %_s %f *)" ident))
    with _ ->
      error "invalid (* part N *) line"
        (filename,line) <:sexp_of< string * string >>
  )
  else
    Ok None

let of_string ~filename contents =
  let open Result.Monad_infix in
  String.split ~on:'\n' contents
  |> Result.List.fold
      ~init:((0., Buffer.create 100), [])
      ~f:(fun (curr_part, parts) line ->
        line_to_part_num ~filename line >>| function
        | Some part_num ->
          (part_num, (Buffer.create 100)), curr_part::parts
        | None ->
          (
            let part_num,buf = curr_part in
            Buffer.add_string buf line;
            Buffer.add_char buf '\n';
            (part_num,buf), parts
          )
      )
  >>= fun (curr_part,parts) ->
  let parts = List.rev_map (curr_part::parts) ~f:(fun (number,content) ->
    {number; content = Buffer.contents content} )
  in
  let part_nums = List.map parts ~f:(fun x -> x.number) in
  if List.is_sorted_strictly part_nums ~compare:Float.compare then
    Ok parts
  else
    error "part numbers not strictly increasing"
      (filename,part_nums) <:sexp_of< string * float list >>

let of_file filename =
  Reader.file_contents filename
  >>| of_string ~filename
