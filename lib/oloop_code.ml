open Core.Std
open Async.Std

(** [line_to_part filename line] parses the part number from a
    line. [None] is returned if the line does not indicate the start
    of a new part. [Error] is returned if the line does indicate start
    of a new part, but there is an error in the formatting. The
    [filename] is only for error messages. *)
let line_to_part filename line : float option Or_error.t =
  let lstripped = String.lstrip line in
  if String.is_prefix ~prefix:"(* part " lstripped then (
    try Ok (Some (Scanf.sscanf lstripped "(* part %f *)" ident))
    with _ ->
      error "invalid (* part N *) line"
        (filename,line) <:sexp_of< string * string >>
  )
  else
    Ok None

let split_parts_exn ~filename contents =
  String.split ~on:'\n' contents
  |> List.fold
      ~init:(0., [ (0., Buffer.create 100) ])
      ~f:(fun (part,parts) line ->
        match ok_exn (line_to_part filename line) with
        | Some part ->
          (part, (part,(Buffer.create 100)) :: parts)
        | None ->
          (
            match List.Assoc.find parts part with
            | Some buf ->
              Buffer.add_string buf line;
              Buffer.add_char buf '\n';
              (part,parts)
            | None ->
              assert false
          )
      )
  |> snd
  |> List.map ~f:(fun (a,b) -> (a, String.strip (Buffer.contents b)))
  |> List.rev

let split_parts_of_file_exn filename =
  Reader.file_contents filename
  >>| split_parts_exn ~filename

let split_toplevel_phrases_eol s =
  let rec loop (phrase,phrases) = function
    | [] -> (phrase,phrases)
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
  let phrase,phrases = loop ([],[]) (String.split_lines s) in
  match phrase with
  | _::_ ->
    error "OCaml toplevel phrase did not end with double semicolon"
      (make_phrase phrase) sexp_of_string
  | [] ->
    match phrases with
    | [] -> Or_error.error_string "empty OCaml toplevel phrase"
    | _ -> Ok (List.map (List.rev phrases) ~f:make_phrase)


let split_toplevel_phrases_anywhere s =
  let open Result.Monad_infix in
  let s = String.rstrip s in
  let indexes =
    String.substr_index_all s ~may_overlap:false ~pattern:";;"
    |> fun l -> -2::l
  in
  (
    match indexes with
    | [] -> assert false
    | _::[] ->
      error "double semicolon not found in OCaml toplevel phrase"
        s sexp_of_string
    | _ ->
      let rec loop accum = function
        | [] -> accum
        | _::[] -> accum
        | i::(j::_ as indexes) ->
          loop ((i+2,j+2)::accum) indexes
      in
      Ok (loop [] indexes)
  )
  >>= fun ranges ->
  match ranges with
  | [] -> assert false
  | (_,last)::_ ->
    if last < String.length s then
      error "OCaml toplevel phrase doesn't end with ;;"
        s sexp_of_string
    else
      List.rev ranges
      |> List.map ~f:(fun (i,j) -> String.slice s i j)
      |> fun x -> Ok x


let split_toplevel_phrases where s = match where with
  | `Eol -> split_toplevel_phrases_eol s
  | `Anywhere -> split_toplevel_phrases_anywhere s

