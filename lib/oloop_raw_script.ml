open Core.Std
open Oloop_core2
open Async.Std

type item = {
  input : string;
  output : string;
}

type t = item list

(** List of lines is in reverse order. The [output] must be empty if
    the [input] is, i.e. you cannot have output without input. *)
type partial_item = {
  input : string list;
  output : string list;
}

(** [complete x items] assumes we are done parsing [x], so completes
    it by converting it to an [item]. It then adds the item to
    [items]. Note the items are kept in reverse order. *)
let complete (x:partial_item) (items : item list) : item list =
  let f lines = List.rev lines |> String.concat ~sep:"\n" in
  if x.input = [] then (* partial item is empty *)
    items
  else
    {input = f x.input; output = f x.output}::items

let of_string ~filename content =
  let open Result.Monad_infix in
  String.split content ~on:'\n' |>
  List.filter ~f:(function "" -> false | _ -> true) |>
  Result.List.fold
    ~init:(`Parsing_input, {input=[];output=[]}, [])
    ~f:(fun (state, curr_item, completed_items) line ->
        match
          String.is_prefix line ~prefix:"# ",
          String.is_suffix (String.rstrip line) ~suffix:";;",
          state
        with
        | true,true,_ ->
          Ok (
            `Parsing_output,
            {input=[line]; output=[]},
            (complete curr_item completed_items)
          )
        | true,false,_ ->
          Ok (
            `Parsing_input,
            {input=[line];output=[]},
            (complete curr_item completed_items)
          )
        | false,true,`Parsing_input -> (
            assert (curr_item.output = []);
            Ok (
              `Parsing_output,
              {curr_item with input=line::curr_item.input},
              completed_items
            )
          )
        | false,true,`Parsing_output ->
          error "unexpected double semicolon while parsing output"
            filename sexp_of_string
        | false,false,`Parsing_input ->
          Ok (
            `Parsing_input,
            {curr_item with input=line::curr_item.input},
            completed_items
          )
        | false,false,`Parsing_output ->
          Ok (
            `Parsing_output,
            {curr_item with output=line::curr_item.output},
            completed_items
          )
      )
  >>| fun (_, curr_item, completed_items) ->
  complete curr_item completed_items |>
  List.rev

let of_file filename =
  Reader.file_contents filename
  >>| of_string ~filename
