(* Syntax hightlight code and eval ocaml toplevel phrases.
 * Based on http://github.com/ocaml/ocaml.org
 * Modified by Anil Madhavapeddy for Real World OCaml and to use Core *)
open Core.Std
open Async.Std
module Code = Oloop_code

(** Run these phrases silently before any code *)
let initial_phrases = [
  "#use \"topfind\"";
  "#camlp4o";
  "#require \"core\"";
  "#require \"core.syntax\"";
  "#require \"core.top\""
]


(*** Suppress values beginning with _.  Lifted straight from uTop:
 * uTop_main.ml
 * ------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 **)

let orig_print_out_signature = !Toploop.print_out_signature
let orig_print_out_phrase = !Toploop.print_out_phrase

let rec map_items
    (unwrap : 'a -> Outcometree.out_sig_item * 'b)
    (wrap : Outcometree.out_sig_item -> 'b -> 'a)
    (items : 'a list)
    : 'a list
    =
  match items with
  | [] ->
    []
  | item :: items ->
    let sig_item, _ = unwrap item in
    let name, _ =
      match sig_item with
      | Outcometree.Osig_class (_, name, _, _, rs)
      | Outcometree.Osig_class_type (_, name, _, _, rs)
      | Outcometree.Osig_module (name, _, rs)
      | Outcometree.Osig_type ({Outcometree.otype_name=name;_}, rs) ->
        (name, rs)
      | Outcometree.Osig_typext ({Outcometree.oext_name=name;_}, _)
      | Outcometree.Osig_modtype (name, _)
      | Outcometree.Osig_value (name, _, _) ->
        (name, Outcometree.Orec_not)
    in
    let keep = name = "" || name.[0] <> '_' in
    if keep then
      item :: map_items unwrap wrap items
    else
      (* Replace the [Orec_next] at the head of items by [Orec_first] *)
      let items =
        match items with
        | [] ->
          []
        | item :: items' ->
          let sig_item, extra = unwrap item in
          match sig_item with
          | Outcometree.Osig_class (a, name, b, c, rs) ->
            if rs = Outcometree.Orec_next then
              wrap (Outcometree.Osig_class (a, name, b, c, Outcometree.Orec_first)) extra :: items'
            else
              items
          | Outcometree.Osig_class_type (a, name, b, c, rs) ->
            if rs = Outcometree.Orec_next then
              wrap (Outcometree.Osig_class_type (a, name, b, c, Outcometree.Orec_first)) extra :: items'
            else
              items
          | Outcometree.Osig_module (name, a, rs) ->
            if rs = Outcometree.Orec_next then
              wrap (Outcometree.Osig_module (name, a, Outcometree.Orec_first)) extra :: items'
            else
              items
          | Outcometree.Osig_type (out_type_decl, rs) ->
            if rs = Outcometree.Orec_next then
              wrap (Outcometree.Osig_type (out_type_decl, Outcometree.Orec_first)) extra :: items'
            else
              items
          | Outcometree.Osig_typext _
          | Outcometree.Osig_modtype _
          | Outcometree.Osig_value _ ->
            items
      in
      map_items unwrap wrap items

let print_out_signature (pp:Format.formatter) (items : Outcometree.out_sig_item list) : unit =
  orig_print_out_signature pp (map_items (fun x -> (x, ())) (fun x () -> x) items)

let print_out_phrase (pp:Format.formatter) (phrase:Outcometree.out_phrase) : unit =
  let phrase =
    match phrase with
    | Outcometree.Ophr_eval _
    | Outcometree.Ophr_exception _ -> phrase
    | Outcometree.Ophr_signature items ->
      Outcometree.Ophr_signature (map_items (fun x -> x) (fun x y -> (x, y)) items)
  in
  orig_print_out_phrase pp phrase

let () =
  Toploop.print_out_signature := print_out_signature;
  Toploop.print_out_phrase := print_out_phrase

(** End of uTop code *)

let string_of_queue q =
  String.concat ~sep:"" (Queue.to_list q)

let toploop_eval t (phrase: string) =
  Oloop.eval t phrase >>| function
  | Result.Ok (out_phrase, o) ->
     let b = Buffer.create 1024 in
     !Oprint.out_phrase (Format.formatter_of_buffer b) out_phrase;
     { Code.input = phrase;
       output = Buffer.contents b;
       stdout = string_of_queue (Oloop.Output.stdout o);
       stderr = string_of_queue (Oloop.Output.stderr o) }
  | Result.Error(_, msg) ->
     { Code.input = phrase;
       output = msg;
       stdout = "";  stderr = "" }

let run ?out_dir ?open_core ?open_async filename =
  eprintf "C: %s\n%!" filename;
  let out_dir = match out_dir with
    | Some x -> x
    | None -> Filename.dirname filename
  in
  let initial_phrases = match open_core with
    | Some x ->
       if x then initial_phrases @ ["open Core.Std"]
       else initial_phrases
    | None -> initial_phrases
  in
  let initial_phrases = match open_async with
    | Some x ->
       if x then initial_phrases @ ["#require \"async\"";"open Async.Std"]
       else initial_phrases
    | None -> initial_phrases
  in
  Oloop.create Oloop.Output.separate >>= function
  | Result.Error e ->
     return(eprintf "Could not create a toploop for %S\nReason: %s\n"
                    filename (Error.to_string_hum e))
  | Result.Ok t ->
  Deferred.List.iter initial_phrases
                     ~f:(fun phrase ->
                         Oloop.eval t phrase >>| function
                         | Result.Ok _ -> ()
                         | Error(_, msg) -> eprintf "ERROR: %s\n%!" msg
                        ) >>= fun () ->
  let parts =
    Code.split_parts_exn ~filename (In_channel.read_all filename) in
  let eval_part (part, content) =
    eprintf "X: %s, part %f\n%S\n\n%!" filename part content;
    let data = ok_exn (Code.split_toplevel_phrases `Anywhere content) in
    Deferred.List.map data ~f:(toploop_eval t) >>| fun data ->
    let data = <:sexp_of< Code.phrase list >> data
               |> Sexp.to_string in
    let base = Filename.(basename filename |> chop_extension) in
    let out_file = sprintf "%s/%s.%f.txt" out_dir base part in
    Out_channel.write_all out_file ~data
  in
  Deferred.List.iter parts ~f:eval_part >>| fun () ->
  shutdown 0


let main = Command.basic
  ~summary:"Run files through the Core toplevel"
  Command.Spec.(
    empty
    +> flag "-o" (optional string)
      ~doc:"DIR Write files to directory DIR. Default is write to the \
            same directory that FILE is in."
    +> flag "-c" (optional bool) ~doc:"CORE Do open Core.Std before \
                                      evaluating any code"
    +> flag "-a" (optional bool) ~doc:"ASYNC Do open Async.Std before \
                                      evaluating any code"
    +> anon (sequence ("file" %: file))
  )
  (fun out_dir open_core open_async files () ->
   ignore(Deferred.List.iter files ~f:(run ?out_dir ?open_core ?open_async));
   never_returns(Scheduler.go()))

let () = Command.run main
