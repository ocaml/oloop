(*							-*-tuareg-*- *)
(*** Suppress values beginning with _.  Lifted straight from uTop:
 * uTop_main.ml
 * ------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 **)

let rec map_items unwrap wrap items =
  match items with
  | [] ->
     []
  | item :: items ->
     let sig_item, _ = unwrap item in
     let name, _rec_status =
       match sig_item with
       | Outcometree.Osig_class (_, name, _, _, rs)
       | Outcometree.Osig_class_type (_, name, _, _, rs)
       | Outcometree.Osig_module (name, _, rs)
#if OCAML_VERSION >= (4, 02, 0)
       | Outcometree.Osig_type ({ Outcometree.otype_name = name }, rs) ->
#else
       | Outcometree.Osig_type ((name, _, _, _, _), rs) ->
#endif
          (name, rs)
#if OCAML_VERSION >= (4, 02, 0)
       | Outcometree.Osig_typext ({ Outcometree.oext_name = name}, _)
#else
       | Outcometree.Osig_exception (name, _)
#endif
       | Outcometree.Osig_modtype (name, _)
       | Outcometree.Osig_value (name, _, _) ->
          (name, Outcometree.Orec_not)
#if OCAML_VERSION >= (4, 03, 0)
       | Outcometree.Osig_ellipsis -> ("", Outcometree.Orec_not)
#endif
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
                 wrap (Outcometree.Osig_class
                         (a, name, b, c, Outcometree.Orec_first)) extra
                 :: items'
               else
                 items
            | Outcometree.Osig_class_type (a, name, b, c, rs) ->
               if rs = Outcometree.Orec_next then
                 wrap (Outcometree.Osig_class_type
                         (a, name, b, c, Outcometree.Orec_first)) extra
                 :: items'
               else
                 items
            | Outcometree.Osig_module (name, a, rs) ->
               if rs = Outcometree.Orec_next then
                 wrap (Outcometree.Osig_module
                         (name, a, Outcometree.Orec_first)) extra :: items'
               else
                 items
            | Outcometree.Osig_type (oty, rs) ->
               if rs = Outcometree.Orec_next then
                 wrap (Outcometree.Osig_type
                         (oty, Outcometree.Orec_first)) extra :: items'
               else
                 items
#if OCAML_VERSION >= (4, 02, 0)
            | Outcometree.Osig_typext _
#else
            | Outcometree.Osig_exception _
#endif
#if OCAML_VERSION >= (4, 03, 0)
            | Outcometree.Osig_ellipsis
#endif
            | Outcometree.Osig_modtype _
            | Outcometree.Osig_value _ ->
               items
       in
       map_items unwrap wrap items


let print_out_signature (pp:Format.formatter)
                        (items : Outcometree.out_sig_item list) : unit =
  !Oprint.out_signature pp (map_items (fun x -> (x, ())) (fun x () -> x) items)

let print_out_phrase (pp:Format.formatter)
                     (phrase:Outcometree.out_phrase) : unit =
  let phrase =
    match phrase with
    | Outcometree.Ophr_eval _
    | Outcometree.Ophr_exception _ -> phrase
    | Outcometree.Ophr_signature items ->
      Outcometree.Ophr_signature (map_items (fun x -> x) (fun x y -> (x, y)) items)
  in
  !Oprint.out_phrase pp phrase

(* End of uTop code *)
