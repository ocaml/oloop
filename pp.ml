(* Pre-process some files in lib/ to handle various compiler versions.
   This use the cpp way of writing conditionals on OCAML_VERSION *)

open Printf
open Scanf

let ocaml_version =
  sscanf Sys.ocaml_version "%d.%d.%d" (fun m n p -> (m,n,p))

let version_cmp (m1, n1, p1) (m2, n2, p2) =
  let c = compare m1 m2 in
  if c <> 0 then c
  else let c = compare n1 n2 in
       if c <> 0 then c
       else compare p1 p2

type line_type =
  | Normal
  | If of bool
  | Else
  | Endif

let classify_line l =
  let verify_constraint cmp m n p =
    let c = version_cmp ocaml_version (m, n, p) in
    match cmp with
    | ">" -> c > 0
    | ">=" -> c >= 0
    | "=" | "==" -> c = 0
    | "<" -> c < 0
    | "<=" -> c <= 0
    | _ -> printf "Comparison %S not correct.\n" cmp;
          exit 1 in
  try If(sscanf l "#if OCAML_VERSION %s (%d, %d, %d)" verify_constraint)
  with _ ->
       if l = "#else" then Else
       else if l = "#endif" then Endif
       else Normal

let copy_and_filter fname0 fname1 =
  let fh0 = open_in fname0 in
  let fh1 = open_out fname1 in
  fprintf fh1 "(* GENERATED FILE, modify the original %S *)\n" fname0;
  let output = ref true in (* whether to output the current line *)
  try
    while true do
      let l = input_line fh0 in
      match classify_line l with
      | If test -> output := test
      | Else -> output := not !output
      | Endif -> output := true
      | Normal ->
         if !output then (
           output_string fh1 l;
           output_char fh1 '\n';
         )
    done;
    assert false
  with End_of_file ->
    close_in fh0;
    close_out fh1

let () =
  copy_and_filter "lib/oloop_ocaml.pp.ml" "lib/oloop_ocaml.ml"
