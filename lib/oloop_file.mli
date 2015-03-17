(** A collection of parsed code chunks from a .topscript file *)
type t

(** Creates a collection of code chunks given a file name *)
val make_code_chunks : string -> t
