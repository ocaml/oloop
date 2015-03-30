
module Rule : sig

    type t =
      { required_values : Longident.t list;
        (** Values that must exist and be persistent for the rule to apply. *)
        rewrite : Location.t -> Parsetree.expression -> Parsetree.expression;
        (** The rewrite function. *)
        mutable enabled : bool;
        (** Default state. *)
      }

    val add : Longident.t -> t -> unit
    (** [add lid r] add a rewrite rule, [lid] is the identifier of the
        type constructor. *)

    val async : Longident.t
    (** Key associated to the Async Deferred.t rule. *)
    val lwt : Longident.t
    (** Key associated to the Lwt.t rule. *)

    val enable : Longident.t -> unit
    val disable : Longident.t -> unit


    val rewrite : Parsetree.toplevel_phrase -> Parsetree.toplevel_phrase
    (** [rewrite phrase] return a phrase modified using to the active
        rules.  *)
  end
