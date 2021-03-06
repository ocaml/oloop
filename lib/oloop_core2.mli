(** Core extensions. After `open Core.Std`, can do `open Oloop_core2`
    to extend some modules in Core.Std. *)
open Core_kernel.Std

module Result : sig
  include module type of Result
  with type ('a,'b) t = ('a,'b) Result.t

  module List : sig
    type ('a, 'b) monad = ('a, 'b) t
    type 'a t = 'a list

    val map
      : 'a list
      -> f:('a -> ('b, 'err) monad)
      -> ('b t, 'err) monad

    val mapi
      : 'a t
      -> f:(int -> 'a -> ('b, 'err) monad)
      -> ('b t, 'err) monad

    val fold
      : 'a t
      -> init:'b
      -> f:('b -> 'a -> ('b, 'err) monad)
      -> ('b, 'err) monad

    val foldi
      : 'a t
      -> init:'b
      -> f:(int -> 'b -> 'a -> ('b, 'err) monad)
      -> ('b, 'err) monad

  end
end
