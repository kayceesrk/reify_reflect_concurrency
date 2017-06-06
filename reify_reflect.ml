(* Monadic Reflection :
   http://www.cs.ioc.ee/mpc-amast06/msfp/filinski-slides.pdf *)

(* The monad signature *)
module type Monad =
sig
  type +_ t
  val return : 'a -> 'a t
  val (>>=)  : 'a t -> ('a -> 'b t) -> 'b t
end

module type ReifyReflect =
sig
  type +_ t
  val reify : (unit -> 'a) -> 'a t
  val reflect : 'a t -> 'a
end

(* Build reify and reflect operations for any monad *)
module Make(M: Monad) : ReifyReflect with type 'a t = 'a M.t =
struct
  type +'a t = 'a M.t

  effect E : 'a M.t -> 'a

  let reify f = match f () with
      x -> M.return x
    | effect (E m) k -> M.(>>=) m (continue k)

  let reflect m = perform (E m)
end
