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
module Make(M: Monad) : ReifyReflect with type 'a t = 'a M.t
