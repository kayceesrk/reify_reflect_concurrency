(* Signatures *)

module type SchedD = sig
  val fork  : (unit -> unit) -> unit
  val yield : unit -> unit
  val run   : (unit -> unit) -> unit
end

module type MVarD = sig
  type 'a mvar
  val create       : 'a -> 'a mvar
  val create_empty : unit -> 'a mvar
  val put          : 'a mvar -> 'a -> unit
  val take         : 'a mvar -> 'a
end

module type Monad =
sig
  type +_ t
  val return : 'a -> 'a t
  val (>>=)  : 'a t -> ('a -> 'b t) -> 'b t
  val (>>)   : 'a t -> 'b t -> 'b t
end

module type SchedM = sig
  include Monad
  val fork  : unit t -> unit t
  val yield : unit -> unit t
  val run   : unit t -> unit

  val iter_m : ('a -> unit t) -> 'a list -> unit t
  val map_m  : ('a -> 'b t) -> 'a list -> 'b list t
end

module type MVarM = sig
  include Monad
  type 'a mvar
  val create : 'a -> 'a mvar
  val create_empty : unit -> 'a mvar
  val put : 'a mvar -> 'a -> unit t
  val take : 'a mvar -> 'a t
end

(* Base implementation -- Direct-style *)

module SchedD = struct
  effect Fork    : (unit -> unit) -> unit
  effect Yield   : unit

  type 'a cont = ('a,unit) continuation
  effect Suspend : ('a cont -> unit) -> 'a
  effect Resume  : ('a cont * 'a) -> unit

  let run main =
    let run_q = Queue.create () in
    let enqueue t v =
      Queue.push (fun () -> continue t v) run_q
    in
    let rec dequeue () =
      if Queue.is_empty run_q then ()
      else Queue.pop run_q ()
    in
    let rec spawn f =
      match f () with
      | () -> dequeue ()
      | effect Yield k -> enqueue k (); dequeue ()
      | effect (Fork f) k -> enqueue k (); spawn f
      | effect (Suspend f) k -> f k; dequeue ()
      | effect (Resume (k', v)) k ->
          enqueue k' v; continue k ()
    in
    spawn main

  let fork f = perform (Fork f)
  let yield () = perform Yield
  let suspend f = perform (Suspend f)
  let resume (k,v) = perform (Resume (k,v))
end

module MVarD = struct

  module S = SchedD

  type 'a mv_state =
    | Full  of 'a * ('a * unit S.cont) Queue.t
    | Empty of 'a S.cont Queue.t

  type 'a mvar = 'a mv_state ref

  let create_empty () = ref (Empty (Queue.create ()))

  let create v = ref (Full (v, Queue.create ()))

  let put mv v =
    match !mv with
    | Full (v', q) -> S.suspend (fun k -> Queue.push (v,k) q)
    | Empty q ->
        if Queue.is_empty q then
          mv := Full (v, Queue.create ())
        else
          let t = Queue.pop q in
          S.resume (t, v)

  let take mv =
    match !mv with
    | Empty q -> S.suspend (fun k -> Queue.push k q)
    | Full (v, q) ->
        if Queue.is_empty q then
          (mv := Empty (Queue.create ()); v)
        else
          let (v', t) = Queue.pop q in
          (mv := Full (v', q);
           S.resume (t, ());
           v)
end

(* Base implementation -- Monad *)

module SchedM = struct
  type action =
    | Atom of zaction
    | Fork of zaction * zaction
    | Yield of zaction
    | Suspend
    | Resume of zaction * zaction
    | Stop

  and zaction = unit -> action

  type +'a t = ('a -> action) -> action

  type 'a cont = 'a -> action

  let (>>=) f k = fun c -> f (fun a -> k a c)
  let (>>) a b = a >>= (fun _ -> b)
  let return x = fun c -> c x
  let atom f = fun c -> Atom (fun () -> (let b = f () in c b))
  let action f = f (fun () -> Stop)
  let fork f = fun c -> Fork ((fun () -> action f), c)
  let stop = fun c -> Stop
  let yield () = fun c -> Yield c
  let suspend f = fun c ->
    match f c with
    | None -> Suspend
    | Some (v, None) -> c v
    | Some (v, Some l) -> Resume ((fun () -> c v), l)

  type ready_cont = zaction
  let prepare k v = fun () -> k v

  let rec round = function
      | [] -> ()
      | (x::xs) -> match x with
          | Atom th -> let y = th () in round (xs @ [y])
          | Fork (a1, a2) -> round (a2 () :: a1 () :: xs)
          | Yield a -> round ( xs @ [a ()])
          | Suspend -> round xs
          | Resume (a1, a2) -> round (a1 () :: a2 () :: xs)
          | Stop -> round xs

  let run m = round [action m]

  let rec iter_m f l =
    match l with
      | [] -> return ()
      | x :: l ->
          let tx = f x and tl = iter_m f l in
          tx >>= fun () -> tl

  let map f m = (>>=) m (fun x -> return (f x))
  let (>|=) t f = map f t

  let rec map_m f l =
    match l with
    | [] -> return []
    | x :: l ->
      let tx = f x and tl = map_m f l in
      tx >>= fun x ->
      tl >|= fun l ->
      x :: l
end

module MVarM = struct

  module S = SchedM
  include S

  type 'a mv_state =
    | Full  of 'a * ('a * unit S.cont) Queue.t
    | Empty of 'a S.cont Queue.t

  type 'a mvar = 'a mv_state ref

  let create_empty () = ref (Empty (Queue.create ()))

  let create v = ref (Full (v, Queue.create ()))

  let put mv v =
    S.suspend ( fun k ->
    match !mv with
    | Full (v', q) ->
        Queue.push (v,k) q;
        None
    | Empty q ->
        if Queue.is_empty q then
          ( mv := Full (v, Queue.create ());
            Some ((), None))
        else
          let t = Queue.pop q in
          Some ((), Some (S.prepare t v)) )

  let (>>) = S.(>>)

  let take mv =
    S.suspend (fun k ->
    match !mv with
    | Empty q ->
        Queue.push k q;
        None
    | Full (v, q) ->
        if Queue.is_empty q then
          (mv := Empty (Queue.create ());
          Some (v, None))
        else
          let (v', t) = Queue.pop q in
          mv := Full (v', q);
          Printf.printf "take: resume\n";
          Some (v, Some (S.prepare t ())))
end

(* Chameneos *)

module ChameneosCommon = struct

  module List = ListLabels
  module String = StringLabels

  open Printf

  module Color = struct
    type t =
    | Blue
    | Red
    | Yellow

    let complement t t' =
    match t, t' with
      | Blue, Blue -> Blue
      | Blue, Red -> Yellow
      | Blue, Yellow -> Red
      | Red, Blue -> Yellow
      | Red, Red -> Red
      | Red, Yellow -> Blue
      | Yellow, Blue -> Red
      | Yellow, Red -> Blue
      | Yellow, Yellow -> Yellow

    let to_string = function
      | Blue -> "blue"
      | Red -> "red"
      | Yellow -> "yellow"

    let all = [ Blue; Red; Yellow ]
  end

  type chameneos = Color.t ref

  let spell_int i =
    let spell_char = function
      | '0' -> "zero"
      | '1' -> "one"
      | '2' -> "two"
      | '3' -> "three"
      | '4' -> "four"
      | '5' -> "five"
      | '6' -> "six"
      | '7' -> "seven"
      | '8' -> "eight"
      | '9' -> "nine"
      | x -> failwith "unexpected char"
    in
    let s = string_of_int i in
    String.iter s ~f:(fun c -> printf " %s" (spell_char c))

  let print_complements () =
    List.iter Color.all ~f:(fun c1 ->
      List.iter Color.all ~f:(fun c2 ->
        printf "%s + %s -> %s\n"
    (Color.to_string c1)
    (Color.to_string c2)
    (Color.to_string (Color.complement c1 c2))));
    printf "\n"

  let rec tabulate' acc f = function
    | 0 -> acc
    | n -> tabulate' (f()::acc) f (n-1)

  let tabulate f n = List.rev @@ tabulate' [] f n

end

module ChameneosD (Sched : SchedD) (MVar : MVarD) = struct

  include ChameneosCommon

  open Printf

  type mp =
  | Nobody of int
  | Somebody of int * chameneos * chameneos MVar.mvar

  let arrive (mpv : mp MVar.mvar) (finish : (int * int) MVar.mvar) (ch : chameneos) =
    let waker = MVar.create_empty () in
    let inc x i = if (x == ch) then i+1 else i in
    let rec go t b =
      let w = MVar.take mpv in
      match w with
      | Nobody 0 ->
          MVar.put mpv w;
          MVar.put finish (t,b)
      | Nobody q ->
          MVar.put mpv (Somebody (q, ch, waker));
          go (t+1) @@ inc (MVar.take waker) b
      | Somebody (q, ch', waker') ->
          MVar.put mpv (Nobody (q - 1));
          Sched.yield ();
          let c'' = Color.complement !ch !ch' in
          ch := c'';
          ch' := c'';
          MVar.put waker' ch;
          go (t+1) @@ inc ch' b
  in go 0 0

  let work colors n =
    let () = List.iter colors ~f:(fun c ->
                printf " %s" (Color.to_string c)); printf "\n" in
    let fs = tabulate MVar.create_empty (List.length colors) in
    let mpv = MVar.create (Nobody n) in
    let chams = List.map ~f:(fun c -> ref c) colors in
    let () = List.iter2 ~f:(fun fin ch ->
                Sched.fork (fun () -> arrive mpv fin ch)) fs chams in
    let ns = List.map ~f:MVar.take fs in
    let () = List.iter ~f:(fun (n,b) -> print_int n; spell_int b; printf "\n") ns in
    let sum_meets = List.fold_left ~init:0 ~f:(fun acc (n,_) -> n+acc) ns in
    spell_int sum_meets; printf "\n"

  let main n = Sched.run (fun () ->
    print_complements ();
    let module C = Color in
    work [ C.Blue; C.Red; C.Yellow ] n;
    printf "\n";
    work [ C.Blue; C.Red; C.Yellow; C.Red; C.Yellow; C.Blue; C.Red; C.Yellow; C.Red; C.Blue ] n;
    printf "\n")
end

module ChameneosM (Sched : SchedM) (MVar : MVarM with type 'a t = 'a Sched.t) = struct

  include ChameneosCommon

  open Printf

  let (>>=) = MVar.(>>=)
  let (>>) = MVar.(>>)

  type mp =
  | Nobody of int
  | Somebody of int * chameneos * chameneos MVar.mvar

  let arrive (mpv : mp MVar.mvar) (finish : (int * int) MVar.mvar) (ch : chameneos) =
    let waker = MVar.create_empty () in
    let inc x i = if (x == ch) then i+1 else i in
    let rec go t b =
      MVar.take mpv >>= fun w ->
      match w with
      | Nobody 0 ->
          MVar.put mpv w >>
          MVar.put finish (t,b)
      | Nobody q ->
          Sched.yield () >>
          MVar.put mpv (Somebody (q, ch, waker)) >>
          MVar.take waker >>= fun w' ->
          go (t+1) @@ inc w' b
      | Somebody (q, ch', waker') ->
          Sched.yield () >>
          MVar.put mpv (Nobody (q - 1)) >>
          let c'' = Color.complement !ch !ch' in
          let () = ch := c'' in
          let () = ch' := c'' in
          MVar.put waker' ch  >>
          go (t+1) @@ inc ch' b
    in go 0 0

  let work colors n =
    let () = List.iter colors ~f:(fun c ->
                printf " %s" (Color.to_string c)); printf "\n" in
    let fs = tabulate MVar.create_empty (List.length colors) in
    let mpv = MVar.create (Nobody n) in
    let chams = List.map ~f:(fun c -> ref c) colors in
    let comb = List.combine fs chams in
    Sched.iter_m (fun (fin,ch) -> Sched.fork (arrive mpv fin ch)) comb >>
    Sched.map_m MVar.take fs >>= fun ns ->
    let () = List.iter ~f:(fun (n,b) -> print_int n; spell_int b; printf "\n") ns in
    let sum_meets = List.fold_left ~init:0 ~f:(fun acc (n,_) -> n+acc) ns in
    let () = spell_int sum_meets in
    let () = printf "\n" in
    Sched.return ()

  let main n = Sched.run (
    print_complements ();
    let module C = Color in
    work [ C.Blue; C.Red; C.Yellow ] n >>= fun () ->
    printf "\n";
    work [ C.Blue; C.Red; C.Yellow; C.Red; C.Yellow;
            C.Blue; C.Red; C.Yellow; C.Red; C.Blue ] n >>= fun () ->
    printf "\n";
    Sched.return ())
end

(* Refiy reflect *)

module type ReifyReflect =
sig
  type +_ t
  val reify : (unit -> 'a) -> 'a t
  val reflect : 'a t -> 'a
end

(* Build reify and reflect operations for any monad *)
module Make_ReifyReflect(M: Monad) : ReifyReflect with type 'a t = 'a M.t =
struct
  type +'a t = 'a M.t

  effect E : 'a M.t -> 'a

  let reify f = match f () with
      x -> M.return x
    | effect (E m) k -> M.(>>=) m (continue k)

  let reflect m = perform (E m)
end

(* Direct style *)
module Chameneos = ChameneosD(SchedD)(MVarD)

(* Reify-reflect -- conc monad *)

module RR = Make_ReifyReflect(SchedM)

module SchedRR = struct
  let fork f = RR.reflect (SchedM.fork (RR.reify f))
  let run f = SchedM.run (RR.reify f)
  let yield () = RR.reflect (SchedM.yield ())
end

module MVarRR = struct
  include MVarM
  let put mv v = RR.reflect (put mv v)
  let take mv = RR.reflect (take mv)
end

module ChameneosRR = ChameneosD(SchedRR)(MVarRR)

(* Reify-reflect -- lwt *)

module RRLwt = Make_ReifyReflect(struct
  include Lwt
  let (>>) a b = a >>= (fun _ -> b)
end)

module SchedRRLwt = struct
  let fork f = Lwt.ignore_result (RRLwt.reify f)
  let run f = Lwt_main.run (RRLwt.reify f)
  let yield () = RRLwt.reflect (Lwt_main.yield ())
end

module MVarRRLwt = struct
  type 'a mvar = 'a Lwt_mvar.t
  include Lwt_mvar
  let put mv v = RRLwt.reflect (put mv v)
  let take mv = RRLwt.reflect (take mv)
end

module ChameneosRRLwt = ChameneosD(SchedRRLwt)(MVarRRLwt)

(* Benchmark everything *)

module Benchmark = struct
  let get_mean_sd l =
    let get_mean l = (List.fold_right (fun a v -> a +. v) l 0.) /.
                (float_of_int @@ List.length l)
    in
    let mean = get_mean l in
    let sd = get_mean @@ List.map (fun v -> abs_float (v -. mean) ** 2.) l in
    (mean, sd)

  let benchmark f s n =
    let _ = Printf.printf "**************** Benchmarking %s ****************\n%!" s in
    let rec run acc = function
    | 0 -> acc
    | n ->
        let t1 = Unix.gettimeofday () in
        let () = f () in
        let d = Unix.gettimeofday () -. t1 in
        run (d::acc) (n-1)
    in
    let r = run [] n in
    get_mean_sd r
end

let n =
  try
    int_of_string (Sys.argv.(1))
  with
  | _ -> 600

let repeats = 5

let _ = Gc.full_major ()
let (cm,csd) = Benchmark.benchmark (fun () -> Chameneos.main n)"Chameneos"  repeats
let _ = Gc.full_major ()

module M = ChameneosM(SchedM)(MVarM)
let (cmm,cmsd) = Benchmark.benchmark (fun () -> M.main n) "Chameneos_M" repeats
let _ = Gc.full_major ()

module SchedLwt = struct
  include Lwt
  let (>>) a b = a >>= fun _ -> b
  let fork f = Lwt.ignore_result f; return ()
  let yield = Lwt_main.yield
  let run = Lwt_main.run
  let iter_m = Lwt_list.iter_p
  let map_m = Lwt_list.map_p
end

module ChameneosLwt = ChameneosM(SchedLwt)(struct
  include SchedLwt
  type 'a mvar = 'a Lwt_mvar.t
  let create = Lwt_mvar.create
  let create_empty = Lwt_mvar.create_empty
  let put = Lwt_mvar.put
  let take = Lwt_mvar.take
end)

let (crrm,crrsd) = Benchmark.benchmark (fun () -> ChameneosRR.main n) "Chameneos_M_RR" repeats
let _ = Gc.full_major ()

let (clwtm,clwtsd) = Benchmark.benchmark (fun () -> ChameneosLwt.main n) "Chameneos_Lwt" repeats
let _ = Gc.full_major ()

let (crrlwtm,crrlwtsd) = Benchmark.benchmark (fun () -> ChameneosRRLwt.main n) "Chameneos_Lwt_RR" repeats
let _ = Gc.full_major ()

module SchedThunk = struct
  type 'a t = unit -> 'a
  let (>>=) v f = fun _ -> f (v ()) ()
  let (>>) v f = fun _ -> v (); f ()
  let return v = fun _ -> v
  let fork f = fun _ -> SchedD.fork f
  let yield () = fun _ -> SchedD.yield ()
  let iter_m f l = fun _ -> List.iter (fun v -> f v ()) l
  let map_m f l = fun _ -> List.map (fun v -> f v ()) l
  let run f = SchedD.run f
end

module MVarThunk = struct
  include SchedThunk
  include MVarD
  let put mv v = fun _ -> put mv v
  let take mv = fun _ -> take mv
end

module ChameneosThunk = ChameneosM(SchedThunk)(MVarThunk)

let (ctm,ctsd) = Benchmark.benchmark (fun () -> ChameneosThunk.main n) "Chameneos_Thunk" repeats
let _ = Gc.full_major ()

module TRR = Make_ReifyReflect(SchedThunk)

module SchedThunkRR = struct
  let fork f = TRR.reflect (SchedThunk.fork (TRR.reify f))
  let run f = SchedThunk.run (TRR.reify f)
  let yield () = TRR.reflect (SchedThunk.yield ())
end

module MVarThunkRR = struct
  include MVarThunk
  let put mv v = TRR.reflect (put mv v)
  let take mv = TRR.reflect (take mv)
end

module ChameneosThunkRR = ChameneosD(SchedThunkRR)(MVarThunkRR)

let (ctrrm,ctrrsd) = Benchmark.benchmark (fun () -> ChameneosThunkRR.main n) "Chameneos_Thunk_RR" repeats
let _ = Gc.full_major ()

let _ = Printf.printf "Chameneos: mean=%f sd=%f\n%!" cm csd
let _ = Printf.printf "Chameneos_M: mean=%f sd=%f\n%!" cmm cmsd
let _ = Printf.printf "Chameneos_M_RR: mean=%f sd=%f\n%!" crrm crrsd
let _ = Printf.printf "Chameneos_Lwt: mean=%f sd=%f\n%!" clwtm clwtsd
let _ = Printf.printf "Chameneos_Lwt_RR: mean=%f sd=%f\n%!" crrlwtm crrlwtsd
let _ = Printf.printf "Chameneos_Thunk: mean=%f sd=%f\n%!" ctm ctsd
let _ = Printf.printf "Chameneos_Thunk_RR: mean=%f sd=%f\n%!" ctrrm ctrrsd
