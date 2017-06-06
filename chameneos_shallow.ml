module S : sig
  type 'a t = unit -> 'a
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  val (>>) : 'a t -> 'b t -> 'b t
  val return : 'a -> 'a t
  val fork : unit t -> unit t
  val yield : unit t
  val iter_p : ('a -> unit t) -> 'a list -> unit t
  val map_p : ('a -> 'b t) -> 'a list -> 'b list t
  val run : unit t -> unit
end = struct
  type 'a t = unit -> 'a
  let (>>=) v f = fun _ -> f (v ()) ()
  let (>>) v f = fun _ -> v (); f ()
  let return v = fun _ -> v
  let fork f = fun _ -> perform (Sched.Fork f)
  let yield = fun _ -> perform Sched.Yield
  let iter_p f l = fun _ -> List.iter (fun v -> f v ()) l
  let map_p f l = fun _ -> List.map (fun v -> f v ()) l
  let run f = Sched.run f
end

open S

module MVar : sig
  type 'a t
  val create : 'a -> 'a t
  val create_empty : unit -> 'a t
  val put : 'a t -> 'a -> unit S.t
  val take : 'a t -> 'a S.t
end = struct
  module M = MVar.Make (Sched)
  include M
  let put mv v = fun _ -> M.put mv v
  let take mv = fun _ -> M.take mv
end

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

type mp =
| Nobody of int
| Somebody of int * chameneos * chameneos MVar.t

let arrive (mpv : mp MVar.t) (finish : (int * int) MVar.t) (ch : chameneos) =
  let waker = MVar.create_empty () in
  let inc x i = if (x == ch) then i+1 else i in
  let rec go t b =
    MVar.take mpv >>= fun w ->
    match w with
    | Nobody 0 ->
        MVar.put mpv w >>
        MVar.put finish (t,b)
    | Nobody q ->
        MVar.put mpv (Somebody (q, ch, waker)) >>
        MVar.take waker >>= fun w' ->
        go (t+1) @@ inc w' b
    | Somebody (q, ch', waker') ->
        MVar.put mpv (Nobody (q - 1)) >>
        let c'' = Color.complement !ch !ch' in
        let () = ch := c'' in
        let () = ch' := c'' in
        MVar.put waker' ch  >>
        go (t+1) @@ inc ch' b
  in go 0 0

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
  printf "\n";
;;

let rec tabulate' acc f = function
  | 0 -> acc
  | n -> tabulate' (f()::acc) f (n-1)
;;

let tabulate f n = List.rev @@ tabulate' [] f n

let work colors n =
  let () = List.iter colors ~f:(fun c ->
              printf " %s" (Color.to_string c)); printf "\n" in
  let fs = tabulate MVar.create_empty (List.length colors) in
  let mpv = MVar.create (Nobody n) in
  let chams = List.map ~f:(fun c -> ref c) colors in
  let comb = List.combine fs chams in
  iter_p (fun (fin,ch) -> fork (arrive mpv fin ch)) comb >>
  map_p MVar.take fs >>= fun ns ->
  let () = List.iter ~f:(fun (n,b) -> print_int n; spell_int b; printf "\n") ns in
  let sum_meets = List.fold_left ~init:0 ~f:(fun acc (n,_) -> n+acc) ns in
  let () = spell_int sum_meets in
  let () = printf "\n" in
  return ()

let main =
  let n =
    try
      int_of_string (Sys.argv.(1))
    with
    | _ -> 600
  in
  print_complements ();
  let module C = Color in
  work [ C.Blue; C.Red; C.Yellow ] n >>= fun () ->
  printf "\n";
  work [ C.Blue; C.Red; C.Yellow; C.Red; C.Yellow;
          C.Blue; C.Red; C.Yellow; C.Red; C.Blue ] n >>= fun () ->
  printf "\n"; return ()

let () = run main
