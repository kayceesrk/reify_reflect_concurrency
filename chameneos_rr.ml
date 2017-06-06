module List = ListLabels
module String = StringLabels
open Printf

module RR = Reify_reflect.Make(Sched_monad)

module Sched = struct
  let fork f = RR.reflect (Sched_monad.fork (RR.reify f))
  let run f = Sched_monad.run (RR.reify f)
  let yield () = RR.reflect (Sched_monad.yield)
end

module MVar = struct
  include MVar_monad
  let put mv v = RR.reflect (MVar_monad.put mv v)
  let take mv = RR.reflect (MVar_monad.take mv)
end

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

let rec break n = if n = 0 then () else break (n-1)

let arrive (mpv : mp MVar.t) (finish : (int * int) MVar.t) (ch : chameneos) =
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
        Sched.yield ();
        MVar.put mpv (Nobody (q - 1));
        let c'' = Color.complement !ch !ch' in
        ch := c'';
        ch' := c'';
        MVar.put waker' ch;
        break 0;
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
  let () = List.iter2 ~f:(fun fin ch ->
              Sched.fork (fun () -> arrive mpv fin ch)) fs chams in
  let ns = List.map ~f:MVar.take fs in
  let () = List.iter ~f:(fun (n,b) -> print_int n; spell_int b; printf "\n") ns in
  let sum_meets = List.fold_left ~init:0 ~f:(fun acc (n,_) -> n+acc) ns in
  spell_int sum_meets; printf "\n"

let main () =
  let n =
    try
      int_of_string (Sys.argv.(1))
    with
    | _ -> 600
  in
  print_complements ();
  let module C = Color in
  work [ C.Blue; C.Red; C.Yellow ] n;
  printf "\n";
  work [ C.Blue; C.Red; C.Yellow; C.Red; C.Yellow; C.Blue; C.Red; C.Yellow; C.Red; C.Blue ] n;
  printf "\n"

let () = Sched.run main
