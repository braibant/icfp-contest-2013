(*
 * Generator -- a combinator library to generate random values
 * Copyright (C) 2008-2012 Xavier Clerc
 *               2013      Gabriel Scherer
 *
 * This library evolved from the Generator module of Xavier Clerc's
 * Kaputt library: http://kaputt.x9c.fr/
 *
 * This library is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License <http://www.gnu.org/licences> for more
 * details.
 *)

type random = Random.State.t
type 'a gen = random -> 'a

(** 'a gen is a monad *)
let map f gen = fun rand -> f (gen rand)
let map' gen f = map f gen

let app f gen = fun rand -> f rand (gen rand)
let app' gen f = app f gen

let pure f = fun _ -> f

let return x = fun _ -> x
let bind f gen = fun rand -> f (gen rand) rand
let bind' gen f = bind f gen
let join gen = fun rand -> gen rand rand

let rec fix derec_gen param =
  fun rand -> derec_gen (fix derec_gen) param rand

let prod g1 g2 =
  fun rand -> 
    let v1 = g1 rand in
    let v2 = g2 rand in
    (v1, v2)

(** Value generators *)
let unit r = ()
let bool r = Random.State.bool r
let make_int a b r = a + Random.State.int r (b - a)

let split_int n r =
  let k = Random.State.int r (n + 1) in
  (k, n - k)

let make_char start len r =
  let n = Random.State.int r len in
  char_of_int (n + int_of_char start)
let lowercase = make_char 'a' 26
let uppercase = make_char 'A' 26
let digit = make_char '0' 10

let string int char r =
  let len = int r in
  let res = String.create len in
  for i = 0 to len - 1 do res.[i] <- char r done;
  res

let select li r =
  let len = List.length li in
  List.nth li (Random.State.int r len)

let choose li = join (select li)


(** backtracking operator *)
type 'a backtrack_gen = 'a option gen

let guard p gen r =
  let x = gen r in
  if p x then Some x else None

let cond p gen r =
  (* it is important not to call (gen r) if 'p' is false, as this
     function may be used to guard cases where the random generator
     would fail on its input (e.g. a negative number passed to
     Random.State.int) *)
  if p then Some (gen r) else None

let rec backtrack gen r = match gen r with
  | None -> backtrack gen r
  | Some result -> result


(** fueled generators *)
type 'a fueled = (int -> 'a option) gen

module Fuel = struct
  let map f gen random fuel =
    match gen random fuel with
      | None -> None
      | Some x -> Some (f x)
  let map' f gen = map gen f

  let zero v _random = function
    | 0 -> Some v
    | _ -> None

  let tick gen random fuel =
    let fuel = fuel - 1 in
    if fuel < 0 then None
    else gen random fuel

  let prod gen1 gen2 split = fun random fuel ->
    let fuel1, fuel2 = split fuel random in
    match gen1 random fuel1, gen2 random fuel2 with
      | None, _ | _, None -> None
      | Some v1, Some v2 -> Some (v1, v2)

  (** takes and returns non-empty lists *)
  let list li random fuel =
    let project gen =
      match gen random fuel with
        | None -> []
        | Some v -> [v] in
    match List.flatten (List.map project li) with
      | [] -> None
      | _::_ as li -> Some li 

  let rec fix derec_gen param =
    fun random fuel -> derec_gen (fix derec_gen) param random fuel
end

let nullary v = Fuel.zero v
let unary gen f = Fuel.(map f (tick gen))
let binary gen1 gen2 merge =
  let open Fuel in
  map'
    (tick (prod gen1 gen2 split_int))
    (fun (v1, v2) -> merge v1 v2)

let fuel_choose li =
  let open Fuel in
  map' (list li) (fun li' -> join (select li'))
