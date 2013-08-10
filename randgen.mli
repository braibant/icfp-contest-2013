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

(** 'a gen is a functor *)
val map : ('a -> 'b) -> 'a gen -> 'b gen
val map' : 'a gen -> ('a -> 'b) -> 'b gen

(** 'a gen is applicative *)
val app : ('a -> 'b) gen -> 'a gen -> 'b gen
val app' : 'a gen -> ('a -> 'b) gen -> 'b gen

val pure : ('a -> 'b) -> ('a -> 'b) gen

(** 'a gen is a monad *)
val return : 'a -> 'a gen
val bind : ('a -> 'b gen) -> 'a gen -> 'b gen
val bind' : 'a gen -> ('a -> 'b gen) -> 'b gen
val join : 'a gen gen -> 'a gen

(** parametrized fixpoints *)
val fix : (('a -> 'b gen) -> ('a -> 'b gen)) -> 'a -> 'b gen

(* val cont : ('a -> 'b gen) -> ('b -> 'c gen) -> ('a -> 'c gen) *)

(** Value generators and combinators *)
val split_int : int -> (int * int) gen
val make_char : char -> int -> char gen
val lowercase : char gen
val uppercase : char gen

(** adapted from Kaputt.Generator *)
val unit : unit gen
val make_int : int -> int -> int gen
val string : int gen -> char gen -> string gen
val bool : bool gen

(** input lists must be non-empty *)
val select : 'a list -> 'a gen
val choose : 'a gen list -> 'a gen

(** backtracking generator *)
type 'a backtrack_gen = 'a option gen

val guard : ('a -> bool) -> 'a gen -> 'a backtrack_gen
val cond : bool -> 'a gen -> 'a backtrack_gen
val backtrack : 'a backtrack_gen -> 'a gen

(** fueled generators *)
type 'a fueled = (int -> 'a option) gen

module Fuel : sig
  val map : ('a -> 'b) -> 'a fueled -> 'b fueled
  val map' : 'a fueled -> ('a -> 'b) -> 'b fueled

  val zero : 'a -> 'a fueled
  val tick : 'a fueled -> 'a fueled
  val prod : 'a fueled -> 'b fueled -> (int -> (int * int) gen) -> ('a * 'b) fueled

  (** takes and returns non-empty lists *)
  val list : 'a fueled list -> 'a list fueled

  val fix : (('a -> 'b fueled) -> ('a -> 'b fueled)) -> ('a -> 'b fueled)
end

(** convenience functions for fueled generators *)

val nullary : 'a -> 'a fueled
(** zero *)

val unary : 'a fueled -> ('a -> 'a) -> 'a fueled
(** tick *)

val binary : 'a fueled -> 'b fueled -> ('a -> 'b -> 'c) -> 'c fueled
(** tick and split_int *)

val fuel_choose : 'a gen fueled list -> 'a gen fueled
