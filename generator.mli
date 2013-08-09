open Term

type op = If0o | Foldo | Op1o of op1 | Op2o of op2

module OSet : Set.S with type elt = op

val ops_from_list : op list -> OSet.t
val all_ops : OSet.t

val operators : Term.exp -> OSet.t
val generate : ?filter:bool -> int -> ?exact:bool -> OSet.t -> Term.exp list
val generate_tfold : ?filter:bool -> int -> ?exact:bool -> OSet.t -> Term.exp list
val generate_novar : ?filter:bool -> int -> ?exact:bool -> OSet.t -> Term.exp list
val generate_constants : ?filter:bool -> int -> ?exact:bool -> OSet.t -> Int64.t list
