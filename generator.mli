open Term

module OSet : Set.S with type elt = op

val ops_from_list : op list -> OSet.t
val all_ops : OSet.t

val operators : Term.exp -> OSet.t
val generate : ?force_fold:bool -> int -> ?exact:bool -> OSet.t -> Term.exp list
val generate_tfold : int -> ?exact:bool -> OSet.t -> Term.exp list
val generate_novar : ?force_fold:bool -> int -> ?exact:bool -> OSet.t -> Term.exp list
val generate_constants : ?force_fold:bool -> int -> ?exact:bool -> OSet.t -> Int64.t list
val generate_context : int -> OSet.t -> Term.exp list -> Term.exp list

