open Term

val generate : ?force_fold:bool -> int -> ?exact:bool -> OSet.t -> Term.exp array
val generate_tfold : int -> ?exact:bool -> OSet.t -> Term.exp array
val generate_novar : ?force_fold:bool -> int -> ?exact:bool -> OSet.t -> Term.exp array
val generate_constants : ?force_fold:bool -> int -> ?exact:bool -> OSet.t -> Int64.t array
val generate_context : int -> OSet.t -> Term.exp list -> Term.exp array
