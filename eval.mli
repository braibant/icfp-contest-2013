val eval : Term.exp -> int64 -> int64
val evalv : Term.exp -> int64 array -> int64 array

val h_evalv : Term.exp -> int64 array array -> int64 array -> int64 array

val eval_with_holes : int64 array -> Term.exp -> int64 -> int64
