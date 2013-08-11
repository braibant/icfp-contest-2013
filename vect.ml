
type t = int64 array
let compare = Pervasives.compare
(* let size = 256 *)
(* let mk c = Array.create size c  *)
let mk size c = Array.create size c 
(* let zero = mk 0L *)
(* let one = mk 1L *)
let equal x y =
  let n = Array.length x in
  assert (n = Array.length y);
  let rec aux i =
    if i = n then true else x.(i) = y.(i) && aux (i + 1)
  in 
  aux 0
