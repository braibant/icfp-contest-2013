
type t = int64 array
let compare = Pervasives.compare
let size = 256
let mk c = Array.create size c 
let zero = mk 0L
let one = mk 1L
let (equal) = (=)
