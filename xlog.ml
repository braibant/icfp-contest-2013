module IMap = Map.Make (struct type t = int64 let compare = Int64.compare end)

include IMap

type log = 
  int64 IMap.t * Term.exp list
    
let empty = IMap.empty, []
 
let save n t = 
  let o = open_out n in 
  Marshal.to_channel o t [];
  close_out o
    
let restore n = 
  let i = open_in n in
  let x = (Marshal.from_channel i : log) in 
  close_in i; x
    
let logv ((t,g): log) keys values =
  let n = Array.length keys in 
  assert (n = Array.length values);
  let t = ref t in 
  for i = 0 to n - 1 do
    t := add keys.(i) values.(i) !t
  done; 
  !t,g
    
let log ((t,g): log) key value =
  add key value t,g
    
let guess (t,g) guess = 
  t, guess :: g
    
let print ((t,g): log)= 
  let open PPrint in 
  let int64 n = string (Printf.sprintf "0x%Lx" n) in
  let t = group (separate_map (semi ^^ break 1) (fun (x, y) -> group (parens (int64 x ^/^ int64 y))) (IMap.bindings t)) in
  let g = group (separate_map (semi) Print.doc_exp g) in 
  group (prefix 2 1 (string "(* evals *)") t) ^^ hardline ^^
    group (prefix 2 1 (string "(* guesses *)") g) ^^ hardline 
      
let print_short (t,g) =
  let open PPrint in 
  let t = List.length (IMap.bindings t) in 
  let g = List.length g in 
  let int n= string (Printf.sprintf "%i" n ) in 
  group (prefix 2 1 (string "(* evals *)") (int t)) ^^ hardline ^^
    group (prefix 2 1 (string "(* guesses *)") (int g)) ^^ hardline 


