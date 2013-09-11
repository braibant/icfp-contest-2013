
module S = Term.OSet 

module Lattice = struct

  let bottom = S.empty 
  let top = Term.all_ops
  let join a b = S.union a b

  module H = Hashtbl.Make (struct type t = S.t let hash = Hashtbl.hash let equal = S.equal end)
  
  let uids = H.create 53 
  let values = Hashtbl.create 53 
  let next = ref 0 

  let add v =
    try H.find uids v
    with 
      Not_found ->
	let r = !next in 
	H.add uids v r;
	Hashtbl.add values r v;
	incr next;
	r

  let uid v = H.find uids v
  let set uid = Hashtbl.find values uid
end


let op1 op l =
  match op with
  | Term.Op1o op -> List.rev_map (fun x -> Term.Notations.op1 op x) l
  | _ -> [] 

let op2 op l l' =
  match op with
  | Term.Op2o op -> 
    let rec aux acc = function
      | [] -> acc
      | t1::q1 -> 
	let acc = List.fold_left (fun acc t2 -> Term.Notations.op2 op t1 t2 :: acc) acc l' in
	aux acc q1
    in
    aux [] l
  | _ -> []

let ops1 ops l =
  S.fold (fun op acc -> List.rev_append (op1 op l) acc ) ops []

let ops2 ops l1 l2  =
  S.fold (fun op acc -> List.rev_append (op2 op l1 l2) acc) ops []

module State = struct
  module H = Hashtbl.Make (struct type t = int * S.t let equal (a,b) (c,d) = a == c && S.equal b d let hash = Hashtbl.hash end)
    
  let state = 
    try 
      Printf.printf "loading generator state...\t%!";
      let i = open_in "gen.state" in 
      let res = Marshal.from_channel i in
      close_in i;
      Printf.printf "done! (size: %i)\n%!" (H.length res);
      res
    with Sys_error _  -> H.create 30

  let save () : unit =
    Printf.printf "saving a generator state of size %i...\t%!" (H.length state);
    let o = open_out "gen.state" in
    Marshal.to_channel o state [];
    close_out o;
    Printf.printf "done!\n%!"

  let _ = at_exit save

  let atoms = 
    let open Term.Notations in 
    [c0; c1; mk_arg]

  let get n ops = if n = 1 || S.is_empty ops then atoms else H.find state (n,ops)
  let set n ops r = H.add state (n,ops) r
end

let print_ops ops = 
  S.fold (fun op acc -> Printf.sprintf "%s; %s" (Term.string_of_op op) acc) ops ""
  
let rec populate n ops f = 
  Printf.printf "populate %i %s\n%!" n (print_ops ops);
  try let l = State.get n ops in List.iter f l; l
  with
    Not_found ->
      Printf.printf "populating...\n%!";
      let r = ref [] in
      let add l = r := List.rev_append l !r in 
      add (ops1 ops (populate (n-1) ops f));
      for k = 1 to n/2  do
	add (ops2 ops (populate k ops f) (populate (n-k) ops f));
      done;

      S.iter (fun op -> 
	let ops' = (S.remove op ops) in
	add (op1 op (populate (n-1) ops' f));
	for k = 1 to n/2  do
	  add (op2 op (populate k ops' f) (populate (n-k) ops' f));
	done
      ) ops;

       State.set n ops !r;
       List.iter f !r;
       !r
  
  

    
  
