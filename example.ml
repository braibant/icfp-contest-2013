open Term
open Notations 
 
let _ = Random.self_init ()

let gen =
  let x =  mk_arg in 
  let y = mk_farg in 
  let acc = mk_facc in 

  let rec gen n = 
    if n = 0 then  
      match Random.int 3 with
      | 0 -> C0 
      | 1 -> C1 
      | _ -> x
    else
      match Random.int 13 with
      | 0 -> C0 
      | 1 -> C1 
      | 2 -> x
      | 3 -> If0 (gen (n - 1), gen (n-1), gen (n - 1))
      | 4 -> ~~ (gen (n-1))
      | 5 -> shl1 (gen (n - 1))
      | 6 -> shr1 (gen (n - 1))
      | 7 -> shr4 (gen (n - 1))
      | 8 -> shr16 (gen (n - 1))
      | 9 -> gen (n - 1) && gen (n-1)
      | 10 -> gen (n - 1) || gen (n-1)
      | 11 -> gen (n - 1) ++ gen (n-1)
      | _ -> gen (n - 1) ** gen (n-1)
  in
  gen 

let examples = Array.init 50 (fun _ -> gen 10)

let _ =
  if not !Sys.interactive then
    Array.iter (fun x -> Print.(print (doc_exp x ^/^ hardline))) examples

