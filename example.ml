open Term
open Notations

let _ = Random.self_init ()

let gen =
  let x =  mk_arg in
  let _y = mk_farg in
  let _acc = mk_facc in

  let rec gen n =
    if n = 0 then
      match Random.int 3 with
      | 0 -> c0
      | 1 -> c1
      | _ -> x
    else
      match Random.int 13 with
      | 0 -> c0
      | 1 -> c1
      | 2 -> x
      | 3 -> if0 (gen (n - 1)) (gen (n-1)) (gen (n - 1))
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

(** Currently the generator is quite naive:
    it generates neither If0 nor Fold *)
let random size =
  let open Randgen in
  let ($$) f x = app f x in
  
  let choose_const = select [c0; c1] in
    
  let choose_var = function
    | `Top -> return mk_arg
    | `Fold -> select [mk_arg; mk_facc; mk_farg] in

  let choose_op1 = select [(~~); shl1; shr1; shr4; shr16] in
  let choose_op2 = select [(&&); (||); ( ** ); (++)] in

  let open Fuel in
  let fueled_gen = 
    fix (fun term env ->
      fuel_choose [
        nullary choose_const;
        nullary (choose_var env);
        unary (term env) (fun e -> choose_op1 $$ e);
        binary (term env) (term env) (fun e1 e2 -> choose_op2 $$ e1 $$ e2);
      ]
    ) in
  
  let rand = Random.get_state () in
  match fueled_gen `Top rand size with
    | None -> failwith "Example.random"
    | Some term -> term rand
