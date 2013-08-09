open Net
open Protocol

module Oracle(X: sig val id : string val secret : Term.exp end) = struct
  let eval v =
    let open Eval in 
    match send_eval (Request.({name = `Id X.id; arguments = v})) with 
    | `Eval_body (`Ok r) -> r
    | _ -> invalid_arg "eval"

  let guess t =
    let program =  Print.print_program t in 
    let open Guess in 
      match send_guess (Request.({id = X.id; program})) with 
      | `Guess_body {Response.status=`Win} -> Loop.Equiv 
      | `Guess_body {Response.status=`Mismatch m} -> Loop.Discr (m.Response.input, m.Response.challenge_result )
      | `Guess_body {Response.status=`Error msg} ->
	failwith (Printf.sprintf "guess: received error message from server\nmsg: %s\nprogram:%s\n" msg program)
      | _ -> invalid_arg "guess"

  let reveal () = X.secret
end
      

(* this is the main handler for training problems. I tested it in
   interactive mode, but not yet in automated mode [loop] *)
let train () =
  let open Training in 
  match send_training ({Protocol.Training.Request.size = Some !Config.problem_size; Protocol.Training.Request.operators = Some []}) with
  | `Training_body pb ->
    let secret = Parser.prog_of_string (pb.Response.challenge) in 
    Printf.printf "start (size of the secret:%i)\n%!" (Term.size secret);
    Print.print_exp_nl secret;
    let module Oracle = Oracle(struct let id= pb.Response.id let secret = secret end) in
    let module Params = struct let n = Term.size secret let ops = Generator.operators secret end in 
    let module Loop = Loop.FState(Params)(Oracle) in 
    if !Config.interactive_mode 
    then Loop.iloop ()
    else Loop.loop ()
  | _ -> assert false 

let _ =
  Arg.parse Config.args (fun rest -> ()) "usage";
  train ()