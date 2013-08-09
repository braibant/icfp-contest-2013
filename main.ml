open Net
open Protocol

module Oracle(X: sig val id : string end) = struct
  let eval v =
    let open Eval in 
    match send_eval (Request.({name = `Id X.id; arguments = v})) with 
    | `Eval_body (`Ok r) -> r
    | _ -> invalid_arg "eval"

  let guess t =
    let open Guess in 
      match send_guess (Request.({id = X.id; program = t})) with 
      | `Guess_body ({Response.status=`Win}) -> Loop.Equiv 
      | `Guess_body ({Response.status=`Mismatch m}) -> Loop.Discr (m.Response.input, m.Response.challenge_result )
      | _ -> invalid_arg "eval"

  let reveal () = assert false 
end
      

let train () =
  let open Training in 
  match send_training ({Protocol.Training.Request.size = Some 4; Protocol.Training.Request.operators = Some []}) with
  | `Training_body pb ->
    let secret = Parser.prog_of_string (pb.Response.challenge) in 
    Printf.printf "start (size of the secret:%i)\n%!" (Term.size secret);
    Print.print_exp_nl secret;
    let module Oracle = Oracle(struct let secret = secret end) in
    let module Params = struct let n = Term.size secret let ops = Generator.operators secret end in 
    let module Loop = FState(Params)(Oracle) in 
  if !Config.interactive_mode 
  then Loop.iloop ()
  else Loop.loop ()

let _ = match send_status () with
  | `Status_body b -> Printf.printf "youhou"
  | _ -> assert false
