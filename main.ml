(** Offline training, with a randomly-generated term *)

module OfflineOracle(S: sig val secret : Term.exp end)  = struct
  include S
  open Loop
    
  let eval x = Eval.evalv secret x  

  let discriminating n = Array.init n (fun x -> Term.rnd64 ())

  let guess p' =
    let confidence = 10000 in
    let tests = discriminating confidence in
    let rec aux i =
      if i = confidence - 1 then Equiv
      else
	let x = tests.(i) in
	if Eval.eval p' x = Eval.eval secret x
	then aux (succ i)
	else Discr (x,Eval.eval secret x)
    in aux 0

  let reveal () = secret
end

let train_offline () =
  let secret = Example.random !Config.problem_size in
  Printf.printf "start (size of the secret:%i)\n%!" (Term.size secret);
  let module Oracle = OfflineOracle(struct let secret = secret end) in
  let module Params = struct
    let n = Term.size secret
    let ops = Generator.operators secret
  end in
  let module Loop = Loop.FState(Params)(Oracle) in
  if !Config.interactive_mode
  then Loop.iloop ()
  else Loop.loop ()


(** Online training, on the server *)
module OnlineOracle(X: sig val id : string val secret : Term.exp end) = struct
  let eval v =
    let open Protocol.Eval in 
    match Net.send_eval (Request.({name = `Id X.id; arguments = v})) with 
    | `Eval_body (`Ok r) -> r
    | _ -> invalid_arg "eval"

  let guess t =
    let program =  Print.print_program t in 
    let open Protocol.Guess in 
      match Net.send_guess (Request.({id = X.id; program})) with 
      | `Guess_body {Response.status=`Win} -> Loop.Equiv 
      | `Guess_body {Response.status=`Mismatch m} -> Loop.Discr (m.Response.input, m.Response.challenge_result )
      | `Guess_body {Response.status=`Error msg} ->
	failwith (Printf.sprintf "guess: received error message from server\nmsg: %s\nprogram:%s\n" msg program)
      | _ -> invalid_arg "guess"

  let reveal () = X.secret
end

(* this is the main handler for training problems. I tested it in
   interactive mode, but not yet in automated mode [loop] *)
let train_online () =
  let open Protocol.Training in 
  match Net.send_training ({
    Request.size = Some !Config.problem_size;
    Request.operators = Some []
  })
  with
  | `Training_body pb ->
    let secret = Parser.prog_of_string (pb.Response.challenge) in 
    Printf.printf "start (size of the secret:%i)\n%!" (Term.size secret);
    Print.print_exp_nl secret;
    let module Oracle = OnlineOracle(struct
      let id= pb.Response.id
      let secret = secret
    end) in
    let module Params = struct
      let n = Term.size secret
      let ops = Generator.operators secret
    end in 
    let module Loop = Loop.FState(Params)(Oracle) in 
    if !Config.interactive_mode 
    then Loop.iloop ()
    else Loop.loop ()
  | _ -> assert false 




(** Setting up usage *)

let _ =
  Arg.parse Config.args (fun rest -> ()) "ICFP contest 2013 prototype";
  match !Config.source with
    | None ->
      Print.print PPrint.(flow (break 1) (words
        "no source of challenge has been selected; \
         you must pass either --train-offline, --train-online \
         or, when it will be supported, the --real-stuff."));
      print_newline ();
    | Some Config.Real_stuff ->
      print_endline "This mode is not yet supported."
    | Some Config.Train_offline ->
      train_offline ()
    | Some Config.Train_online ->
      train_online ()
