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

  let reveal () = Some secret
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
module OnlineOracle(X: sig val id : string val secret : Term.exp option end) =
struct
  let eval v =
    let open Protocol.Eval in 
    match Net.send_eval (Request.({name = `Id X.id; arguments = v})) with 
    | `Eval_body (`Ok r) -> r
    | `Eval_body (`Error error) ->
      invalid_arg (Printf.sprintf "eval error: %s" error)
    | #Net.unexpected as other ->
      invalid_arg (Printf.sprintf "eval: %s" (Net.str_of_return other))

  let guess t =
    let program =  Print.print_program t in 
    let open Protocol.Guess in 
      match Net.send_guess (Request.({id = X.id; program})) with 
      | `Guess_body {Response.status=`Win} -> Loop.Equiv 
      | `Guess_body {Response.status=`Mismatch m} ->
        Loop.Discr (m.Response.input, m.Response.challenge_result )
      | `Guess_body {Response.status=`Error msg} ->
        let msg =
          Printf.sprintf
            "guess: received error message from server\nmsg: %s\nprogram:%s\n"
            msg program
        in failwith msg
      | #Net.unexpected as other ->
        invalid_arg (Printf.sprintf "guess: %s" (Net.str_of_return other))

  let reveal () = X.secret
end

(* this is the main handler for training problems.  *)
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
      let secret = Some secret
    end) in
    let module Params = struct
      let n = Term.size secret
      let ops = Generator.operators secret
    end in 
    let module Loop = Loop.FState(Params)(Oracle) in 
    if !Config.interactive_mode 
    then Loop.iloop ()
    else Loop.loop ()
  | #Net.unexpected as other ->
    invalid_arg (Printf.sprintf "train: %s" (Net.str_of_return other))

(** real world play *)

let play_online problem =
  let open Protocol.Problem.Response in
  let module Oracle = OnlineOracle(struct
    let id = problem.id
    let secret = None
  end) in
  let module Params = struct
    let n = problem.size
    let ops =
      Generator.ops_from_list
        (List.map Term.op_of_string problem.operators)
  end in
  let module Loop = Loop.FState(Params)(Oracle) in
  if !Config.interactive_mode 
  then Loop.iloop ()
  else Loop.loop ()

(** manipulating the problem list *)

let problems = lazy begin
  let json = Yojson.Basic.from_file !Config.problems_file in
  Yojson.Basic.Util.convert_each Protocol_json.problem_of_json json
end

let problem_of_id id =
  let open Protocol.Problem.Response in
  List.find (fun prob -> prob.id = id) (Lazy.force problems)

let sync_problem_list () =
  Printf.printf "Syncing problem list\n%!";
  match Net.send_myproblems_raw () with
    | #Net.unexpected as error ->
      Printf.eprintf "sync_problem_list net error: %s.\n%!"
        (Net.str_of_return error)
    | `Problem_json json ->
      try
        Yojson.Basic.to_file ~std:true !Config.problems_file json;
        Printf.printf "Problem list synced\n%!";
      with exn ->
        Printf.eprintf "sync_problem_list sys error: %s.\n%!"
          (Printexc.to_string exn)

let show_problem id =
  Printf.printf "Showing problem %s.\n%!" id;
  try
    let problem = problem_of_id id in
    Yojson.Basic.pretty_to_channel stdout
      problem.Protocol.Problem.Response.json;
    print_newline ()
  with Not_found ->
    Printf.eprintf "No problem with id %s.\n%!" id


let list_problems () = failwith "not implemented yet"

(** Setting up usage *)

let _ =
  (* this test allows to also load main.cmo in the toplevel;
     please don't use observable global effect outside it *)
  if not !Sys.interactive then begin
    Arg.parse Config.args (fun rest -> ()) "ICFP contest 2013 prototype";

    if !Config.sync_problem_list then sync_problem_list ();
    begin match !Config.show_problem with
      | None -> ()
      | Some id -> show_problem id
    end;
    if !Config.list_problems then list_problems ();

    print_newline ();

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
  end
