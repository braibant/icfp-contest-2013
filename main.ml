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

(** real world play *)
type problem_data = {
  id : string;
  operators : Generator.OSet.t;
  size : int;
}

let problem_data p = {
  id = p.Protocol.Problem.Response.id;
  operators =
    (* we must double-check the semantics of 'operators' in the protocol *)
    Generator.all_ops;
    (* Generator.ops_from_list *)
    (*   (List.map Term.op_of_string p.Protocol.Problem.Response.operators); *)
  size = p.Protocol.Problem.Response.size;
}

let play_online problem secret =
  let module Oracle = OnlineOracle(struct
    let id = problem.id
    let secret = secret
  end) in
  let module Params = struct
    let n = problem.size
    let ops = problem.operators
  end in
  let module Loop = Loop.FState(Params)(Oracle) in
  if !Config.interactive_mode 
  then Loop.iloop ()
  else Loop.loop ()

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
    let problem = {
      id = pb.Response.id;
      size = Term.size secret;
      operators = Generator.operators secret;
    } in
    play_online problem (Some secret)
  | #Net.unexpected as other ->
    invalid_arg (Printf.sprintf "train: %s" (Net.str_of_return other))

(** manipulating the problem list *)

let problems = lazy begin
  let json = Yojson.Basic.from_file !Config.problems_file in
  Yojson.Basic.Util.convert_each Protocol_json.problem_of_json json
end

let unsolved_problems = lazy begin
  let open Protocol.Problem.Response in
  List.filter (fun prob -> prob.solved = None) (Lazy.force problems)
end

let problem_difficulty prob =
  let open Protocol.Problem.Response in
  float (List.length prob.operators) ** float prob.size

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

let print_problem problem =
  Yojson.Basic.pretty_to_channel stdout
    problem.Protocol.Problem.Response.json;
  print_newline ()

let show_problem id =
  Printf.printf "Showing problem %s.\n%!" id;
  try
    let problem = problem_of_id id in
    print_problem problem
  with Not_found ->
    Printf.eprintf "No problem with id %s.\n%!" id

let list_problems () =
  if not !Config.sync_problem_list then sync_problem_list ();
  let rec take n = function
    | [] -> []
    | hd::tl -> if n = 0 then [] else hd :: take (n - 1) tl in
  let problems_sorted =
    List.sort (fun pa pb ->
      compare (problem_difficulty pa) (problem_difficulty pb))
      (Lazy.force unsolved_problems)
  in
  let easy = take 10 problems_sorted in
  print_endline "10 easy problems:";
  List.iter print_problem easy;
  print_newline ()


let show_status () =
  match Net.send_status_raw () with
    | `Status_json json ->
      Yojson.Basic.pretty_to_channel stdout json;
      print_newline ();
      ()
    | #Net.unexpected as other ->
      invalid_arg (Printf.sprintf "status: %s" (Net.str_of_return other))

(** Setting up usage *)

let _ =
  (* this test allows to also load main.cmo in the toplevel;
     please don't use observable global effect outside it *)
  if not !Sys.interactive then begin
    Arg.parse Config.args (fun rest -> ()) "ICFP contest 2013 prototype";

    if !Config.show_status then begin
      show_status ();
      exit 0;
    end;

    if !Config.sync_problem_list then sync_problem_list ();
    begin match !Config.show_problem with
      | None -> ()
      | Some id -> show_problem id
    end;
    if !Config.list_problems then list_problems ();

    print_newline ();

    match !Config.source with
      | None -> ()
      | Some Config.Train_offline ->
        train_offline ()
      | Some Config.Train_online ->
        train_online ()
      | Some (Config.Single_problem id) ->
        begin match
          (try Some (problem_of_id id) with Not_found -> None)
        with
          | None ->
            Printf.eprintf
              "The local problem list knows of no problem of id %s. \
               Aborting for your own good.\n%!" id
          | Some prob ->
            Printf.printf
              "You want to play in %s mode against the following problem:\n"
              (if !Config.interactive_mode
               then "interactive" else "non-interactive");
            print_problem prob;
            print_endline "Confirm? y/n";
            begin match read_line () with
              | "y" -> play_online (problem_data prob) None
              | "n" -> ()
              | other ->
                Printf.printf "unknown answer %S, aborting.\n%!" other
            end
        end
  end
