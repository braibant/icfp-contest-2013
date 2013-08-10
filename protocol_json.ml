open Protocol
open Yojson.Basic
open Yojson.Basic.Util

let json_of_int64 n =
  `String (Printf.sprintf "0x%Lx" n)
let int64_of_json j =
  try Scanf.sscanf (to_string j) "0x%Lx" (fun n -> n)
  with _ -> failwith "int64_of_json"

let json_of_int64_array arr =
  `List (List.map json_of_int64 (Array.to_list arr))

let int64_array_of_json json =
  Array.of_list (List.map int64_of_json (Yojson.Basic.Util.to_list json))

(* this is necessary because of the following 
   where easyChairId is supposed to be int
   {
     "easyChairId": "243",
     "contestScore": 0,
    "lightningScore": 0,
    "trainingScore": 0,
    "mismatches": 0,
    "numRequests": 32,
    "requestWindow": { "resetsIn": 9.478999999999999, "amount": 1, "limit": 5 },
    "cpuWindow": { "resetsIn": 49.479, "amount": 0.8, "limit": 20 },
    "cpuTotalTime": 24.8
   }
*)
let to_int_robust json =
  try to_int json
  with exn ->
    try int_of_string (to_string json)
    with _ -> raise exn

let ($) f x = f x
let protect name f = fun json ->
  try f json
  with exn -> 
    Printf.eprintf "Error when parsing the following input:";
    pretty_to_channel stderr json;
    flush stderr;
    raise exn

(** Problem *)

let problem_of_json = protect "problem" $ fun json ->
  let open Problem.Response in
  let get f = member f json in
  let id = to_string (get "id") in
  let size = to_int (get "size") in
  let operators = convert_each to_string (get "operators") in
  let solved = to_bool_option (get "solved") in
  let timeleft = to_number_option (get "timeLeft") in
  Problem.({json; id; size; operators; solved; timeleft})


(** Eval *)

let json_of_eval request : json =
  let open Eval.Request in
  let arguments = "arguments", json_of_int64_array request.arguments in
  match request.name with
  | `Id id -> `Assoc ["id", `String id; arguments ]
  | `Program p -> `Assoc ["program", `String p; arguments ]

let eval_of_json = protect "eval" $ fun json ->
  let open Eval.Response in
  let get f = member f json in
  match to_string (get "status") with
                                       (* yes, this strange
                                          final 's'
                                          is in the doc *)
    | "ok" -> `Ok (int64_array_of_json (get "outputs"))
    | "error" -> `Error (to_string (get "message"))
    | status ->
      failwith (Printf.sprintf "eval_of_json: unknown status %S" status)

(** Guesses *)

let json_of_guess request : json =
  let open Guess.Request in
  `Assoc ["id", `String request.id; "program", `String request.program]

let guess_of_json = protect "guess" $ fun json ->
  let open Guess.Response in
  let get f = member f json in
  let lightning = to_bool_option (get "lightning") in
  let status = match to_string (get "status") with
    | "win" -> `Win
    | "error" -> `Error (to_string (get "message"))
    | "mismatch" ->
      let mismatch = match (get "values") with
        | `List [a; b; c] ->
          {
            input = int64_of_json a;
            challenge_result = int64_of_json b;
            guess_result = int64_of_json c;
          }
        | values ->
          failwith
            (Printf.sprintf "mismatch_of_json: invalid mismatch information %S"
               (to_string values))
      in
      `Mismatch mismatch
    | status ->
      failwith
        (Printf.sprintf "guess_of_json: unknown status %S" status)
  in { lightning; status }


(** Training *)

let json_of_training request : json =
  let maybe m f = match m with
    | None -> []
    | Some x -> [f x] in
  let open Training.Request in
  `Assoc (
    maybe request.size (fun s -> "size", `Int s)
    @ maybe request.operators (fun s ->
      "operators", `List (List.map (fun o -> `String o) s))
  )

let training_of_json = protect "training" $ fun json ->
  let open Training.Response in
  let get f = member f json in
  let challenge = to_string (get "challenge") in
  let id = to_string (get "id") in
  let size = to_int (get "size") in
  let operators = convert_each to_string (get "operators") in
  { json; challenge; id; size; operators }


(** Status *)

let status_of_json = protect "status" $ fun json ->
  let open Status.Response in
  let get f = member f json in
  let easy_chair_id = to_int_robust (get "easyChairId") in
  let contest_score = to_int (get "contestScore") in
  let lightning_score = to_int (get "lightningScore") in
  let training_score = to_int (get "trainingScore") in
  let mismatches = to_int (get "mismatches") in
  let num_requests = to_int (get "numRequests") in
  let window_of_json conv json =
    to_number (member "resetsIn" json),
    conv (member "amount" json),
    conv (member "limit" json) in
  let request_window =
    let (request_resets_in, request_amount, request_limit) =
      window_of_json to_int (get "requestWindow") in
    { request_resets_in; request_amount; request_limit } in
  let cpu_window =
    let (cpu_resets_in, cpu_amount, cpu_limit) =
      window_of_json to_number (get "cpuWindow") in
    { cpu_resets_in; cpu_amount; cpu_limit } in
  let cpu_total_time = to_number (get "cpuTotalTime") in
  {
    easy_chair_id;
    contest_score;
    lightning_score;
    training_score;
    mismatches;
    num_requests;
    request_window;
    cpu_window;
    cpu_total_time;
  }
