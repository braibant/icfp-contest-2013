open Http_client

let name = function
  | `Problem -> "myproblems"
  | `Eval -> "eval"
  | `Guess -> "guess"
  | `Training -> "train"
  | `Status -> "status"

let addr name =
  Printf.sprintf "http://icfpc2013.cloudapp.net/%s?auth=%svpsH1H"
    name !(Config.auth)

let response call =
  let pipeline = new pipeline in
  pipeline # add call;
  pipeline # run ();
  call

let json_body name call =
  let body = call # response_body # value in
  Yojson.Basic.from_string ~fname:name body

let unknown_code name n =
  failwith (Printf.sprintf "call %S: unknown status code %d" name n)

let send_myproblems () =
  let name = name `Problem in
  let call = new post_raw (addr name) "" in
  match (response call) # response_status_code with
    | 403 -> `Authorization_required
    | 429 -> `Try_again_later
    | 200 ->
      let json = json_body name call in
      let problems =
        Yojson.Basic.Util.convert_each Protocol_json.problem_of_json json in
      `Problem_body problems
    | n -> unknown_code name n

let send_eval request =
  let name = name `Eval in
  let body = Yojson.Basic.to_string (Protocol_json.json_of_eval request) in
  let call = new post_raw (addr name) body in
  match (response call) # response_status_code with
    | 400 -> `Bad_request
    | 401 -> `Unauthorized
    | 404 -> `Not_found
    | 410 -> `Gone
    | 412 -> `Already_solved
    | 413 -> `Request_too_big
    | 200 ->
      let json = json_body name call in
      let response = Protocol_json.eval_of_json json in
      `Eval_body response
    | n -> unknown_code name n

let send_guess request =
  let name = name `Guess in
  let body = Yojson.Basic.to_string (Protocol_json.json_of_guess request) in
  let call = new post_raw (addr name) body in
  match (response call) # response_status_code with
    | 400 -> `Bad_request
    | 401 -> `Unauthorized
    | 404 -> `Not_found
    | 410 -> `Gone
    | 412 -> `Already_solved
    | 413 -> `Request_too_big
    | 200 ->
      let json = json_body name call in
      let response = Protocol_json.guess_of_json json in
      `Guess_body response
    | n -> unknown_code name n

let send_training request =
  let name = name `Training in
  let body = Yojson.Basic.to_string (Protocol_json.json_of_eval request) in
  let call = new post_raw (addr name) body in
  match (response call) # response_status_code with
    | 400 -> `Bad_request
    | 403 -> `Authorization_required
    | 429 -> `Try_again_later
    | 200 ->
      let json = json_body name call in
      let response = Protocol_json.training_of_json json in
      `Training_body response
    | n -> unknown_code name n

let send_status request =
  let name = name `Status in
  let call = new post_raw (addr name) "" in
  match (response call) # response_status_code with
    | 400 -> `Bad_request
    | 403 -> `Authorization_required
    | 429 -> `Try_again_later
    | 200 -> 
      let json = json_body name call in
      let response = Protocol_json.status_of_json json in
      `Status_body response
    | n -> unknown_code name n
