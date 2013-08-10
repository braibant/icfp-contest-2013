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

let handle name call =
  match (response call) # response_status_code with
    | 400 -> `Bad_request
    | 401 -> `Unauthorized
    | 403 -> `Authorization_required
    | 404 -> `Not_found
    | 410 -> `Gone
    | 412 -> `Already_solved
    | 413 -> `Request_too_big
    | 429 -> `Try_again_later
    | 200 -> `Body (json_body name call)
    | n -> `Unknown_code n

type unexpected = [
| `Bad_request
| `Unauthorized
| `Authorization_required
| `Not_found
| `Gone
| `Already_solved
| `Request_too_big
| `Try_again_later
| `Unknown_code of int
]

let str_of_return = function
  | `Bad_request -> "Bad request"
  | `Unauthorized -> "Unauthorized"
  | `Authorization_required -> "Authorization required"
  | `Not_found -> "Not found"
  | `Gone -> "Gone"
  | `Already_solved -> "Already solved"
  | `Request_too_big -> "Request too big"
  | `Try_again_later -> "Try again later"
  | `Unknown_code n -> Printf.sprintf "Unknown code '%d'" n
  | `Result json -> Printf.sprintf "Result: %S" (Yojson.Basic.to_string json)

let send_myproblems () =
  let name = name `Problem in
  let call = new post_raw (addr name) "" in
  match handle name call with
    | #unexpected as ret -> ret
    | `Body json ->
      let problems =
        Yojson.Basic.Util.convert_each Protocol_json.problem_of_json json in
      `Problem_body problems

let send_eval request =
  let name = name `Eval in
  let body = Yojson.Basic.to_string (Protocol_json.json_of_eval request) in
  let call = new post_raw (addr name) body in
  match handle name call with
    | #unexpected as ret -> ret
    | `Body json -> `Eval_body (Protocol_json.eval_of_json json)

exception Bad_request
exception Unauthorized
exception Not_found
exception Gone
exception Already_solved
exception Request_too_big

let send_guess request =
  let name = name `Guess in
  let body = Yojson.Basic.to_string (Protocol_json.json_of_guess request) in
  let call = new post_raw (addr name) body in
  match handle name call with
    | #unexpected as ret -> ret
    | `Body json -> `Guess_body (Protocol_json.guess_of_json json)

let send_training request =
  let name = name `Training in
  let body = Yojson.Basic.to_string (Protocol_json.json_of_training request) in
  let call = new post_raw (addr name) body in
  match handle name call with
    | #unexpected as ret -> ret
    | `Body json -> `Training_body (Protocol_json.training_of_json json)

let send_status () =
  let name = name `Status in
  let call = new post_raw (addr name) "" in
  match handle name call with
    | #unexpected as ret -> ret
    | `Body json -> `Status_body (Protocol_json.status_of_json json)

