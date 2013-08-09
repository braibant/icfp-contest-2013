(* Note: I used (nested) submodules to avoid name clashes among field
   names *)

type error_message = string

(** Getting problems


    Use the following API to initiate a new problem instance:
    
    POST /myproblems
*)

module Problem = struct
  module Request = struct
    type request = ()
  end

  module Response = struct
    type response = {
      id: string;
      (** a problem ID corresponding to some program P *)
      size: int;
      (** `size` is in the range [3,30] and |P| = size *)
      operators: string list;
      (** When interprested as a set, (Operators P `operators`) is valid *)
      solved: bool option;
      (** If present, `solved` indicates whether or not a point has been
          scored on this problem. *)
      timeleft: float option;
      (** If present, `timeLeft` records the time in seconds remaining
          to solve the problem. If absent, you have 300 seconds to solve
          the problem---the clock starts ticking as soon as you make an
          /eval or /guess request for that problem. *)
    }
  end

  type return = [
  | (* 200 *) `Problem_body of Response.response list
  | (* 403 *) `Authorization_required 
  | (* 429 *) `Try_again_later
  ]
end

(** Evaluating programs

    Use the following API to evaluate a program on some inputs:

    POST /eval
*)
module Eval = struct
  module Request = struct
    type request = {
      name: [ `Id of string
              (** A program ID obtained previously from the Game server *)
            | `Program of string
              (** A \BV program P formatted as a string. The program must
                  be 1024 characters or less, and |P| is less than or
                  equal to 100 *)
            ];
      arguments: int64 array;
      (** Up to 256 64-bit unsigned numbers *)
    }
  end

  module Response = struct
    type response = [
    | `Ok of int64 array
      (** an array `outputs` of 64-bit unsigned numbers, in 1-1
          correspondence with `arguments`, such that, if P is the input
          program, outputs[i] = P (arguments[i]). *)
    | `Error of error_message
    ]
  end

  type return = [
  | (* 200 *) `Eval_body of Response.response
  | (* 400 *) `Bad_request  (* some input is not well-formed *)
  | (* 401 *) `Unauthorized (* problem was not requested by the current user *)
  | (* 404 *) `Not_found (* no such challenge *)
  | (* 410 *) `Gone (* problem requested more than 5 minutes ago *)
  | (* 412 *) `Already_solved (* problem was already solved (by current user) *)
  | (* 413 *) `Request_too_big
  ]
end


(** Submitting guesses


    Use the following API to submit guesses (and test if they are valid
    solutions to the challenge):

    POST /guess
*)
module Guess = struct

  module Request = struct
    type request = {
      id: string; (** a problem ID obtained from the Game server *)
      program: string; (** a \BV program, guessed to be equivalent to the secret *)
    }
  end

  module Response = struct
    type response = {
      status: status;
      lightning: bool option;
      (** if `lightning` is present and true, then your guess was counted
          for the lightning division *)
    }
    and status = [
    | `Win
    (** If r.status is "win" if you guessed correctly. You score one
        point if the request id is not a training ID. *)
    | `Mismatch of mismatch (*. see mismatch below *)
    | `Error of error_message
    (** If status is "error", then the message field contains an
        explanation. Note, if the Game server is unable to prove that
        your guess is functionally equivalent to the secret program,
        then you do NOT score a point. You can either try another guess,
        or move on to another problem. If you make reasonable guesses,
        we do not expect this to happen. *)
    ]
    and mismatch = {
      input: int64; (** an input argument *)
      challenge_result: int64; (** the result of the challenge program *)
      guess_result: int64; (** the result of your guessed program *)
    }
  end
  
  type return = [
  | (* 200 *) `Guess_body of Response.response
  | (* 400 *) `Bad_request (* some input is not well-formed *)
  | (* 401 *) `Unauthorize (* problem was not request by the current user *)
  | (* 404 *) `Not_found (* no such challenge *)
  | (* 410 *) `Gone (* problem requested more than 5 minutes ago *)
  | (* 412 *) `Already_solved (* problem was already solved by current user *)
  | (* 413 *) `Request_too_big
  ]
end


(** Training

    Use the following API to obtain training programs:

    POST /train *)

module Training = struct
  module Request = struct
    type request = {
      size: int option; (* the size of the problem requested, in the range [3,30] *)
      operators: string list option; (* either [], ["tfold"] or ["fold"] *)
    }
  end

  module Response= struct
    type training_problem = {
      challenge: string; (* some \BV program P *)
      id: string; (* a traning problem ID associated with P *)
      size: int; (* |P| (equal to the request size if present) *)
      operators: string list; (* (Operators P operators) is valid,
                                 and if the request operators is []
                                 then "fold" and "tfold" do not occur in `operators`;
                                 if the request operators was defined,
                                 it is included in `operators` *)
    }
  end

  type return = [
  | (* 200 *) `Training_body of Response.training_problem
  | (* 400 *) `Bad_request
  | (* 403 *) `Authorization_required
  | (* 429 *) `Try_again_later
  ]
end

    
(** Status

   The following API displays various statistics about your team:

   POST /status
*)

module Status = struct
  module Request = struct
    type request = ()
  end

  module Response = struct
    type status = {
      easy_chair_id: int;
      contest_score: int;
      lightning_score: int;
      training_score: int;
      mismatches: int;
      num_requests: int;
      request_window: request_window;
      cpu_window: cpu_window;
      cpu_total_time: float;
    }
    and request_window = {
      request_resets_in: float;
      (** the time in seconds until the current 20-second
          window expires and your allocation is reset *)
      request_amount: int;
      (** the number of requests you have already made in the
           current window *)
      request_limit: int;
      (** maximum number of requests you are allowed to make in the
          current window. *)
    }
    and cpu_window = {
      cpu_resets_in: float;
      (** the time in seconds until the current 60-second window expires and
          your allocation is reset. *)
      cpu_amount: float;
      (** the amount of CPU time you have already consumed in
           the current window. *)
      cpu_limit: float;
      (** maximum number of CPU time you have available to you in the
          current window. *)
    }
    (**  Depending on load, we may change the window sizes and number of
         requests/cpu time allotted to you. *)
  
  end

  type return = [
  | (* 200 *) `Status_body of Response.status
  | (* 400 *) `Bad_request
  | (* 403 *) `Authorization_required
  | (* 429 *) `Try_again_later
  ]
end
