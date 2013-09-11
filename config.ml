let auth = ref "0243sNUd0X4oALiqb3Vw2Hejjh4uRV6JHy65rUX9"

let logfile = ref "logfile"
let interactive_mode = ref false 
let problem_size = ref 4
let search_max = ref 100
let synthesis = ref false
let jobs = ref 4
let context_size = ref 10
let solved_file = ref "solved_problems"
let problems_file = ref "problems"
let last_training_file = ref "last_training"
let teraram = ref false

let quotient = ref false

let bypass = ref false

type source =
| Train_offline
| Train_offline2
| Train_online
| Train_serialized
| Single_problem of string
| Easy_problems_of_size_at_most of int

let source = ref None

let set_source s () =
  source := Some s

let sync_problem_list = ref false
let show_problem = ref None
let list_problems = ref false

let show_status = ref false

let pause_time_after_problem = ref 0

let args = 
  let open Arg in 
  align [
    "--status", Set show_status, " show status and exit";

    "--sync-problem-list", Set sync_problem_list,
    " synchronize the 'problems' file with our current state of resolution";
    "--show-problem", String (fun id -> show_problem := Some id),
    "ID show the problem status for the given id";
    "--list-problems", Set list_problems,
    " list some available problems, easiest first";

    "--train-offline", Unit (set_source Train_offline),
    " play offline with a randomly-generated term";

    "--train-offline2", Unit (set_source Train_offline2),
    " play offline with a randomly-generated term";

    "--train-online", Unit (set_source Train_online),
    " use the online training mode";
    "--train-serialized", Unit (set_source Train_serialized),
    " play offline with the last online training test, if serialized";

    "--single-problem", String (fun id -> set_source (Single_problem id) ()),
    "ID run online against the problem of the given id";

    "--solve-easy-problems", Int (fun n ->
      set_source (Easy_problems_of_size_at_most n) ()),
    "INT solve easy problems of size at most the given size";

    "--log", Set_string logfile, "PATH set log file";
    "-o", Set_string logfile, " (idem)";
    "--interactive", Set interactive_mode, " interactive mode" ;
    "-i", Set interactive_mode, " (idem)" ;
    "--problem-size", Set_int problem_size, "INT set problem size";
    "-n", Set_int problem_size, " (idem)";
    "--search-max", Set_int search_max, "Max size of generated terms";

    "--teraram", Set teraram, "";

    "-s", Set synthesis, " use synthesis mode";
    (* "-j", Int (fun i -> jobs := i;   Functory.Cores.set_number_of_cores i), " number of cores"; *)
    "-j", Int (fun i -> jobs := i;   Parmap.set_default_ncores i), " number of cores";
    "--context-size", Set_int context_size, " set max context size";
    (* "--batch", Set_int batch_size, " size of batches to be dispatched between the cores" *)

    "--quotient", Set quotient, " set quotienting";
    "--no-quotient", Clear quotient, " disable quotienting (disabled by default)";

    "--pause-after-problem", Set_int pause_time_after_problem, "INT sleep the given number of seconds after each problem (default 0)";
    "--bypass", Set bypass, " disbale all interactive queries"
  ]
