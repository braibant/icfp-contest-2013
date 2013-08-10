let auth = ref "0243sNUd0X4oALiqb3Vw2Hejjh4uRV6JHy65rUX9"

let logfile = ref "logfile"
let interactive_mode = ref false 
let problem_size = ref 4

let solved_file = ref "solved_problems"
let problems_file = ref "problems"

type source =
| Train_offline
| Train_online
| Single_problem of string

let source = ref None

let set_source s () =
  source := Some s

let sync_problem_list = ref false
let show_problem = ref None
let list_problems = ref false

let show_status = ref false

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
    "--train-online", Unit (set_source Train_online),
    " use the online training mode";
    "--single-problem", String (fun id -> set_source (Single_problem id) ()),
    "ID run online against the problem of the given id";

    "--log", Set_string logfile, "PATH set log file";
    "-o", Set_string logfile, " (idem)";
    "--interactive", Set interactive_mode, " interactive mode" ;
    "-i", Set interactive_mode, " (idem)" ;
    "--problem-size", Set_int problem_size, "INT set problem size";
    "-n", Set_int problem_size, " (idem)";
  ]
