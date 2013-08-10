let auth = ref "0243sNUd0X4oALiqb3Vw2Hejjh4uRV6JHy65rUX9"

let logfile = ref "logfile"
let interactive_mode = ref false 
let problem_size = ref 4

let solved_file = ref "solved_problems"

type source =
| Train_offline
| Train_online
| Real_stuff

let source = ref None

let set_source s () =
  source := Some s

let args = 
  let open Arg in 
  align [
    "--train-offline", Unit (set_source Train_offline),
    " play offline with a randomly-generated term";
    "--train-online", Unit (set_source Train_online),
    " use the online training mode";
    "--log", Set_string logfile, "PATH set log file";
    "-o", Set_string logfile, " (idem)";
    "--interactive", Set interactive_mode, " interactive mode" ;
    "-i", Set interactive_mode, " (idem)" ;
    "--problem-size", Set_int problem_size, "INT set problem size";
    "-n", Set_int problem_size, " (idem)";
  ]
