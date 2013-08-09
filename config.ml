let auth = ref "0243sNUd0X4oALiqb3Vw2Hejjh4uRV6JHy65rUX9"

let logfile = ref "logfile"
let interactive_mode = ref false 
let problem_size = ref 4
let secret = ref "(lambda (x) (plus x 1))"


let args = 
  let open Arg in 
  ["-o", Set_string logfile, " set log file";
   "-i", Set interactive_mode, " interactive mode" ;
   "-n", Set_int problem_size, " set problem size";
   "-s", Set_string secret, " set the secret (debug)"]
