open Main

let launch_all_missiles p =
  if 
  Generator.OSet.mem Term.Foldo p.Main.operators
    || Random.int 4 = 0 
  then 
    Sys.command 
      (Printf.sprintf "timeout 300 ./main.native --teraram --search-max 13 --single-problem %s" p.id)
  else
    Sys.command 
      (Printf.sprintf "timeout 300 ./main.native --teraram -j 30 -s --context-size 8 --single-problem %s" p.id)

let give_me_a_target () = 
  if not !Config.sync_problem_list then sync_problem_list ();
  let pb = List.hd (unsolved_problems_sorted ())  in 
  problem_data pb
  
let _ =
  launch_all_missiles (give_me_a_target ())
  
