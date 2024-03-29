open Main

let _ = Random.self_init ();;

let launch_all_missiles p =
  let has_fold =  Generator.OSet.mem Term.Foldo p.Main.operators  in 
  if has_fold 
    || Random.int 6 <> 0 
  then 
    let i = 
      if  p.Main.tfold   then 13 else 11 in 
    let command =       (Printf.sprintf "timeout 300 ./main.native --bypass --teraram --search-max %i --single-problem %s" i p.id)
    in
    Printf.printf "%s\n%!" command;
    Sys.command command
  else
    let command =       (Printf.sprintf "timeout 300 ./main.native --bypass --teraram -j 30 -s --context-size 8 --single-problem %s" p.id)
    in
    Printf.printf "%s\n%!" command;
    Sys.command command

let give_me_a_target () = 
  if not !Config.sync_problem_list then sync_problem_list ();
  let l =  unsolved_problems_sorted () in 
  let n = Random.int (min (List.length l) 20) in
  let pb = List.nth l n in
  problem_data pb 
  
let _ =
  launch_all_missiles (give_me_a_target ())
  
