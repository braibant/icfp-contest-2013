let equiv t1 t2 =
  Sat.discriminate_with_holes [(t1, t2)] = [Sat.Unsat]

let quotient_list li =
  let rec quotient acc = function
    | [] -> acc
    | h::tl ->
      let pairs = List.map (fun x -> (x,h)) acc in
      let equivs = Sat.discriminate_with_holes pairs in
      quotient (if List.mem Sat.Unsat equivs then acc else h :: acc) tl
  in quotient [] li

let discriminate set discr holes =
  let h = Hashtbl.create 100 in
  List.iter (fun e ->
    let k = Array.map (Eval.eval_with_holes holes e) discr in
    let li = try Hashtbl.find h k with Not_found -> [] in
    Hashtbl.replace h k (e :: li)) set;
  Utils.hashtbl_values h

let discr_from_sat list =
  let n = List.length list in
  let pairs = ref [] in
  for i = 0 to 10 do
    let pair = List.nth list (Random.int n), List.nth list (Random.int n) in 
    pairs := pair :: !pairs;
  done;
  let sat_results = Sat.discriminate_with_holes !pairs in
  let get_result = function
    | Sat.Unsat | Sat.Unknown -> []
    | Sat.Sat (input, holes) -> [input] in
  let keys = List.concat (List.map get_result sat_results) in
  Array.of_list keys

let quotient ?(time_budget=5.) set =
  if List.length set > 10_000 then set else
  let holes = Array.init 20 (fun _ -> Term.rnd64 ()) in
  let discr = Array.init 40 (fun _ -> Term.rnd64 ()) in
  let subsets = discriminate set discr holes in
  let start_time = Unix.gettimeofday () in
  let budget_ended = ref false in
  let size_limit = 10 in
  let handle subset =
    let len = List.length subset in
    if len <= size_limit then quotient_list subset
    else if !budget_ended then subset
    else begin
      if Unix.gettimeofday () -. start_time > time_budget then begin
        print_endline "quotient budget ended";
        budget_ended := true;
      end;
      let discr = discr_from_sat subset in
      let subsubsets = discriminate subset discr holes in
      let sub_handle subsubset =
        if !budget_ended && List.length subsubset > size_limit then subsubset
        else quotient_list subsubset in
      List.concat (List.map sub_handle subsubsets)
    end
  in
  List.concat (List.map handle subsets)
