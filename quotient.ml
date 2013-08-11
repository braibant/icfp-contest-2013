let ops = Generator.all_ops
let n = 8

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

let discriminate set discr =
  let h = Hashtbl.create 100 in
  List.iter (fun e ->
    let k = Array.map (Eval.eval e) discr in
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

let quotient set =
  let discr = Array.init 100 (fun _ -> Term.rnd64 ()) in
  let subsets = discriminate set discr in
  let handle subset =
    let len = List.length subset in
    if len < 10 then quotient_list subset
    else begin
      let discr = discr_from_sat subset in
      let subsubsets = discriminate subset discr in
      if false (* debug *) then begin
        Printf.printf "list %d was split into %s\n" len
          (String.concat "," (List.map (fun l -> string_of_int (List.length l))
                                subsubsets))
      end;
      let sub_handle subsubset =
        if false (* can have size condition here *) then subsubset
        else quotient_list subsubset in
      List.concat (List.map sub_handle subsubsets)
    end
  in
  List.concat (List.map handle subsets)  

let test ~size_terms ~size_contexts =
  let terms =
    Generator.generate ~force_fold:false size_terms  ~exact:false ops in
  Printf.printf "terms: %i\n%!" (Array.length terms);
  let contexts = (Generator.generate_context size_contexts ops
                    ([Term.Notations.hole 0 false])) in 
  Printf.printf "contexts: %i\n%!" (Array.length contexts);
  let qterms = Utils.time "qterms" (fun () ->
    Utils.begin_end_msg "qterms" (fun () ->
      quotient (Array.to_list terms))) in
  Printf.printf "qterms: %i\n%!" (List.length qterms);
  let qcontexts = Utils.time "qcontexts" (fun () ->
    Utils.begin_end_msg "qcontexts" (fun () ->
      quotient (Array.to_list contexts))) in
  Printf.printf "qcontexts: %i\n%!" (List.length qcontexts);
  ()

(*
let () = test ~size_terms:6 ~size_contexts:4
*)
