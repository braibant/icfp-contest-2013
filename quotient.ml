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

let quotient set =
  let discr = Array.init 100 (fun _ -> Term.rnd64 ()) in
  let h = Hashtbl.create 100 in
  Array.iter (fun e ->
    let k = Array.map (Eval.eval e) discr in
    let li = try Hashtbl.find h k with Not_found -> [] in
    Hashtbl.replace h k (e :: li)) set;
  Hashtbl.fold (fun k es qs -> quotient_list es :: qs) h []
            
(*
let terms =
  Array.init (n+1) (fun i ->
    if i < 2 then [| |]
    else
      Utils.begin_end_msg ("computing terms " ^ string_of_int i) begin fun () ->
        Array.of_list (Generator.generate i ops)
      end
  )

let nterms = Array.map Array.length terms
let total_nterms = Array.fold_left (+) 0 nterms

let q2 = quotient terms.(2);;
let q3 = quotient terms.(3);;
let q4 = quotient terms.(4);;
let q5 = Utils.time "quotient 5" (fun () -> quotient terms.(5));;
let q6 = Utils.time "quotient 6" (fun () -> quotient terms.(6));;

let terms26 =
  Array.of_list
    (List.concat
       (List.map (fun n -> Array.to_list terms.(n)) [2;3;4;5;6]))

let q26 = quotient terms26;;
*)

let test ~size_terms ~size_contexts =
  let terms =
    Generator.generate ~force_fold:false size_terms  ~exact:false ops in
  Printf.printf "terms: %i\n%!" (Array.length terms);
  let contexts = (Generator.generate_context size_contexts ops
                    ([Term.Notations.hole 0 false])) in 
  Printf.printf "contexts: %i\n%!" (Array.length contexts);
  let qterms = Utils.time "qterms" (fun () ->
    Utils.begin_end_msg "qterms" (fun () ->
      quotient terms)) in
  Printf.printf "qterms: %i\n%!" (List.length qterms);
  let qcontexts = Utils.time "qcontexts" (fun () ->
    Utils.begin_end_msg "qcontexts" (fun () ->
      quotient contexts)) in
  Printf.printf "qcontexts: %i\n%!" (List.length qcontexts);
  ()

let () = test ~size_terms:5 ~size_contexts:4
