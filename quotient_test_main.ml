open Quotient

let ops = Generator.all_ops

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

(* GS: these settings are the largest I tested on my machine;
   ~size_terms:7 takes 60 seconds
   ~size_contexts:5 takes 30 seconds

   let () = test ~size_terms:7 ~size_contexts:5
*)
let () = test ~size_terms:3 ~size_contexts:5
