let begin_end_msg msg f =
  Printf.printf "BEGIN %S\n%!" msg;
  let result = f () in
  Printf.printf "END %S\n%!" msg;
  result

let ask_confirmation () =
  print_endline "Confirm? y/n";
  match read_line () with
    | "y" | "Y" | "yes" -> `Yes
    | "n" | "N" | "no" -> `No
    | other -> `Other other

