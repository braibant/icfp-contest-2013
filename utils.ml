let begin_end_msg msg f =
  Printf.printf "BEGIN %S\n%!" msg;
  let result = f () in
  Printf.printf "END %S\n%!" msg;
  result
