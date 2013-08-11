let begin_end_msg msg f =
  Printf.printf "BEGIN %S [%f]\n%!" msg (Sys.time ());
  let result = f () in
  Printf.printf "END %S [%f]\n%!" msg (Sys.time ());
  result

let ask_confirmation () =
  print_endline "Confirm? y/n";
  match read_line () with
    | "y" | "Y" | "yes" -> `Yes
    | "n" | "N" | "no" -> `No
    | other -> `Other other

let write_json_to_file file json =
  Yojson.Basic.to_file ~std:true file json

let read_json_from_file file =
  Yojson.Basic.from_file ~fname:file file

let time name f =
  let time_start = Unix.gettimeofday () in
  let stop () =
    let time_stop = Unix.gettimeofday () in
    Printf.printf "time %S: %f\n" name (time_stop -. time_start) in
  try
    let result = f () in
    stop ();
    result
  with exn ->
    stop ();
    raise exn
