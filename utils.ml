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

let write_json_to_file file json =
  Yojson.Basic.to_file ~std:true file json

let read_json_from_file file =
  Yojson.Basic.from_file ~fname:file file
