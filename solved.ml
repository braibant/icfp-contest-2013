let add_to_file id file =
  ignore (Sys.command (Printf.sprintf "echo %S >> %s" id file))

let is_in_file id file =
  let cmd =
    Printf.sprintf "grep \"^%s\" %s > /dev/null" id !Config.solved_file in
  Sys.command cmd = 0

let mark_as_solved id =
  add_to_file id !Config.solved_file

let is_solved id =
  is_in_file id !Config.solved_file
