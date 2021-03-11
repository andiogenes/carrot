let () =
  let file = Array.get Sys.argv 1 in

  file
  |> Front.pass_file
  |> List.iter (fun x -> x |> Code.code_to_str |> print_endline)
