let () =
  let file = Array.get Sys.argv 1 in

  file
  |> Front.pass_file
  |> Interp.exec
