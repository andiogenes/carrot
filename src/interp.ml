type value = 
  | Number of float
  | String of string
  | ArrMarker
  | ObjMarker
  | Array of value array
  | Ref of string * value
  | Object of (string, value) Hashtbl.t

let rec value_to_str v = match v with
  | Number x -> Printf.sprintf "%g" x
  | String x -> x
  | ArrMarker -> ""
  | ObjMarker -> ""
  | Array arr -> arr |> Array.map value_to_str |> Array.to_list |> String.concat " " |> Printf.sprintf "[ %s ]"
  | Ref (k, v) -> Printf.sprintf "%s: %s" k (value_to_str v)
  | Object tbl -> tbl 
    |> Hashtbl.to_seq 
    |> Seq.map (fun (k, v) -> Printf.sprintf "%s: %s" k (value_to_str v)) 
    |> List.of_seq 
    |> String.concat "; " 
    |> Printf.sprintf "{ %s }"

exception Illegal_element

let exec cu =
  let st = Stack.create () in

  let rec fold_number f acc = 
    if Stack.is_empty st then acc
    else match Stack.top st with
      | Number x -> 
        let _ = Stack.pop st in fold_number f (f acc x)
      | _ -> acc in


  let rec fold_string f acc = 
    if Stack.is_empty st then acc
    else match Stack.top st with
      | String x -> 
        let _ = Stack.pop st in fold_string f (f acc x)
      | _ -> acc in

  let rec fold_array acc =
    if Stack.is_empty st then Array.of_list acc
    else match Stack.pop st with
      | ArrMarker -> Array.of_list acc
      | x -> fold_array (x::acc) in

  let rec fold_object acc =
    if Stack.is_empty st then acc
    else match Stack.pop st with
      | Ref (k, v) -> let _ = Hashtbl.add acc k v in fold_object acc
      | ObjMarker -> acc
      | _ -> raise Illegal_element in

  let exec_code code = 
    let module C = Code in match code with
    | C.Number x -> Stack.push (Number x) st
    | C.String x -> Stack.push (String x) st
    | C.Identifier "print" -> Stack.pop st |> value_to_str |> print_endline
    | C.Identifier "pop" -> let _ = Stack.pop st in ()
    | C.Identifier "dup" -> let _ = (Stack.top st |> Stack.push) st in ()
    | C.Identifier "+" -> let x = fold_number (+.) 0. in Stack.push (Number x) st
    | C.Identifier "-" -> 
      let s = match Stack.pop st with Number x -> x | _ -> raise Illegal_element in
      let x = fold_number (-.) s in Stack.push (Number x) st
    | C.Identifier "*" -> let x = fold_number ( *. ) 1. in Stack.push (Number x) st
    | C.Identifier "/" ->
      let s = match Stack.pop st with Number x -> x | _ -> raise Illegal_element in
      let x = fold_number ( /. ) s in Stack.push (Number x) st
    | C.Identifier "++" -> let x = fold_string (fun a b -> String.concat "" [a; b]) "" in Stack.push (String x) st
    | C.ArrStart -> Stack.push ArrMarker st
    | C.ArrEnd -> let arr = fold_array [] in Stack.push (Array arr) st
    | C.ArrAt Int i ->
      let arr = match Stack.pop st with Array x -> x | _ -> raise Illegal_element in
      let elem = Array.get arr i in Stack.push elem st
    | C.Ref k -> let v = Stack.pop st in Stack.push (Ref (k, v)) st
    | C.ObjStart -> Stack.push ObjMarker st
    | C.ObjEnd -> let obj = fold_object (Hashtbl.create 8) in Stack.push (Object obj) st
    | C.Access k ->
      let obj = match Stack.pop st with Object x -> x | _ -> raise Illegal_element in
      let v = Hashtbl.find obj k in let _ = Stack.push v st in ()
    | _ -> () in

  List.iter exec_code cu 