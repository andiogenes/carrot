type value = 
  | Number of float
  | String of string
  | ArrMarker
  | ObjMarker
  | BlockMarker
  | BindMarker of string
  | Array of value array
  | Ref of string * value
  | EmptyRef of string
  | Object of (string, value) Hashtbl.t
  | Code of Code.code
  | Block of Code.code list
  
exception Illegal_element

let rec value_to_str v = match v with
  | Number x -> Printf.sprintf "%g" x
  | String x -> x
  | ArrMarker -> raise Illegal_element
  | ObjMarker -> raise Illegal_element
  | BlockMarker -> raise Illegal_element
  | BindMarker _ -> raise Illegal_element
  | Code _ -> raise Illegal_element
  | Block _ -> "<block>"
  | Array arr -> arr |> Array.map value_to_str |> Array.to_list |> String.concat " " |> Printf.sprintf "[ %s ]"
  | Ref (k, v) -> Printf.sprintf "%s: %s" k (value_to_str v)
  | EmptyRef k -> Printf.sprintf "%s: <empty>" k
  | Object tbl -> tbl 
    |> Hashtbl.to_seq 
    |> Seq.map (fun (k, v) -> Printf.sprintf "%s: %s" k (value_to_str v)) 
    |> List.of_seq 
    |> String.concat "; " 
    |> Printf.sprintf "{ %s }"

let exec cu =
  let st = Stack.create () in
  let scope = Hashtbl.create 16 in
  let is_eval = ref true in


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
    match Stack.pop st with
    | ArrMarker -> Array.of_list acc
    | x -> fold_array (x::acc) in


  let rec fold_object acc =
    match Stack.pop st with
    | Ref (k, v) -> let _ = Hashtbl.replace acc k v in fold_object acc
    | ObjMarker -> acc
    | _ -> raise Illegal_element in


  let rec fold_block acc =
    match Stack.pop st with
    | BlockMarker -> acc
    | Code x -> fold_block (x::acc)
    | _ -> raise Illegal_element in


  let rec bind_block scope acc = 
    match Stack.pop st with
    | BindMarker k -> let _ = Hashtbl.replace scope k (Block acc) in acc
    | Code x -> bind_block scope (x::acc)
    | _ -> raise Illegal_element in


  let rec bind_eval scope arg_opt = 
    match Stack.pop st with
    | BindMarker k -> let _ = Option.iter (Hashtbl.replace scope k) arg_opt in arg_opt
    | x when Option.is_none arg_opt -> bind_eval scope (Some x)
    | _ -> raise Illegal_element in


  let rec exec_code scope code =
    let module C = Code in 
    if !is_eval then match code with
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
      | C.Assign k ->
        let obj = Stack.pop st in Hashtbl.replace scope k obj
      | C.BlockStart -> let _ = Stack.push BlockMarker st in is_eval := false
      | C.Execute -> 
        let block = match Stack.pop st with Block x -> x | _ -> raise Illegal_element in
        List.iter (exec_code scope) block
      | C.Def k -> let _ = Stack.push (BindMarker k) st in is_eval := false
      | C.Var k -> Stack.push (BindMarker k) st
      | C.DefVarEnd -> let _ = bind_eval scope None in ()
      | C.Import ->
        let m = match Stack.pop st with EmptyRef x -> x | _ -> raise Illegal_element in
        let cu = Front.pass_file (Printf.sprintf "%s.ct" m) in
        let mscope = Hashtbl.create 16 in
        let _ = List.iter (exec_code mscope) cu in
        Hashtbl.replace scope m (Object mscope)
      | C.Identifier k -> 
        let v = match Hashtbl.find_opt scope k with
          | Some v -> v 
          | None -> EmptyRef k in
        Stack.push v st
      | _ -> ()
    else match code with
      | C.BlockEnd -> 
        let block = fold_block [] in let _ = Stack.push (Block block) st in is_eval := true
      | C.DefVarEnd -> let _ = bind_block scope [] in is_eval := true
      | x -> let _ = Stack.push (Code x) st in () in

  List.iter (exec_code scope) cu