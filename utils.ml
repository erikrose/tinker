exception Error of string

let rec last list =
  match list with
  | [] -> raise (Error "Tried to get the last item of an empty list.")
  | [only] -> only
  | _ :: rest -> last rest

let zip a b =
  List.map2 (fun a b  -> (a, b)) a b
