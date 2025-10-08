let choose ?(weights : float list option) (l : 'a list) : 'a option =
  match l with
  | [] -> None
  | _ ->
      let weights =
        match weights with
        | Some w when List.length w = List.length l -> w
        | Some w ->
            List.init (List.length l) (fun i ->
                Option.value (List.nth_opt w i) ~default:1.)
        | _ -> List.init (List.length l) (fun _ -> 1.)
      in
      let total = List.fold_left ( +. ) 0. weights in
      let r = Random.float total in
      let rec pick acc = function
        | [], [] -> List.hd l
        | x :: xs, w :: ws ->
            let acc' = acc +. w in
            if r <= acc' then x else pick acc' (xs, ws)
        | _ -> failwith "weights mismatch"
      in
      Some (pick 0. (l, weights))
