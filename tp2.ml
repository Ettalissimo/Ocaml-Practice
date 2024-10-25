(* 1 Algorithmes de tri *)

let rec insert order e lst =
  match lst with
  | [] -> [e]
  | h :: t -> if order e h then e :: lst else h :: (insert order e t)


let rec insertion_sort order lst =
  match lst with
  | [] -> []
  | h :: t -> insert order h (insertion_sort order t)


let split lst =
  let rec aux left right n =
    if n = 0 then (List.rev left, right)
    else match right with
      | [] -> (List.rev left, [])
      | h :: t -> aux (h :: left) t (n - 1)
  in aux [] lst (List.length lst / 2)


let rec merge order lst1 lst2 =
  match lst1, lst2 with
  | [], _ -> lst2
  | _, [] -> lst1
  | h1 :: t1, h2 :: t2 ->
      if order h1 h2 then h1 :: (merge order t1 lst2)
      else h2 :: (merge order lst1 t2)


let rec merge_sort order lst =
  match lst with
  | [] | [_] -> lst
  | _ ->
      let left, right = split lst in
      merge order (merge_sort order left) (merge_sort order right)



let split_tail lst =
  let rec aux left right n =
    if n = 0 then (List.rev left, right)
    else match right with
      | [] -> (List.rev left, [])
      | h :: t -> aux (h :: left) t (n - 1)
  in aux [] lst (List.length lst / 2)


let merge_tail order lst1 lst2 =
  let rec aux acc l1 l2 =
    match l1, l2 with
    | [], _ -> List.rev_append acc l2
    | _, [] -> List.rev_append acc l1
    | h1 :: t1, h2 :: t2 ->
        if order h1 h2 then aux (h1 :: acc) t1 l2
        else aux (h2 :: acc) l1 t2
  in aux [] lst1 lst2


let merge_sort_tail order lst =
  let rec sort acc order lst =
    match lst with
    | [] | [_] -> List.rev_append acc lst
    | _ ->
        let left, right = split_tail lst in
        merge_tail order (sort [] order left) (sort [] order right)
  in sort [] order lst


let rec first_n n lst =
  match n, lst with
  | 0, _ | _, [] -> []
  | _, h :: t -> h :: first_n (n - 1) t



let first_n_sorted order n lst =
  let sorted_lst = insertion_sort order lst in
  first_n n sorted_lst


let first_n_efficient order n lst =
  let rec aux acc lst =
    match lst with
    | [] -> acc
    | h :: t ->
        let new_acc = insert order h acc in
        if List.length new_acc > n then aux (List.tl new_acc) t else aux new_acc t
  in aux [] lst


