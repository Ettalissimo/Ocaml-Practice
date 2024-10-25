let rec max_list lst =
  match lst with
  | [] -> failwith "La liste est vide"  (* Gère le cas où la liste est vide *)
  | [x] -> x  (* Si la liste contient un seul élément, c'est le maximum *)
  | x :: xs -> 
      let max_reste = max_list xs in
      if x > max_reste then x else max_reste  (* Compare x avec le max du reste de la liste *)

(* Fonction pour trouver le maximum d'une liste d'entiers *)
let rec max_list lst =
  match lst with
  | [] -> failwith "Liste vide"
  | [x] -> x
  | x :: xs -> max x (max_list xs)

(* Fonction pour trouver le maximum des maximums *)
let max_max liste_de_listes =
  match liste_de_listes with
  | [] -> failwith "Liste de listes vide"
  | xs -> max_list (List.map max_list xs)



(* Exercice 2 *)

let suivant lst =
  let rec aux acc count current = function
    | [] -> acc @ [count; current]  (* Ajouter le dernier groupe *)
    | x :: xs when x = current -> aux acc (count + 1) current xs  (* Compter les occurrences *)
    | x :: xs -> aux (acc @ [count; current]) 1 x xs  (* Ajouter le groupe et recommencer *)
  in
  match lst with
  | [] -> []  (* Gérer le cas de la liste vide *)
  | x :: xs -> aux [] 1 x xs

let suite n premier =
  let rec aux acc term = function
    | 0 -> List.rev acc  (* Retourner la liste accumulée *)
    | k -> aux (term :: acc) (suivant term) (k - 1)  (* Appel récursif *)
  in
  aux [] premier n
