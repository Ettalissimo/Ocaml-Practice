(* Exercice 1 : Génération d'un code de Gray à nn bits *)

let rec gray_code n =
  if n = 0 then
    [[]]
  else
    let prev_code = gray_code (n - 1) in
    let with_0 = List.map (fun seq -> 0 :: seq) prev_code in
    let with_1 = List.map (fun seq -> 1 :: seq) (List.rev prev_code) in
    with_0 @ with_1

(* Exemple *)
let () = 
  let code = gray_code 3 in
  List.iter (fun seq -> List.iter (Printf.printf "%d") seq; print_newline ()) code

(* Exercice 2 : Combinaisons de kk éléments d'une liste *)

let rec combinaisons l k =
  if k = 0 then
    [[]]
  else match l with
    | [] -> []
    | x :: xs ->
        let with_x = List.map (fun comb -> x :: comb) (combinaisons xs (k - 1)) in
        let without_x = combinaisons xs k in
        with_x @ without_x

(* Exemple *)
let () = 
  let combs = combinaisons [1; 2; 3; 4] 3 in
  List.iter (fun comb -> List.iter (Printf.printf "%d ") comb; print_newline ()) combs


(*  Exercice 3 : Partitions d’un entier nn *)

let rec partitions n =
  let rec aux n max_val =
    if n = 0 then [[]]
    else
      let options = ref [] in
      for i = 1 to min n max_val do
        let sub_partitions = aux (n - i) i in
        let with_i = List.map (fun p -> i :: p) sub_partitions in
        options := !options @ with_i
      done;
      !options
  in
  aux n n

(* Exemple *)
let () = 
  let parts = partitions 4 in
  List.iter (fun part -> List.iter (Printf.printf "%d ") part; print_newline ()) parts


(*  Exercice 4 : Insertion d’un élément à toutes les positions d’une liste *)

let rec insertions_partout e l =
  match l with
  | [] -> [[e]]
  | x :: xs ->
      (e :: l) :: (List.map (fun lst -> x :: lst) (insertions_partout e xs))

(* Exemple *)
let () =
  let insertions = insertions_partout 0 [1; 2] in
  List.iter (fun lst -> List.iter (Printf.printf "%d ") lst; print_newline ()) insertions

(* Exercice 5 : Permutations d’une liste*)

let rec permutations l =
  match l with
  | [] -> [[]]
  | x :: xs ->
      let perms = permutations xs in
      List.flatten (List.map (insertions_partout x) perms)

(* Exemple *)
let () =
  let perms = permutations [1; 2; 3] in
  List.iter (fun perm -> List.iter (Printf.printf "%d ") perm; print_newline ()) perms
