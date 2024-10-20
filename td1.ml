(*1. Écrire la fonction deuxieme qui renvoie le deuxième élément d’une liste. *)

let deuxieme liste =
  match liste with
    | (_::d::_) -> d
    | _ -> failwith "La liste ne contient pas assez d'éléments"

(*
2. Écrire les fonctions n_a_zero et zero_a_n, telles que :
n_a_zero n = [n; n-1;...; 1 ; 0]
zero_a_n n = [0; 1 ;...; n-1; n]
Attention : utilisation de append interdite.
*)

let rec n_a_zero n =
  match n with 
    | 0 -> [0]
    | _ -> n::(n_a_zero (n-1))

(* rev : 'a list -> 'a list *)
let zero_a_n n =
  let rec aux n acc =
    match n with
    | 0 -> 0 :: acc
    | _ -> aux (n - 1) (n :: acc)
  in
  aux n []

(*3. Liste des indices/positions d’un élément e dans une liste l.*)

let position e l =
  let rec aux e l acc index =
    match l with 
    | [] -> acc                            (* Si la liste est vide, retourner les indices accumulés *)
    | h :: t -> 
        if h = e then 
          aux e t (index :: acc) (index + 1)  (* Ajouter l'index à acc si h == e *)
        else 
          aux e t acc (index + 1)              (* Continuer sans ajouter à acc mais incrémenter l'index *)
  in
  List.rev (aux e l [] 0)  (* Inverser les indices avant de les retourner *)



  let rec fold_left f e l =
    match l with 
    | [] -> e 
    | h::t -> fold_left f (f e h) t


  let rec fold_right f l e =
    match l with
    | [] ->
    | h::t ->  f t (fold_right f h e) 


(*3. Ecrire la fonction  ́ rev (qui renverse une liste) a l’aide des deux iterateurs.  ́
Quelle version a la complexite la plus faible? (cf vid  ́ eos du  ́ List . fold left )*)  

  let rev l = fold_left (fun r x -> x::r ) [] l

  let rev l = fold_right (fun r x -> x@[r] ) l [] 



  
  
