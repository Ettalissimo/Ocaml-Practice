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

let rev l =  List.fold_left (fun r x -> x::r ) [] l

let rev l =  List.fold_right (fun r x -> x@[r] ) l [] 


(*1. Écrire map, qui applique une fonction donnée à tous les éléments d’une liste, i.e. map f [a; b; c] = [f a; f b; f c].*)

let rec map  f l =
  match l with
  | [] -> []
  | h::t -> (f h)::(map f t)


(* 2. Écrire flatten (aplatissement d’une liste de listes).*)

let flatten l = List.fold_left (fun x r  = x@r) l [] 


(*3. Écrire une fonction fsts qui prend une liste de couples et renvoie la liste des premiers éléments.*)

let fsts l =  List.fold_right (fun (a,_) r = a::r) l []

(*4. Écrire une fonction split telle que : split [(a1,b1);...;(an,bn)]=([a1;...;an],[b1;...;bn]).*)

let  split l = List.fold_right (fun (a,b) (x,y) = (a::x,b::y) ) l ([], [])

(*5. Écrire une fonction qui supprime les doublons d’une liste.*)

let supprimer_doublons l = fold_right(fun x r -> if (List.mem x r ) then r else x::r) l []

  
  
