(* Écrire deux modules Base2 et Base5 conforme à Base et correspondant à une base 2 et une base 5. *)

module Base2 =
  struct
    let base = 2
  end

module Base5 =
  struct
    let base =5
  end

(* 1. Écrire le contrat et les tests unitaires de la fonction l taille qui construit une liste constituée de
n (passé en paramètre) fois le même élément e (passé en paramètre).*)

(* Contrat fonction 1_taille:
  description : fonction qui cree une liste d un element repete n fois
  parametres : entier n et l element qu on veut repeter
  resultat : une liste d element e 
  profil : int -> 'a -> 'a list
*)

let rec l_taille n e = 
  match n with
  | 0 -> []
  | _ -> e::(l_taille (n-1) e)


let%test _ = l_taille 0 7 = []
let%test _ = l_taille 5 10 = [10;10;10;10;10]
let%test _ = l_taille 2 9 = [9;9]

(* 3. Définir l’exception ArgumentInvalide. *)
exception ArgumentInvalide 

(* 4. Écrire la fonction get qui renvoie le i-ième élément d’une liste. Pensez à dé-commenter les tests
pour tester la fonction.*)

(* Contrat get :
   description: une fonction qui retourne le i eme element de la liste
   parametres: la position i et la liste 
   resultat: un element
   profil: int -> 'a list -> 'a
*)

let get i l = 
  let rec aux i l acc =
    match l with
    | [] -> failwith " khawya la liste"
    | h::t -> if (acc = i) then h
              else (aux i t (acc+1))

 let%test _ = get 0 [1;2;3] = 1
 let%test _ = get 2 [] = "khawya la liste"
 let%test _ = get 2 [1;2;3] = 3
 
 


let rec get i lst =
  match lst with
  | [] -> raise ArgumentInvalide
  | x :: xs -> if i = 0 then x 
               else get (i - 1) xs

(*5. Écrire la fonction set qui positionne le i-ième élément d’une liste. Pensez à dé-commenter les
tests pour tester la fonction.*)


let set i lst e =
  match lst with
  | [] -> raise ArgumentInvalide
  | _ -> List.map (fun j x = if (j = i ) then e else x) lst


(*  Exercice 2 : *)
let decompose x =
  let rec aux x acc =  
    match x with
    | 0 -> acc
    | _ -> let q =x / base in
           let r = x mod base
           in  aux q (r::acc)
  in aux x []


  
