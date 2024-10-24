(* 1. Écrire une interface qui abstrait les structures de données de type “collection”.*)

module type Collection =
  sig
    type 'a t 
    exception CollectionVide
    val vide : 'a t
    val est_vide : 'a t -> bool
    val ajouter : 'a -> 'a t -> 'a t
    val enlever : 'a t -> ('a * 'a t)
  end
  
module Pile : Collection
  struct
    type 'a t = 'a list
    exception CollectionVide
    let vide  = []
    let est_vide  l = ( l = [] )
    let ajouter x  l =  x::l
    let enlever l = match l with
                    | [] -> raise CollectionVide
                    | x::q -> (x , q)
                           
                    
  end

module File : Collection
  struct
    type 'a t = 'a list
    exception CollectionVide
    let vide  = []
    let est_vide  l = ( l = [] )
    let ajouter x  l =  l@[x]
    let enlever l = match l with
                    | [] -> raise CollectionVide
                    | x::q -> (x , q)
  end




(* Écrire une interface qui abstrait les paramètres d’un itérateur fold. *)

module type Fold =
  sig
    type a
    type b
    val cas_de_base : b
    val traite_et_combine : a  -> b -> b
  end


module create_list_int : Fold =
  struct 
    type a = 'b list
    type b = 'int
    let cas_de_base = []
    let traite_et_combine elt lc  = elt::lc
    
