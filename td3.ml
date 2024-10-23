(* 1- *)


let rec recherche c lb =
  match lb with 
  | [] -> None
  | (tc,ta)::qlb ->
                    if (c<tc) then None
                    else if (c = tc) then Some ta
                    else recherche c qlb

let rec appartient lc (Noeud(b,lb)) =
  match lc with
  | [] -> b
  | c::qlc -> match (recherche c lb) with
              | None -> false
              | Some a -> appartient qlc a

let rec maj c nouvelleb lb =
  match lb with
  | [] -> [c, nouvelleb]
  | (tc, ta) :: qlb -> 
      if c < tc then (c, nouvelleb) :: (tc, ta) :: qlb
      else if c = tc then (c, nouvelleb) :: qlb
      else (tc, ta) :: (maj c nouvelleb qlb)

      
let rec ajout lc (Noeud (b, lb)) =
  match lc with
  | [] -> Noeud (true, lb)
  | c :: qlb -> 
      let arbre = 
        match (recherche c lb) with
        | None -> Noeud(false, [])
        | Some a -> a
      in Noeud(b, maj c (ajout qlb arbre) lb)


(* 2- *)

let appartient_trie mot Trie(a, fd, fr) =
  let lmst = fd mot in appartient lmot a 

let ajout_trie mot Trie(a, fd, fr) =
  let lmot = fd mot in Trie( ajout lmot a , fd , fr )


