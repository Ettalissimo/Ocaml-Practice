module type SignatureEntree = sig
  (* Définition de la signature attendue *)
  type t
  val some_function : t -> t
end

module type SignatureSortie = sig
  (* Définition de la signature du module résultant *)
  val another_function : t -> t
end

module MonFoncteur (M : SignatureEntree) : SignatureSortie = struct
  (* Corps du foncteur, utilisant le module M *)
  let another_function x = M.some_function x
end
