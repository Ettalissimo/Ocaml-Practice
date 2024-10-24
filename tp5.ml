(*Écrire un module PrintSimple, conforme à ExprSimple, qui permet de convertir les expressions en
chaîne de caractères.*)

module PrintSimple : ExprSimple with type t = string =
  struct
    type t = string
    let const c = string_of_int c
    let plus e1 e2 = "(" e1 ^ "+" ^ e2 ")"
    let mult e1 e2 = "(" e1 ^ "*" ^  e2 ")"
  end

module PrintExemples = ExemplesSimples (PrintSimple)

let%test _ = (PrintExemples.exemple1 = 1+(2*3) )
let%test _ = (PrintExemples.exemple2 = (5+2)*(2*3) )


(* 1. Écrire un module CompteSimple, conforme à ExprSimple, qui permet de compter les opérations
d’une expression.*)

module CompteSimple : ExprSimple with type t = int =
  struct
    type t = int
    let const c = 0
    let plus e1 e2 = 1 + e1 + e2
    let mult e1 e2 = 1 + e1 ∗ e2
  end

module CompteExemples = ExemplesSimples (CompteSimple)

let%test _ = (PrintExemples.exemple1 = 2 )
let%test _ = (PrintExemples.exemple2 = 3 )

(*
Écrire une interface ExprVar qui permet d’abstraire la présence de variable dans les expressions.
Elle ne doit traiter que la définition et l’utilisation de variable et ne doit pas redéfinir les expressions
simples.
*)

module type ExprVar =
  sig
    type t
    val def : string -> t -> t -> t
    val var : string -> t
  end


module type Expr =
  sig
    include ExprSimple
    include (ExprVar with type t := t)
  end

("let .. = .. in ..")

module PrintVar : ExprVar with type t = string =
  struct
    type t = string
    let def a e1 e2 = "let" ^ a "=" ^ e1 ^ "in" ^ e2
    let var a = x
  end 

module Print =
  struct 
    include Printvar
  end

