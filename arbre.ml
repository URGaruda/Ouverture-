open Polynome ;;
open Fonction ;;
(*1.2 exercice 1.5 à 1.7*)

(*exercice 1.5*)
type expression = 
  | Int of int 
  | Pow of char * int 
  | Plus of expression list
  | Mult of expression list 
;;
(*exercice 1.6*)
let figure1 = Plus[ Mult[ Int 123 ; Pow ('x',1) ] ;Int 42 ; Pow ('x',3) ];;
let figure3 = Plus [Mult [Int 123; Pow ('x', 1)]; Int 42; Int 58; Pow ('x', 3)] ;;
let figure2 = Plus [ Int 5 ; Mult [Int 2; Pow ('x',2) ; Int 4] ; Int 2 ; Pow ('x',1) ; Mult [ Int 12 ; Pow ('x',3) ] ];;

(*execice 1.7*)
(*vérifie s'il y a un mult dans la liste*)
let rec voitMult (a:expression list):bool =
  match a with
  |[]->true
  |h::t -> match h with 
            |Mult _->false
            |_ ->voitMult(t)
;;
let taille_liste (l:'a list) : int =
  let rec aux (l:'a list) (v:int) : int =
    match l with 
    |[]->v
    |h::t -> aux t (v+1) 
  in aux l 0 
;;
(*vérifie s'il y a un plus dans la liste*)
let rec voitPlus (a:expression list):bool =
  match a with
  |[]->true
  |h::t -> match h with 
            |Plus _->false
            |_ ->voitPlus(t)
;;

(*fonction qui vérifie si arbre respecte la grammaire*)
let rec verifArbre (a:expression):bool= (* passe pas pour l'instant *)
  let rec iter (l:expression list) : bool =
    match l with 
    |[]->true
    |h::t -> verifArbre h && iter t 
  in match a with 
    |Int(x) -> if x<0 then false else true 
    |Pow(c,x) -> c='x' && x>=0
    |Mult(t) -> (taille_liste t)>=2 && (voitMult t) && iter t
    |Plus(t) -> (taille_liste t)>=2 && (voitPlus t) && iter t
;;

(*fonction qui converti une liste d'un mult en polynome*)
let rec mult_to_poly (e: expression list) (r:monome) : polynome = 
  match e with 
  |[]->[r]
  |h::t-> match h with 
          |Int(x)->if (fst r)=0 then mult_to_poly t ((fst r)+x,snd r) else mult_to_poly t ((fst r)*x,snd r)
          |Pow(c,x)->mult_to_poly t (fst r,(snd r)+x)
          |Mult(v)-> raise Not_found
          |Plus(v)-> mult_to_poly t r 
;;
(*fonction qui converti une liste d'un plus en polynome*)
let rec plus_to_poly (e: expression list) (r:polynome): polynome = 
  match e with 
  |[]->r
  |h::t-> match h with 
          |Int(x)-> plus_to_poly t ((x,0)::r)
          |Pow(c,x)-> plus_to_poly t ((1,x)::r)
          |Mult(v)-> plus_to_poly t ((mult_to_poly v (0,0))@r)
          |Plus(v)-> raise Not_found
;;
(*fonction qui converti une expression en polynome*)
let rec expr_to_poly (e: expression) : polynome = 
  match e with 
  |Int(x) -> [(x,0)]
  |Pow(c,x) -> [(1,x)]
  |Mult(t) -> mult_to_poly t (0,0)
  |Plus(t) -> plus_to_poly t []
  
;;

(*fonction qui transforme une expression en polynome canonique*)
let arb2poly (e:expression) : polynome =
  if verifArbre e then canonique (expr_to_poly e) else raise Not_found 
;;

assert((arb2poly figure3)=[(100,0); (123, 1);(1, 3)]) ;; 
assert((arb2poly figure2)=[(7, 0); (1, 1); (8, 2); (12, 3)]);;

