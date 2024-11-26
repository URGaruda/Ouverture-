open Polynome ;;
open Fonction ;;
(*1.2 exercice 1.5 à 1.7*)

type expression = 
  | Int of int 
  | Pow of char * int 
  | Plus of expression list
  | Mult of expression list 
;;
let figure1 = Plus[ Mult[ Int 123 ; Pow ('x',1) ] ;Int 42 ; Pow ('x',3) ];;

let rec voitMult (a:expression list):bool =
  match a with
  |[]->true
  |h::t -> match h with 
            |Int(x)->voitMult(t)
            |Pow(c,x)->voitMult(t)
            |Mult(v)->false
            |Plus(v)->voitMult(t)
;;

let rec voitPlus (a:expression list):bool =
  match a with
  |[]->true
  |h::t -> match h with 
            |Int(x)->voitMult(t)
            |Pow(c,x)->voitMult(t)
            |Mult(v)->voitMult(t)
            |Plus(v)->false
;;

let rec verifArbre (a:expression):bool= (* passe pas pour l'instant *)
  match a with 
  |Int(x) -> if x<1 then false else true 
  |Pow(c,x) -> true
  |Mult(t) -> voitMult(t) && List.iter verifArbre t
  |Plus(t) -> voitPlus(t) && List.iter verifArbre t
;;

let rec mult_to_poly (e: expression list) (r:monome) : polynome = 
  match e with 
  |[]->[r]
  |h::t-> match h with 
          |Int(x)->mult_to_poly t ((fst r)+x,snd r)
          |Pow(c,x)->mult_to_poly t (fst r,x)
          |Mult(v)-> raise Not_found
          |Plus(v)-> mult_to_poly t r 
;;

let rec plus_to_poly (e: expression list) (r:polynome): polynome = 
  match e with 
  |[]->r
  |h::t-> match h with 
          |Int(x)-> plus_to_poly t ((x,0)::r)
          |Pow(c,x)-> plus_to_poly t ((1,x)::r)
          |Mult(v)-> plus_to_poly t ((mult_to_poly v (0,0))@r)
          |Plus(v)-> raise Not_found
;;

let rec expr_to_poly (e: expression) : polynome = 
  match e with 
  |Int(x) -> [(x,0)]
  |Pow(c,x) -> [(1,x)]
  |Mult(t) -> mult_to_poly t (0,0)
  |Plus(t) -> plus_to_poly t []
  
;;

