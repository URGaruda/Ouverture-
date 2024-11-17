open Polynome ;;
(*1.2 exercice 1.5 à 1.7*)

type expression = 
  | Int of int 
  | Pow of char * int 
  | Plus of expression list
  | Mult of expression list 

let figure1 = Plus[ Mult[ Int 123 ; Pow ('x',1) ] ,Int 42 , Pow ('x',3) ]

let rec mult_to_monome (e: expression) : monome = 

let rec expr_to_poly (e: expression) : polynome = 
  match e with 
  |Int(x) -> [(x,0)]
  |Pow(c,x) -> [(1,x)]
  |Mult(t) ->
  |Plus(t) ->  
