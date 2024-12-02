(*open Arbre ;;*)

type 'a btree =
  | Empty
  | Node of 'a btree * 'a * 'a btree
;;

type 'a confirm = 
  |Empty
  |Int of 'a 
;;
let abr (l:int list) : 'a btree = 
  let rec insert_abr (n:int) (a:'a btree) : 'a btree = 
    match a with
    |Empty -> Node(Empty,n,Empty)
    |Node(g,e,d) -> if n=e then Node(g,e,d) else if n<e then Node((insert_abr n g),e,d) else Node(g,e,(insert_abr n d))

  in let rec abr_cons (l:int list) (a:'a btree): 'a btree =
    match l with
    |[]->a
    |h::t -> abr_cons t (insert_abr h a)

  in abr_cons l Empty
;;

let is_abr (a:'a btree) : bool = 
  let rec it_is (a: 'a btree) (lt:'a confirm) (gt:'a confirm) =
    match a with 
    |Empty->true
    |Node(g,e,d) -> match lt,gt with 
                    |Empty,Empty-> it_is g Empty (Int e) && it_is d (Int e) Empty
                    |Empty,Int(x)-> e<x && it_is g Empty (Int x) && it_is d (Int e) (Int x) 
                    |Int(x),Empty-> e>x && it_is g (Int x) (Int e) && it_is d (Int e) Empty
                    |Int(x),Int(y)-> e>x && e<y && it_is g (Int x) (Int e) && it_is d (Int e) (Int y) 

  in it_is a Empty Empty 
;;

let rec etiquetage (a:'a btree) : expression =
  match a with
  |Empty -> if Random.float 1.0 < 0.5 then Int ((Random.int 401)-200) else Pow ('x',1)
  |Node(Empty,e,Empty) -> if e mod 2 = 1 then Mult [Int ((Random.int 401)-200);Pow ('x',1)] else Pow ('x',Random.int 101)
  |Node(g,e,d)-> if Random.float 1.0 < 0.75 then Plus [ etiquetage g;etiquetage d ] else Mult [etiquetage g;etiquetage d]
;;
let gen_arb (e:expression) : expression = 
  let rec recherche_expression (l:expression list) (n:char): expression list =
    match l with 
    |[]->[]
    |h::t -> if n='M' then match h with
                          |Int(x)-> Int(x) :: recherche_expression t n 
                          |Pow(c,x)-> Pow(c,x) ::  recherche_expression t n 
                          |Plus(v)-> Plus( recherche_expression v n) :: recherche_expression t n 
                          |Mult(v)-> recherche_expression v n @ recherche_expression t n 
              else match h with 
                          |Int(x)-> Int(x) :: recherche_expression t n 
                          |Pow(c,x)-> Pow(c,x) ::  recherche_expression t n 
                          |Plus(v)-> recherche_expression v n @ recherche_expression t n 
                          |Mult(v)-> Mult( recherche_expression v n ) :: recherche_expression t n 
  
  in match e with 
  |Int(x)->Int(x)
  |Pow(c,x)->Pow(c,x)
  |Mult(t)-> Mult(recherche_expression t 'M' )
  |Plus(t)-> Plus(recherche_expression t 'P' )
;;

let figure_droite = Plus [Mult [ Int 123 ; Pow ('x',1)]; Plus [Int 42 ;Pow ('x',3)]] ;;

assert( (gen_arb figure_droite)=figure1);;

assert(is_abr (abr [4;2;3;8;1;9;6;7;5] )=true);;
assert(is_abr (abr [4;8;3;2;1;9;6;7;5] )=true);;
assert(is_abr (abr [4;2;3;8;6;7;5] )=true);;
assert(is_abr (abr [4;2;3;8;1;9;6;7;5;11;24;15;48;32;42] )=true);;
assert(is_abr (Node(Node(Empty,2,Empty),1,Empty))=false);;