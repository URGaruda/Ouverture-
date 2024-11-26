(* Question 1.1 *)
type monome = int * int 
and polynome = monome list ;;



(* Exemples de polynômes *)
let poly1:polynome = [(2,2); (-12,1); (22,0)];;
let poly2:polynome = [(-12,1); (2,1); (22,0); (5,1)];;
let poly3:polynome = [(0,1); (5,1); (0,0)];; 
let poly4:polynome = [(5,3); (2,1); (22,0); (-2,1)];;
let poly5:polynome = [(0,1)];;
let poly6:polynome = [];;



(* Question 1.2 *)
let canonique (p:polynome) : polynome = 
  
  let remove_double (p:polynome) : polynome = 
    let aux (c, d) table =
      let existing_values = try Hashtbl.find table d with Not_found -> [] in
      Hashtbl.replace table d (c :: existing_values) 
    in let table = Hashtbl.create (List.length p) in 
    List.iter (fun monome -> aux monome table) p; 
    Hashtbl.fold (fun d list_c acc -> (List.fold_left (+) 0 list_c, d) :: acc) table []
      
  in let sort_by_degree (p:polynome) : polynome =
       List.sort (fun (k1, v1) (k2, v2) -> if v1 = v2 then compare k1 k2 else compare v1 v2) p
         
  in let remove_null (p:polynome) : polynome =
       List.filter (fun (c, d) -> c <> 0) p
         
  in remove_null (sort_by_degree (remove_double p));;


canonique poly1;; 
canonique poly2;; 
canonique poly3;; 
canonique poly4;; 
canonique poly5;; 
canonique poly6;; 



(* Question 1.4 *)
let poly_prod (p1:polynome) (p2:polynome) : polynome =
  
  let rec monome_prod (m:monome) (p:polynome) : polynome = 
    match p with
    | [] -> []
    | (c,d)::t -> (c*(fst m), d+(snd m))::(monome_prod m t)
  
  in let rec aux (p1:polynome) (p2:polynome) : polynome  = 
       match p1 with
       | []-> [] 
       | m1::t1 -> (monome_prod m1 p2) @ (aux t1 p2)
  
  in canonique (aux p1 p2);; 


poly_prod (canonique poly1) (canonique poly2);; 
poly_prod (canonique poly2) (canonique poly3);; 
poly_prod (canonique poly4) (canonique poly5);; 



(* Question 1.5 *)
type expression = 
  | Int of int 
  | Pow of char * int 
  | Plus of expression list
  | Mult of expression list;;



(* Question 1.6 *)
let figure1 = Plus [ Mult [Int 123;Pow ('x',1)] ; Int 42 ; Pow ('x',3) ];;
let figure2 = Plus [ Int 5 ; Mult [Int 2; Pow ('x',2) ; Int 4] ; Int 2 ; Pow ('x',1) ; Mult [ Int 12 ; Pow ('x',3) ] ];;



(* Question 1.7 *)
let arb2poly (exp:expression) : polynome = 

  let testPow ((var, value) as p) : char * int =
    if var <> 'x' then raise (Invalid_argument "La variable doit etre x")
    else if value < 0 then raise (Invalid_argument "L'exposant des puissances doit etre positif ou nul") 
    else p
    
  in let rec mult2poly (l:expression list) (m:monome) : monome =
    match l with
    | [] -> (fst m, snd m)
    | (h::t) -> match h with
      | Int value -> (mult2poly t (value * fst m, snd m))
      | Pow (var, value) -> let p = (testPow (var, value)) in (mult2poly t (fst m, snd p + snd m))
      | _ -> raise (Invalid_argument "Mult ne peut contenir que des types Int et Pow")
               
  in let rec aux (exp:expression) : polynome = 
       match exp with 
       | Int value -> [(value, 0)]
       | Pow (var, value) -> let p = (testPow (var, value)) in [(1, snd p)] 
       | Mult list -> [(mult2poly list (1,0))]
       | Plus [] -> []
       | Plus (h::t) -> (aux h)@(aux (Plus t))

  in (canonique (aux exp));;

  
arb2poly figure1;;
arb2poly figure2;;



(* Question 1.8 *)
let extraction_alea (l:int list) (p:int list) : (int list * int list) = 
  
  let random : int = 
    Random.self_init ();
    Random.int (List.length l)
  
  in let rec aux (l:int list) (i:int) : int list =
       match l with
       | [] -> []
       | h::t -> if random <> i then h::(aux t (i+1)) else t
          
  in ((aux l 0), (List.nth l random)::p);; 


(extraction_alea [1;2;3;4] [5;6;7;8]);;



(* Question 1.9 *)
let gen_permutation (n:int) =
  
  let rec gen_liste (cpt:int) : int list = 
    if cpt = n then [n] else cpt::(gen_liste (cpt+1)) 
                
  in let rec aux (l:int list) (p:int list) : (int list * int list) =
       if List.length l > 0 
       then let c = (extraction_alea l p) in (aux (fst c) (snd c))
       else ([], p)
  
  in snd (aux (gen_liste 1) []);;


(gen_permutation 3);;

  

(* Question 1.10 *)
type 'a tree = Empty | Node of 'a * 'a tree * 'a tree;;

let abr (l:int list) : int tree =
  
  let rec insertion (n:int) (a:int tree) : int tree =
    match a with
    | Empty -> Node(n, Empty, Empty)
    | Node(x, sag, sad) -> if x = n then raise (Invalid_argument "Chaque element doit etre unique")
        else if x > n then Node(x, insertion n sag, sad) else Node(x, sag, insertion n sad)

  in List.fold_left (fun acc x -> insertion x acc) Empty l;;


abr [4;2;3;8;1;9;6;7;5];;
