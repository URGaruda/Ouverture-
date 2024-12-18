(* Question 1.1 *)

type monome = int * int 
and polynome = monome list ;;



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

let poly1:polynome = [(2,2); (-12,1); (22,0)];;
let poly2:polynome = [(-12,1); (2,1); (22,0); (5,1)];;
let poly3:polynome = [(0,1); (5,1); (0,0)];; 
let poly4:polynome = [(5,3); (2,1); (22,0); (-2,1)];;
let poly5:polynome = [(0,1)];;
let poly6:polynome = [];;

let () = 
  assert (canonique poly1 = [(22,0); (-12,1); (2,2)]);
  assert (canonique poly2 = [(22,0); (-5,1)]);
  assert (canonique poly3 = [(5,1)]);
  assert (canonique poly4 = [(22,0); (5,3)]);
  assert (canonique poly5 = []);
  assert (canonique poly6 = []);;



(* Question 1.3 *)

let rec poly_add (p1:polynome) (p2:polynome) : polynome =
  match p1, p2 with
  | [], [] -> []
  | [], p2 -> p2
  | p1, [] -> p1
  | (c1, d1)::t1, (c2, d2)::t2 ->
    if d1 = d2 then
      let sum = c1 + c2 in
      if sum = 0 then poly_add t1 t2 else (sum, d1) :: poly_add t1 t2
    else if d1 < d2 then (c1, d1) :: poly_add t1 p2
    else (c2, d2) :: poly_add p1 t2;;

let poly_add_canonique (p1:polynome) (p2:polynome) : polynome =
  (canonique (poly_add p1 p2));;

let () = 
  assert (poly_add_canonique [(22,0); (-12,1); (2,2)] [(22,0); (-5,1)] = [(44,0); (-17,1); (2,2)]);
  assert (poly_add_canonique (canonique poly1) (canonique poly2) = [(44,0); (-17,1); (2,2)]);
  assert (poly_add_canonique (canonique poly1) (canonique poly3) = [(22,0); (-7,1); (2,2)]);
  assert (poly_add_canonique (canonique poly4) (canonique poly5) = canonique poly4);;



(* Question 1.4 *)

let poly_prod (p1:polynome) (p2:polynome) : polynome = 
  
  let monome_prod (m:monome) (p:polynome) = 
    List.map (fun (c, d) -> (c * (fst m), d + (snd m))) p 
      
  in (List.fold_left (fun acc mon -> poly_add acc (monome_prod mon p2)) [] p1);;

let poly_prod_canonique (p1:polynome) (p2:polynome) : polynome =
  (canonique (poly_prod p1 p2));;

let () = 
  assert (poly_prod_canonique [(22,0); (-12,1); (2,2)] [(22,0); (-5,1)] = [(484,0); (-374,1); (104,2); (-10,3)]);
  assert (poly_prod_canonique (canonique poly1) (canonique poly2) = [(484,0); (-374,1); (104,2); (-10,3)]);
  assert (poly_prod_canonique (canonique poly1) (canonique poly3) = [(110,1); (-60,2); (10,3)]);
  assert (poly_prod_canonique (canonique poly4) (canonique poly5) = []);;



(* Question 1.5 *)

type expression = 
  | Int of int 
  | Pow of char * int 
  | Plus of expression list
  | Mult of expression list;;



(* Question 1.6 *)

let figure1 = Plus [Mult [Int 123; Pow ('x',1)]; Int 42; Pow ('x',3)];;



(* Question 1.7 *)

let arb2poly (exp:expression) : polynome = 

  let testPow ((var, value) as p) : char * int =
    if var <> 'x' then raise (Invalid_argument "La variable doit etre x")
    else if value < 0 then raise (Invalid_argument "L'exposant des puissances doit etre positif ou nul") 
    else p
    
  in let rec mult2poly (l:expression list) (m:monome) (is_first_exp:bool) : polynome =
    if List.length l < 2 && is_first_exp then raise (Invalid_argument "Un produit doit contenir au moins 2 expressions")
    else match l with
      | [] -> [(fst m, snd m)]
      | (h :: t) -> match h with
      | Int value -> (mult2poly t (value * fst m, snd m) false)
      | Pow (var, value) -> let p = (testPow (var, value)) in (mult2poly t (fst m, snd p + snd m) false)
      | Plus list -> poly_prod_canonique (plus2poly list true) (mult2poly t (fst m, snd m) false)
      | Mult _ -> raise (Invalid_argument "Un produit ne peut pas contenir de produits") 
               
  and plus2poly (l:expression list) (is_first_exp:bool) : polynome = 
    if List.length l < 2 && is_first_exp then raise (Invalid_argument "Une somme doit contenir au moins 2 expressions")
    else match l with 
      | [] -> []
      | (h :: t) -> match h with
        | Int value -> [(value, 0)]@(plus2poly t false)
        | Pow (var, value) -> let p = (testPow (var, value)) in [(1, snd p)]@(plus2poly t false)
        | Mult list -> (mult2poly list (1,0) true)@(plus2poly t false)
        | Plus _ -> raise (Invalid_argument "Une somme ne peut pas contenir de sommes")      

  in let aux (exp:expression) : polynome = 
    match exp with 
    | Int value -> [(value, 0)]
    | Pow (var, value) -> let p = (testPow (var, value)) in [(1, snd p)] 
    | Mult list -> (mult2poly list (1,0) true)
    | Plus list -> (plus2poly list true)
  
  in (canonique (aux exp));;

(* Cas nominaux *)
let int1 = Int 5;; 
let int2 = Int (-4);; 
let pow1 = Pow ('x',1);;
let pow2 = Pow ('x',2);;
let plus1 = Plus [int1; int2];;
let plus2 = Plus [int1; pow2];;
let mult1 = Mult [int1; pow1];;
let mult2 = Mult [int1; int2; pow1; pow2];;
let mult3 = Mult [plus2; pow2];;
let plus3 = Plus [mult1; mult2];;
let test1 = Plus [Int 5; Mult [Int 2; Pow ('x',2); Int 4]; Int 2; Pow ('x',1); Mult [Int 12; Pow ('x',3)]];;

let () = 
  assert (arb2poly int1 = [(5,0)]);
  assert (arb2poly int2 = [(-4,0)]);
  assert (arb2poly pow1 = [(1,1)]);
  assert (arb2poly pow2 = [(1,2)]);
  assert (arb2poly plus1 = [(1,0)]);
  assert (arb2poly plus2 = [(5,0); (1,2)]);
  assert (arb2poly mult1 = [(5,1)]);
  assert (arb2poly mult2 = [(-20,3)]);
  assert (arb2poly mult3 = [(5,2); (1,4)]);
  assert (arb2poly plus3 = [(5,1); (-20,3)]);
  assert (arb2poly test1 = [(7,0); (1,1); (8,2); (12,3)]);;

(* Cas d'erreur *)
let assert_exception_arb2poly (exp:expression) =
  try
    let _ = (arb2poly exp) in assert false
  with
  | Invalid_argument _ -> assert true
  | _ -> assert false;;

let pow3 = Pow ('y', 1);;
let pow4 = Pow ('x', -1);;
let plus4 = Plus [int1];;
let plus5 = Plus [plus1; mult1];;
let mult4 = Mult [pow1];;
let mult5 = Mult [mult1; plus2];;

let () =
  assert_exception_arb2poly pow3;
  assert_exception_arb2poly pow4;
  assert_exception_arb2poly plus4;
  assert_exception_arb2poly plus5;
  assert_exception_arb2poly mult4;
  assert_exception_arb2poly mult5;;



(* Question 1.8 *)

let extraction_alea (l:int list) (p:int list) : (int list * int list) = 
  
  let random : int = 
    Random.self_init ();
    Random.int (List.length l)
  
  in let rec aux (l:int list) (i:int) : int list =
    match l with
    | [] -> []
    | h :: t -> if random <> i then h :: (aux t (i+1)) else t
          
  in ((aux l 0), (List.nth l random) :: p);; 



(* Question 1.9 *)

let gen_permutation (n:int) : int list =
  
  let rec gen_liste (cpt:int) : int list = 
    if cpt = n then [n] else cpt :: (gen_liste (cpt+1)) 
                
  in let rec aux (l:int list) (p:int list) : (int list * int list) =
    if List.length l > 0 
    then let extr = (extraction_alea l p) in (aux (fst extr) (snd extr))
    else ([], p)
  
  in snd (aux (gen_liste 1) []);;



(* Question 1.10 *)

type 'a tree = Empty | Node of 'a * 'a tree * 'a tree;;

let abr (l:int list) : int tree =
  
  let rec insertion (n:int) (a:int tree) : int tree =
    match a with
    | Empty -> Node(n, Empty, Empty)
    | Node(x, sag, sad) -> if x = n then raise (Invalid_argument "Chaque element doit etre unique")
      else if x > n then Node(x, insertion n sag, sad) else Node(x, sag, insertion n sad)

  in List.fold_left (fun acc x -> insertion x acc) Empty l;;

let () = 
  assert (abr [4; 2; 3; 8; 1; 9; 6; 7; 5] = Node (4, Node (2, Node (1, Empty, Empty), Node (3, Empty, Empty)), Node (8, Node (6, Node (5, Empty, Empty), Node (7, Empty, Empty)), Node (9, Empty, Empty))));
  assert (abr [4; 2; 1; 3; 8; 6; 5; 9; 7] = Node (4, Node (2, Node (1, Empty, Empty), Node (3, Empty, Empty)), Node (8, Node (6, Node (5, Empty, Empty), Node (7, Empty, Empty)), Node (9, Empty, Empty))));;



(* Question 1.11 *)

let rec etiquetage (abr:int tree) : expression = 
  match abr with
  | Empty -> if Random.float 1.0 < 0.5 then Int ((Random.int 401)-200) else Pow ('x', 1)
  | Node(l, Empty, Empty) -> if l mod 2 = 1 then Mult [Int ((Random.int 401)-200); Pow ('x', 1)] else Pow ('x', Random.int 101)
  | Node(l, g, d) -> if Random.float 1.0 < 0.75 then Plus [etiquetage g; etiquetage d] else Mult [etiquetage g; etiquetage d];;



(* Question 1.12 *)

let gen_arb (e:expression) : expression = 
  
  let rec aux (l:expression list) (n:char) : expression list =
    match l with 
    | [] -> []
    | h :: t -> match h with
      | Int(x) -> Int(x) :: (aux t n)
      | Pow(c,x) -> Pow(c,x) :: (aux t n) 
      | Plus(v) -> if n = 'P' then (aux v n)@(aux t n) else Plus(aux v 'P') :: (aux t n)
      | Mult(v) -> if n = 'M' then (aux v n)@(aux t n) else Mult(aux v 'M') :: (aux t n) 
          
  in match e with 
  | Int(x) -> Int(x)
  | Pow(c,x) -> Pow(c,x)
  | Mult(t) -> Mult(aux t 'M')
  | Plus(t) -> Plus(aux t 'P');;                                                                                 
  
let figure_droite = Plus [Mult [Int 123; Pow ('x',1)]; Plus [Int 42; Pow ('x',3)]];; 
let () = 
  assert ((gen_arb figure_droite) = figure1);;
