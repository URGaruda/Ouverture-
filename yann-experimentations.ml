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

let poly_add p1 p2 =

  let rec add p1 p2 acc =
    match p1, p2 with
    | [], [] -> acc 
    | (c1, d1) :: t1, [] -> (c1, d1) :: acc @ t1  
    | [], (c2, d2) :: t2 -> (c2, d2) :: acc @ t2  
    | (c1, d1) :: t1, (c2, d2) :: t2 when d1 = d2 -> 
      let new_coef = c1 + c2 in
        if new_coef = 0 then add t1 t2 acc
        else add t1 t2 ((new_coef, d1) :: acc)
    | (c1, d1) :: t1, (c2, d2) :: t2 when d1 < d2 -> 
      add t1 p2 ((c1, d1) :: acc)
    | (c1, d1) :: t1, (c2, d2) :: t2 ->  
      add p1 t2 ((c2, d2) :: acc)
  
  in let resultat = add p1 p2 [] 
    
  in List.sort (fun (_, d1) (_, d2) -> compare d1 d2) resultat;;

let () = 
  assert (poly_add [(22,0); (-12,1); (2,2)] [(22,0); (-5,1)] = [(44,0); (-17,1); (2,2)]);
  assert (poly_add (canonique poly1) (canonique poly2) = [(44,0); (-17,1); (2,2)]);
  assert (poly_add (canonique poly1) (canonique poly3) = [(22,0); (-7,1); (2,2)]);
  assert (poly_add (canonique poly4) (canonique poly5) = canonique poly4);;



(* Question 1.4 *)

let poly_prod (p1:polynome) (p2:polynome) : polynome = 
  
  let monome_prod (m:monome) (p:polynome) = 
    List.map (fun (c, d) -> (c * (fst m), d + (snd m))) p 
      
  in (canonique (List.fold_left (fun acc mon -> poly_add acc (monome_prod mon p2)) [] p1));;

let () = 
  assert (poly_prod [(22,0); (-12,1); (2,2)] [(22,0); (-5,1)] = [(484,0); (-374,1); (104,2); (-10,3)]);
  assert (poly_prod (canonique poly1) (canonique poly2) = [(484,0); (-374,1); (104,2); (-10,3)]);
  assert (poly_prod (canonique poly1) (canonique poly3) = [(110,1); (-60,2); (10,3)]);
  assert (poly_prod (canonique poly4) (canonique poly5) = []);;



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
      | Plus list -> poly_prod (plus2poly list true) (mult2poly t (fst m, snd m) false)
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



(* Question 2.13 *) 

let gen_exp (n:int) (taille:int) : expression list =
  
  let rec gen_permutations n =
    if n <= 0 then []
    else (gen_permutation taille) :: (gen_permutations (n - 1))

  in List.map gen_arb (List.map etiquetage (List.map abr (gen_permutations n)));;



(* Question 2.14 *) 

let time_execution fct arg f =
  let file = open_out_gen [Open_creat; Open_append; Open_text] 0o666 f in
  let start_time = Sys.time () in
  let result = fct arg in
  let end_time = Sys.time () in
  let duration = end_time -. start_time in
  Printf.fprintf file "%f;" duration;
  Printf.printf "Temps d'execution : %fs\n" duration;
  close_out file;
  result;;


(* Stratégie naïve récursive *)
let exp_somme1 (l: polynome list) : polynome =
  let rec aux (l:polynome list) (acc:polynome) : polynome =
    match l with
    | [] -> acc
    | a :: tl -> aux tl (poly_add a acc) 
  in aux l [];;  

(* Stratégie avec List.fold_left *)
let exp_somme2 (l: polynome list) : polynome =
  List.fold_left poly_add [] l;;

(* Stratégie naïve itérative *)
let exp_somme3 (l: polynome list) : polynome =
  let result = ref [] in
  let remaining = ref l in
    while !remaining <> [] do
      match !remaining with
      | [] -> ()
      | a :: tl ->
        result := poly_add a !result;
          remaining := tl
    done;
    !result;;
  

let exp_somme (taille:int) =

  let rec aux (n:int) (pas:int) (max:int) (f:string) =
      
    let file = open_out_gen [Open_creat; Open_append; Open_text] 0o666 f in
    Printf.fprintf file "%d;" n;
    close_out file;

    Printf.printf "\nn = %d\n" n;

    let exp_abr = gen_exp n taille in
    let exp_poly = List.map arb2poly exp_abr in

    let somme1 = time_execution exp_somme1 exp_poly f in
    let somme2 = time_execution exp_somme2 exp_poly f in
    let somme3 = time_execution exp_somme3 exp_poly f in

    let file = open_out_gen [Open_creat; Open_append; Open_text] 0o666 f in
    Printf.fprintf file "\t%d;" (List.length somme1);
    Printf.fprintf file "%d;" (List.length somme2);
    Printf.fprintf file "%d\n" (List.length somme3);
    close_out file;

    if n < max then aux (n + pas) pas max f

  in aux 10 100 1000 "exp_somme.txt";;
  
(*exp_somme 20;;*)



(* Question 2.14 *) 

(* Stratégie naïve récursive 1 *)
let exp_produit1 (l: polynome list) : polynome =
  let rec aux (l:polynome list) (acc:polynome) : polynome =
    match l with
    | [] -> acc
    | a :: tl -> aux tl (poly_prod a acc) 
  in aux l [(1,0)];;

(* Stratégie naïve récursive 2 *)
let exp_produit2 (l: polynome list) : polynome =
  
  let poly_prod_bis (p1:polynome) (p2:polynome) : polynome = 
    let monome_prod (m:monome) (p:polynome) = 
      List.map (fun (c, d) -> (c * (fst m), d + (snd m))) p 
    in (List.fold_left (fun acc mon -> poly_add acc (monome_prod mon p2)) [] p1)

  in let rec aux (l:polynome list) (acc:polynome) : polynome =
    match l with
    | [] -> acc
    | a :: tl -> aux tl (poly_prod_bis a acc)
  in canonique(aux l [(1,0)]);;

(* Stratégie avec List.fold_left *)
let exp_produit3 (l: polynome list) : polynome =
  List.fold_left poly_prod [(1,0)] l;;

(* Stratégie naïve itérative *)
let exp_produit4 (l: polynome list) : polynome =
  let result = ref [(1,0)] in 
  let remaining = ref l in  
  while !remaining <> [] do
    match !remaining with
    | [] -> ()
    | a :: tl -> 
      result := poly_prod a !result; 
      remaining := tl 
  done;
  !result;;

(* Stratégie diviser pour reigner *)
let split_at (index:int) (l: polynome list) =
  let rec aux i acc1 acc2 = function
    | [] -> (List.rev acc1, List.rev acc2)
    | x :: xs when i < index -> aux (i + 1) (x :: acc1) acc2 xs
    | x :: xs -> aux (i + 1) acc1 (x :: acc2) xs
  in
  aux 0 [] [] l;;
let rec exp_produit5 (l: polynome list) : polynome =
  match l with
  | [] -> [] 
  | [a] -> a
  | _ ->
    let mid = List.length l / 2 in
    let (l1, l2) = split_at mid l in
    let p1 = exp_produit5 l1 in
    let p2 = exp_produit5 l2 in
    poly_prod p1 p2;;


let exp_produit (taille:int) =

  let rec aux (n:int) (pas:int) (max:int) (f:string) =
    
    let file = open_out_gen [Open_creat; Open_append; Open_text] 0o666 f in
    Printf.fprintf file "%d;" n;
    close_out file;

    Printf.printf "\nn = %d\n" n;
  
    let exp_abr = gen_exp n taille in
    let exp_poly = List.map arb2poly exp_abr in
  
    let produit1 = time_execution exp_produit1 exp_poly f in
    let produit2 = time_execution exp_produit2 exp_poly f in
    (*let produit3 = time_execution exp_produit3 exp_poly f in*)
    let produit4 = time_execution exp_produit4 exp_poly f in
    let produit5 = time_execution exp_produit5 exp_poly f in

    let file = open_out_gen [Open_creat; Open_append; Open_text] 0o666 f in
    Printf.fprintf file "\t%d;" (List.length produit1);
    Printf.fprintf file "%d;" (List.length produit2);
    Printf.fprintf file "%d;" (List.length produit4);
    Printf.fprintf file "%d\n" (List.length produit5);
    close_out file;

    (*assert (produit1 = produit2);
    assert (produit1 = produit3);
    assert (produit1 = produit4);
    assert (produit1 = produit5);*)
  
    if n < max then aux (n + pas) pas max f
  
  in aux 100 100 1000 "exp_produit.txt";;
  
(*exp_produit 20;;*)
