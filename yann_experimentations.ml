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
  
  in let per = snd (aux (gen_liste 1) []) in 
  (*let file = open_out_gen [Open_creat; Open_append; Open_text] 0o666 "gen_permutation.txt" in
  in snd (aux (gen_liste 1) []);;
  Printf.fprintf file "%d:%s\n" n (String.concat ";" (List.map string_of_int per));
  close_out file;*)
  per;;



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

let exper_gen_abrs (n:int) (taille:int) : expression list =
  
  let rec gen_permutations (n:int) : int list list =
    if n <= 0 then []
    else (gen_permutation taille) :: (gen_permutations (n - 1))

  in List.map gen_arb (List.map etiquetage (List.map abr (gen_permutations n)));;


let rec exper_gen_abrs_20 (n:int) (pas:int) (max:int) : expression list list =
  if n > max then []
  else 
    let file = open_out_gen [Open_creat; Open_append; Open_text] 0o666 "export/exper_gen_abrs_20.txt" in
    let start_time = Sys.time () in
    let exper_abr = (exper_gen_abrs n 20) in
    let end_time = Sys.time () in
    let duration = end_time -. start_time in
    Printf.fprintf file "%d:%f\n" n duration;
    close_out file;
    exper_abr::(exper_gen_abrs_20 (n + pas) pas max);;

(*let exper_polys = List.map (fun l -> List.map arb2poly l) (exper_gen_abrs_20 100 100 1000);;*)



(* Fonction utile pour les expérimentations *)
let time_execution fct arg f is_end =
  let file = open_out_gen [Open_creat; Open_append; Open_text] 0o666 f in
  let start_time = Sys.time () in
  let result = fct arg in
  let end_time = Sys.time () in
  let duration = end_time -. start_time in
  Printf.fprintf file "%f,%d" duration (List.length result);
  if is_end then Printf.fprintf file "\n" else Printf.fprintf file ";";
  close_out file;
  result;;



(* Question 2.14 *) 

(* Stratégie naïve récursive *)
let exper_somme1 (l: polynome list) : polynome =
  let rec aux (l:polynome list) (acc:polynome) : polynome =
    match l with
    | [] -> acc
    | a :: tl -> aux tl (poly_add_canonique a acc) 
  in aux l [];;  

(* Stratégie avec List.fold_left *)
let exper_somme2 (l: polynome list) : polynome =
  List.fold_left poly_add_canonique [] l;;

(* Stratégie naïve itérative *)
let exper_somme3 (l: polynome list) : polynome =
  let result = ref [] in
  let remaining = ref l in
    while !remaining <> [] do
      match !remaining with
      | [] -> ()
      | a :: tl ->
        result := poly_add_canonique a !result;
          remaining := tl
    done;
    !result;;

(* Test du bon fonctionnement des stratégies de somme *)
let exper_test_polys = [[(5,0); (1,1); (8,2); (12,3)]; [(7,0); (3,1); (2,2); (8,3)]; [(5,0); (1,1); (3,2); (6,3); (2,4)]];;
let exper_test_polys_add = [(17,0); (5,1); (13,2); (26,3); (2,4)];;

let () =
  assert(exper_somme1 exper_test_polys = exper_test_polys_add);
  assert(exper_somme2 exper_test_polys = exper_test_polys_add);
  assert(exper_somme3 exper_test_polys = exper_test_polys_add);;


(* Mesure des temps d'exécution pour les stratégies de somme *)
let rec exper_somme (pll:polynome list list) (f:string) =
  match pll with
  | [] -> ()
  | pl :: l -> let n = List.length pl in
    let file = open_out_gen [Open_creat; Open_append; Open_text] 0o666 f in
    Printf.fprintf file "%d:" n;
    close_out file;
  
    (* Application des stratégies de somme *)
    let somme1 = time_execution exper_somme1 pl f false in
    let somme2 = time_execution exper_somme2 pl f false in
    let somme3 = time_execution exper_somme3 pl f true in
  
    assert (somme1 = somme2);
    assert (somme2 = somme3);
    assert (List.length somme1 > 0);
  
    exper_somme l f;;     
        
(*(exper_somme exper_polys "export/exper_somme.txt");;*)



(* Question 2.15 *) 

(* Stratégie naïve récursive 1 *)
let exper_produit1 (l: polynome list) : polynome =
  let rec aux (l:polynome list) (acc:polynome) : polynome =
    match l with
    | [] -> acc
    | a :: tl -> aux tl (poly_prod_canonique a acc) 
  in aux l [(1,0)];;

(* Stratégie naïve récursive 2 *)
let exper_produit2 (l: polynome list) : polynome =
  
  let rec aux (l:polynome list) (acc:polynome) : polynome =
    match l with
    | [] -> acc
    | a :: tl -> aux tl (poly_prod a acc)
  in canonique(aux l [(1,0)]);;

(* Stratégie naïve itérative *)
let exper_produit3 (l: polynome list) : polynome =
  let result = ref [(1,0)] in 
  let remaining = ref l in  
  while !remaining <> [] do
    match !remaining with
    | [] -> ()
    | a :: tl -> 
      result := poly_prod_canonique a !result; 
      remaining := tl 
  done;
  !result;;

(* Stratégie diviser pour régner *)
let exper_produit4 (l: polynome list) : polynome =

  let rec divide_and_conquer (l:polynome list) (debut:int) (longueur:int) : polynome =
    if longueur = 0 then []
    else if longueur = 1 then List.nth l debut
    else
      let milieu = longueur / 2 in
      let p1 = divide_and_conquer l debut milieu in
      let p2 = divide_and_conquer l (debut + milieu) (longueur - milieu) in
      poly_prod_canonique p1 p2
  
  in divide_and_conquer l 0 (List.length l);;

(* Test du bon fonctionnement des stratégies de produit *)
let exper_test_polys_prod = [(175,0); (145,1); (472,2); (1095,3); (859,4); (1408,5); (1786,6); (1020,7); (936,8); (752,9); (192,10)];;

let () =
  assert (exper_produit1 exper_test_polys = exper_test_polys_prod);
  assert (exper_produit2 exper_test_polys = exper_test_polys_prod);
  assert (exper_produit3 exper_test_polys = exper_test_polys_prod);
  assert (exper_produit4 exper_test_polys = exper_test_polys_prod);;


(* Mesure des temps d'exécution pour les stratégies de produit *)
let rec exper_produit (pll:polynome list list) (f:string) (debug:bool) =
  match pll with
  | [] -> ()
  | pl :: l -> 
    
    let n = List.length pl in
    let file = open_out_gen [Open_creat; Open_append; Open_text] 0o666 f in

    (* Logs de debug *)
    if debug then (Printf.fprintf file "\n\nListe des polynomes a multiplier pour n=%d :\n\n" n;
    Printf.fprintf file "[";
    List.iter (fun p -> Printf.fprintf file "%s ; " (String.concat "" (List.map (fun (c, d) -> Printf.sprintf "(%d,%d)" c d) p))) pl;
    Printf.fprintf file "]\n\n");

    Printf.fprintf file "%d:" n;
    close_out file;

    (* Application des stratégies de produit *)
    let produit1 = time_execution exper_produit1 pl f false in
    let produit2 = time_execution exper_produit2 pl f false in
    let produit3 = time_execution exper_produit3 pl f false in
    let produit4 = time_execution exper_produit4 pl f true in

    (* Logs de debug *)
    if debug then (let file = open_out_gen [Open_creat; Open_append; Open_text] 0o666 f in
    Printf.fprintf file "\nResultat de la multiplication :\n\n[";
    Printf.fprintf file "%s;" (String.concat "" (List.map (fun (c, d) -> Printf.sprintf "(%d,%d)" c d) produit1));
    Printf.fprintf file "]\n\n";
    close_out file);
  
    assert (produit1 = produit2);
    assert (produit1 = produit3);
    assert (produit1 = produit4);
    assert (List.length produit1 > 0);
  
    exper_produit l f debug;;     
        
(*(exper_produit exper_polys "export/exper_produit.txt" false);;*)



(* Question 2.16 *)

let rec exper_gen_abr_15 (pow_max:int) : expression list =

  let rec power_positif (i:int) (n:int) : int = 
    if n <= 0 then 1
    else if n = 1 then i
    else i * (power_positif i (n - 1))

  in let rec aux (n:int) (max:int) : expression list =
    if n > max then []
    else 
      let nbr = (power_positif 2 n) in
      let file = open_out_gen [Open_creat; Open_append; Open_text] 0o666 "export/exper_gen_abr_15.txt" in
      let start_time = Sys.time () in
      let exper_abr = (exper_gen_abrs 1 nbr) in
      let end_time = Sys.time () in
      let duration = end_time -. start_time in
      Printf.fprintf file "%d:%f\n" nbr duration;
      close_out file;
      exper_abr@(aux (n + 1) max)
  
  in (aux (-1) pow_max);;


(*let exper_polys_15 = List.map arb2poly (exper_gen_abr_15 13);;*)



(* Question 2.17 *)

(*let exper_somme_15 = exper_somme [exper_polys_15] "export/exper_somme_15.txt";;*)



(* Question 2.18 *)

(*let exper_produit_15 = exper_produit [exper_polys_15] "export/exper_produit_15.txt" false;;*)
