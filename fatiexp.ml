type 'a btree =
  | Empty
  | Node of 'a btree * 'a * 'a btree
;;

(*********************************************************************************)
type expression = 
  | Int of int 
  | Pow of char * int 
  | Plus of expression list
  | Mult of expression list 
;;

(*********************************************************************************)
let extraction_alea li la = (* rempacer li et la par L et P*)
  
  let l = List.length li in 
  if l = 0 then
    (li, la) else
    let r= 1+ Random.int l in
    let remove lr indexr =
      let rec aux lr acc i =
        match lr with
        | [] -> List.rev acc 
        | hd :: tl -> if i = indexr then List.rev_append acc tl else aux tl (hd :: acc) (i + 1)
      in
      aux li [] 0 in
    let e =List.nth li (r-1) in ((remove li (r-1)),e::la);;  
  
    
    
let my_list = [10; 20; 30; 40; 50];;
let my_li= [1;3;6];;

(*test me*)                                    
extraction_alea my_list my_li;; 

(**********************************************************)

let gen_permutation n = 
  let rec gen acc i = 
    if i > n then acc
    else gen ((i)::acc) (i + 1)
  in 
  let rec remp li acc = 
    let l = List.length li in
    if l = 0 then acc
    else 
      let new_li, new_acc = extraction_alea li acc in
      remp new_li new_acc
  in
  
  let liste_initiale = gen [] 1 in
  remp liste_initiale [];;

(*1.9 me*)
gen_permutation 4;;

(******************************************************)

let abr (l:int list) : 'a btree = 
  let rec insert_abr (n:int) (a:'a btree) : 'a btree = 
    match a with
    |Empty -> Node(Empty,n,Empty)
    |Node(g,e,d) -> if n=e then Node(g,e,d) else if n<e then Node((insert_abr n g),e,d) else Node(g,e,(insert_abr n d))

  in let rec abr_cons (l:int list) (a:'a btree): 'a btree =
       match l with
       |[]->a
       |h::t -> abr_cons t (insert_abr h a)

  in abr_cons l Empty;;
let x =abr [4;2;3;8;1;9;6;7;5];;
(*****************************************************************************)

let rec etiquetage (a:'a btree) : expression =
  match a with
  |Empty -> if Random.float 1.0 < 0.5 then Int ((Random.int 401)-200) else Pow ('x',1)
  |Node(Empty,e,Empty) -> if e mod 2 = 1 then Mult [Int ((Random.int 401)-200);Pow ('x',1)] else Pow ('x',Random.int 101)
  |Node(g,e,d)-> if Random.float 1.0 < 0.75 then Plus [ etiquetage g;etiquetage d ] else Mult [etiquetage g;etiquetage d]
;;

etiquetage x;;
(******************************************************************************)
let gen_arb (e:expression) : expression = 
  let rec recherche_expression (l:expression list) (n:char): expression list =
    match l with 
    |[]->[]
    |h::t -> if n='M' then match h with
        |Int(x)-> Int(x) :: recherche_expression t n 
        |Pow(c,x)-> Pow(c,x) ::  recherche_expression t n 
        |Plus(v)-> Plus( recherche_expression v 'P') :: recherche_expression t n 
        |Mult(v)-> recherche_expression v n @ recherche_expression t n 
        else match h with 
          |Int(x)-> Int(x) :: recherche_expression t n 
          |Pow(c,x)-> Pow(c,x) ::  recherche_expression t n 
          |Plus(v)-> recherche_expression v n @ recherche_expression t n 
          |Mult(v)-> Mult( recherche_expression v 'M' ) :: recherche_expression t n 
  
  in match e with 
  |Int(x)->Int(x)
  |Pow(c,x)->Pow(c,x)
  |Mult(t)-> Mult(recherche_expression t 'M' )
  |Plus(t)-> Plus(recherche_expression t 'P' )
;;

let l = etiquetage x;;
gen_arb l;;


(**************************************************************yann***********)
(* Question 1.7 *)

(* Question 1.1 *)
type monome = int * int 
and polynome = monome list ;;

(********************************************************yann*)
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

(****************************************************************)
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
(***************************************************************************)
let arb2poly (exp:expression) : polynome = 

  let testPow ((var, value) as p) : char * int =
    if var <> 'x' then raise (Invalid_argument "La variable doit etre x")
    else if value < 0 then raise (Invalid_argument "L'exposant des puissances doit etre positif ou nul") 
    else p
    
  in let rec mult2poly (l:expression list) (m:monome) (is_first_exp:bool) : polynome =
       if List.length l < 2 && is_first_exp then raise (Invalid_argument "Un produit doit contenir au moins 2 expressions")
       else match l with
         | [] -> [(fst m, snd m)]
         | (h::t) -> match h with
           | Int value -> (mult2poly t (value * fst m, snd m) false)
           | Pow (var, value) -> let p = (testPow (var, value)) in (mult2poly t (fst m, snd p + snd m) false)
           | Plus list -> poly_prod (plus2poly list true) (mult2poly t (fst m, snd m) false)
           | Mult _ -> raise (Invalid_argument "Un produit ne peut pas contenir de produits") 
               
  and plus2poly (l:expression list) (is_first_exp:bool) : polynome = 
    if List.length l < 2 && is_first_exp then raise (Invalid_argument "Une somme doit contenir au moins 2 expressions")
    else match l with 
      | [] -> []
      | (h::t) -> match h with
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

let v=gen_arb l;;

arb2poly v

  

  (****************Experimentation*)
let start_time=Sys.time ();;

let methode1 =
  let perm = let rec tab acc i = 
               if i=0 then acc
               else
                 tab ((gen_permutation 5)::acc) (i-1) in tab [] 3
  in 
  let abr=  let rec arbre acc tabr =
              match acc with
              |[]-> tabr
              |a::tl-> arbre tl ((abr a)::tabr) in arbre perm [] in 
  
  let et =let rec etiq acc tab=
            match acc with
            |[]-> tab
            |a::tl-> etiq tl ((etiquetage a)::tab) in etiq abr [] in

  let g=let rec gen acc tab=
          match acc with
          |[]-> tab
          |a::tl-> gen tl ((gen_arb a)::tab) in gen et [] in 
  
  let rec poly2 acc tab=
    match acc with
    |[]-> tab
    |a::tl-> poly2 tl (( arb2poly a)::tab) in poly2 g [];;


  
(**Maniere recursive ne fonctionne pas **)
let rec prod (l: polynome list) (acc:polynome)=
  match l with
  |[]-> acc
  |a::tl -> prod tl (poly_prod a acc) in prod methode1 [(1,0)];;

  
(*Maniere iterative ne fonctionne pas**)
let prod_iter (l: polynome list) : polynome =
  List.fold_left poly_prod [(1, 0)] l ;;

prod_iter methode1;;
(**addition fonctionne**)
let rec add (l:polynome list) (acc:polynome): polynome=
  match l with
  |[]-> acc
  |a::tl-> add tl (poly_add a acc) in add methode1 [(0,0)];;






            




let end_time = Sys.time ();;
let elapsed_time = end_time -. start_time;;




Printf.printf "Le temps d'exécution est de %.4f secondes.\n" elapsed_time;;
