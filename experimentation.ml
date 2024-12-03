(*open Polynome ;;
open Fonction ;;
open Arbre ;;
open Abr;;*)


(*1.3*)
let poly_add p1 p2 =
  let rec ajouter_polynomes p1 p2 acc =
    match p1, p2 with
    | [], [] -> acc  (* Si les deux listes sont vides, on renvoie l'accumulateur *)
    | (c1, d1) :: t1, [] -> (c1, d1) :: acc @ t1  (* Si p2 est vide, ajouter le reste de p1 *)
    | [], (c2, d2) :: t2 -> (c2, d2) :: acc @ t2  (* Si p1 est vide, ajouter le reste de p2 *)
    | (c1, d1) :: t1, (c2, d2) :: t2 when d1 = d2 ->  (* Si les degrés sont égaux, additionner les coefficients *)
        let nouveau_coef = c1 + c2 in
        if nouveau_coef = 0 then ajouter_polynomes t1 t2 acc  (* Si le coefficient est nul, ignorer *)
        else ajouter_polynomes t1 t2 ((nouveau_coef, d1) :: acc)
    | (c1, d1) :: t1, (c2, d2) :: t2 when d1 < d2 ->  (* Si le degré de p1 est plus petit que celui de p2 *)
        ajouter_polynomes t1 p2 ((c1, d1) :: acc)
    | (c1, d1) :: t1, (c2, d2) :: t2 ->  (* Si le degré de p2 est plus petit que celui de p1 *)
        ajouter_polynomes p1 t2 ((c2, d2) :: acc)
  in
  (* Appeler la fonction récursive*)
  let resultat = ajouter_polynomes p1 p2 [] in
  (* Trier le polynôme résultant*)
  List.sort (fun (_, d1) (_, d2) -> compare d1 d2) resultat;;

(*1.4*)
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

  (*1.8*)
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

(*1.9*)
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

(*2.13*)

let rec generate_expr (n:int) : expression list =
  if n<0 then [] 
  else (gen_arb (etiquetage (abr (gen_permutation 20 ) )))::generate_expr (n-1)
;;

(*2.14*)

let somme_abr (a:expression list) : polynome = 
  let rec somme_poly (l:polynome list ) : polynome =
    match l with 
    |[]->[]
    |h::[]-> h
    |h::i::t -> somme_poly((poly_add h i)::t )
  in 
  let rec list_to_canonique (b:expression list) : polynome list =
    match b with 
    |[]->[]
    |h::t-> (canonique (arb2poly h) )::list_to_canonique t

  in somme_poly (list_to_canonique a )
;;

let mult_abr (a:expression list) : polynome = 
  let rec mult_poly (l:polynome list ) : polynome =
    match l with 
    |[]->[]
    |h::[]-> h
    |h::i::t -> mult_poly((poly_prod h i)::t )
  in 
  let rec list_to_canonique (b:expression list) : polynome list =
    match b with 
    |[]->[]
    |h::t-> (canonique (arb2poly h) )::list_to_canonique t

  in mult_poly (list_to_canonique a )
;;

(*experimentation de fati*)
let start_time=Sys.time ();;

let methode1 =
  let perm = let rec tab acc i = 
               if i=0 then acc
               else
                 tab ((gen_permutation 20)::acc) (i-1) in tab [] 1000
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


  
(**Maniere recursive**)
let rec prod (l: polynome list) (acc:polynome)=
  match l with
  |[]-> acc
  |a::tl -> prod tl (poly_prod a acc) in prod methode1 [(1,0)];;

  
(*Maniere iterative**)

let x = [(1,0)] ;;
let prod_iter (l: polynome list) : polynome =
  List.fold_left (fun x y -> poly_prod x y) x l ;;

  prod_iter methode1;;

let end_time = Sys.time ();;
let elapsed_time = end_time -. start_time;;
  
Printf.printf "Le temps d'exécution est de %.4f secondes.\n" elapsed_time;;



