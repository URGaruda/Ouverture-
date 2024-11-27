(* Question 1.1 *)
type monome = int * int 
and polynome = monome list ;;



(* 1.2 renvoie sa forme canonique *)
let canonique p =
  (* Fonction auxiliaire pour combiner les monômes de même degré *)
  let rec combiner_monomes acc = function
    | [] -> acc
    | (c, d) :: tl ->
        let rec ajouter_ou_mettre_a_jour acc =
          match acc with
          | [] -> [(c, d)]  (* Si l'accumulateur est vide, on ajoute le monome *)
          | (coef, deg) :: tl when deg = d -> (coef + c, deg) :: tl  (* Si le degré existe, additionner les coefficients *)
          | hd :: tl -> hd :: ajouter_ou_mettre_a_jour tl
        in
        combiner_monomes (ajouter_ou_mettre_a_jour acc) tl
  in

  (*Combiner les monômes avec le même degré et supprimer ceux avec un coefficient nul *)
  let p_combined = List.filter (fun (coef, _) -> coef != 0) (combiner_monomes [] p) in

  (* Trier les monômes par degré croissant *)
  List.sort (fun (_, d1) (_, d2) -> compare d1 d2) p_combined;;
  
  
  
  (* 1.3 Fonction poly_add qui additionne deux polynômes canoniques *)
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


(* 1.6*)

  let expr= Plus([Mult([Int 123;Pow('x',1)]);Int 42;Pow('x',3)]);;




(* 1.7*)

(* car mult et plus ont des contraintes de type*)
type mult_exclusion=
  |Int of int
  |Pow of char * int ;;

type plus_exclusion= 
  |Int of int
  |Pow of char * int 
  |Mult of mult_exclusion list;;

type expression =
  |Int of int
  |Pow of char * int 
  |Plus of plus_exclusion list
  |Mult of mult_exclusion list;;


exception Entier_neg;;

let arb2poly (abr:expression)= 
  let tree=function 
    |Int(x)->if x>=0 then [((x,0):monome)] else raise Entier_neg
    |Pow('x',a)-> [((1,a):monome)]
    |Mult([Int(x);Pow('x',a)]) | Mult([Pow('x',a);Int(x)])-> if x>=0&& a>0 then [((x,a):monome)] else raise Entier_neg
    |Plus(a::q)-> let rec pls (li:plus_exclusion list) =
                    match li with 
                    |Int(x)::tl -> if x>=0 then ((x,0):monome)::pls tl else raise Entier_neg
                    |Pow('x',a)::tl-> if a>=0 then ((1,a):monome)::pls tl else raise Entier_neg
                    |Mult([Int(x);Pow('x',a)])::tl | Mult([Pow('x',a);Int(x)])::tl-> if a>=0&& x>0 then((x,a):monome)::pls tl else raise Entier_neg
                    |_->((0,0):monome)::[]
                                                     
        in 
        let aux = (* pour le premier terme*)
          match a with
          | Int(x) ->if x>=0 then  [((x, 0):monome)] else raise Entier_neg
          | Pow('x', a) ->if a>=0 then[((1, a):monome)] else raise Entier_neg
          | Mult([Int(x); Pow('x', a)]) -> if a>=0&& x>0 then [((x, a):monome)] else raise Entier_neg
          |_->((0,0):monome)::[]

        in
        aux @ pls q
  in canonique(tree abr);; 

(* exple*)

arb2poly(expr);;


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
  
    
    
let my_list = [10; 20; 30; 40; 50];;
let my_li= [1;3;6];;

(*test*)                                    
extraction_alea my_list my_li;; 


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

(*1.9*)
gen_permutation 4;;


