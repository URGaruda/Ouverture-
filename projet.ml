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

(*expl*)

(* Fonction pour afficher un polynôme *)
let afficher_polynome p =
  let rec aux acc = function
    | [] -> acc
    | (coef, deg) :: tl ->
        let monome_str =
          if coef = 0 then ""  (* Ignorer les monômes à coefficient nul *)
          else
            let coef_str = if coef = 1 then "" else string_of_int coef in
            let deg_str = if deg = 0 then "" else "x" ^ (if deg = 1 then "" else "^" ^ string_of_int deg) in
            if deg = 0 then string_of_int coef (* Monome constant *)
            else coef_str ^ deg_str
        in
        let acc_str = if acc = "" then monome_str else (if coef > 0 then " + " else " - ") ^ monome_str in
        aux (acc ^ acc_str) tl
  in
  let result = aux "" p in
  if result = "" then "0" else result;;




// erreur à corriger
let p1 = [(3, 2); (1, 1); (2, 0)]  
let p2 = [(5, 1); (4, 0)]           

let p_sum = poly_add p1 p2
let () = print_endline (afficher_polynome p_sum)  
