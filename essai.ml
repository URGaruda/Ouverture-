(* Ex 1.2 de Daba *)
let rearrangements (l : (int * int) list ref) (p : int) (r : int) : int =
  let t = Array.of_list !l in  
  let v = snd t.(r) in            
  let i = ref p in                

  for j = p to (r - 1) do
    if snd t.(j) <= v then begin
      let tmp = t.(!i) in          
      t.(!i) <- t.(j);             
      t.(j) <- tmp;
      i := !i + 1                  
    end
  done;

  let tmp = t.(!i) in
  t.(!i) <- t.(r);
  t.(r) <- tmp;

  l := Array.to_list t;  

  !i
;;

let rec tri_rapide (l : (int * int) list ref) (p : int) (r : int) =
  if p < r then begin
    let q = rearrangements l p r in 
    tri_rapide l p (q - 1);       
    tri_rapide l (q + 1) r;
  end
;;


let trie_croissant_polynome (l : (int * int) list) : (int * int) list =
  let t = ref l in  
  tri_rapide t 0 ((List.length !t) - 1);  
  !t  
;;


let rec fusionne_monome_rec (l: (int*int) list) (c:int) (d:int) : int =
  match l with 
  |[]->c
  |(a,b)::t -> if b=d then fusionne_monome_rec t (c+a) d else (fusionne_monome_rec t c d)  
;;

let fusionne_monome (l: (int*int) list) (m: (int*int)) : (int*int) list = 
    (fusionne_monome_rec l (0) (snd m),(snd m))::(List.filter (fun x -> (snd x)!=(snd m) ) l) 
;;

let rec fusionne_poly (l: (int*int) list) : (int*int) list = 
  match l with 
  |[]->[]
  |(c,d)::t -> (fusionne_monome_rec l (0) (d),(d))::(fusionne_poly (List.filter (fun x -> (snd x)!=(d) ) l) )

;;

let canonique (l:(int*int) list) :(int*int) list = 
  (List.filter (fun x -> (fst x)!=0 ) (fusionne_poly (trie_croissant_polynome l)) )
;;


assert((fusionne_poly [(3, 2); (4, 1); (1, 2); (5, 0)])=[(4, 2); (4, 1); (5, 0)]);;
assert((fusionne_poly [(3, 0); (2, 1); (1, 2)])=[(3, 0); (2, 1); (1, 2)]);;  
assert((fusionne_poly [])=[]);;  
assert((fusionne_poly [(7, 3)])=[(7, 3)]);;  
assert((fusionne_poly [(4, 3); (2, 1); (3, 3); (1, 2); (2, 1)])= [(7, 3); (4, 1); (1, 2)] );;
assert((fusionne_poly [(1, 0); (2, 3); (3, 2); (4, 3); (1, 2); (5, 0)])= [(6, 0); (6, 3); (4, 2)]);;
assert((fusionne_poly [(1, 5); (2, 5); (3, 5); (4, 5)])= [(10, 5)]);;


