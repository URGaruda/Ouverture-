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
    |Node(g,e,d) -> if n<e then Node((insert_abr n g),e,d) else Node(g,e,(insert_abr n d))

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

abr [4;2;3;8;1;9;6;7;5] ;; 
assert(is_abr (abr [4;2;3;8;1;9;6;7;5] )=true);;
assert(is_abr (abr [4;8;3;2;1;9;6;7;5] )=true);;
assert(is_abr (abr [4;2;3;8;6;7;5] )=true);;
assert(is_abr (abr [4;2;3;8;1;9;6;7;5;11;24;15;48;32;42] )=true);;