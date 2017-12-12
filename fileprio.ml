(* File de priorité *)

(* Objectif: on ajoute dans la file les voisins du noeud en cours, la priorité étant au voisin de f minimum, on définit f comme la somme de la distance déjà parcourue g et de la distance de manhattan à la cible *)

type ('a,'b) t =
    Node of ('a,'b) node
  | Nil
and ('a,'b) node = {
    key: 'a;
    data: 'b;
    left: ('a,'b) t;
    right: ('a,'b) t}
;;

exception Empty;;

let empty = Nil;;
let is_empty t = t = Nil ;;

let make k d l r = Node {key = k; data = d; left= l; right = r };;

let rec insert x v t =
  match t with
    Nil -> Node {key=x; data=v; left=Nil; right=Nil}
  | Node {key= y; data=d; left=l; right=r} ->
      if x<y then
        make x v (insert y d r) l
      else 
        make y d (insert x v r) l
;;

let root t =
  match t with
    Nil -> raise Empty
  | Node {key= k; data=d; left=l; right=r} -> (k,d)

let rec remove  t =
  match t with
    Nil -> raise Empty
  | Node {left=Nil; right=r} -> r
  | Node {left=l; right=Nil} -> l
  | Node {left=l; right=r} ->
      let (x,v)= root l and (y,w)= root r in
      if x < y  then  make x v (remove l) r
      else make y w l (remove p r)
      
let extract  t =
  let (prio,x)= root t in
  (prio, x, remove t);;

let rec elements q =
  if is_empty q then []
  else
    let (_,x,new_q)= extract q in
    x:: elements  new_q;;

let rec cardinal = function
    Nil -> 0
  | Node {left= l; right= r} -> cardinal l + 1 + cardinal r;;

let rec iter f = function
    Nil -> ()
  | Node {key=k; data=d; left=l; right=r} ->
      iter f l; f k d; iter f r;;
