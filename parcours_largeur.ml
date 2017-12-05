(*module Set = Fifo;*)
module Gui =  Interface
  
type state =
    Free
  | Marked
  | Dead
  | Exit
  | Path;;

type origin =
    None
  | Start
  | End of int;;

type case = {
    walls : int array;
    mutable state : state;
    mutable father : int * int;
    mutable origin : origin
   };;

type 'a t = 'a list * 'a list

exception Empty

let empty = ([], [])

let is_empty = fun q ->
  q = empty

let add = fun x (front, back) ->
  (x::front, back)

let rec take = fun q ->
  match q with
    ([], []) -> raise Empty
  | (front, []) -> take ([], List.rev front)
  | (front, x :: back) -> (x, (front, back))

let coord_voisin = fun i j dir -> 
	match dir with 
	| 0 -> (i-1,j)
	| 1 -> (i,j-1)
 	| 2 -> (i+1,j)
 	| _ -> (i,j+1);;

let rec test_voisins = fun laby file x y dir boo ->
	match (dir, boo) with 
	| (_,true) -> (laby, file, true)
	| (-1,false) -> (laby, file, false)  
	| (d,_) -> if laby.(x).(y).walls.(d) = 0 then (*pas de mur*)
			begin 
			let (xv,yv) = coord_voisin x y dir in
			match laby.(xv).(yv).state with 
			| Free -> laby.(xv).(yv).state <- Marked;
				let f = add (xv, yv) file in
			  	laby.(xv).(yv).father <- (x, y);
			  	laby.(xv).(yv).origin <- laby.(x).(y).origin;
				Gui.change_color xv yv;
				test_voisins laby f x y (dir-1) false
			| Marked -> 
				if laby.(x).(y).origin = laby.(xv).(yv).origin then test_voisins laby file x y (dir-1) false
				else 
					begin 
					laby.(x).(y).state <- Path;
					laby.(xv).(yv).state <- Path;
					Gui.change_color xv yv;
					Gui.change_color x y;
					test_voisins laby file x y dir true
					end
			| _ -> test_voisins laby file x y (dir-1) false
			end
		else test_voisins laby file x y (dir-1) false;; (* il y a un mur*)

let rec pere_fils = fun lab f b -> 
	match b with 
	| true -> ()
	| false -> let ((x, y), f) = take f in let (lab,file,boo) = test_voisins lab f x y 3 false in pere_fils lab file boo;;


(*let rec pere_fils = fun lab file b -> (* prend en parametre le labyrinthe et la file et revoie la file mise a jour *)
	if (b = true) then
		Printf.printf "labyrinthe terminé"
	else
		let b = ref false in
		let ((x, y), f) = take file in
		(*let (c, f) = Set.take file in*)
	 	let t = lab.(x).(y).walls in
		if t.(0) = 0 then (* test le mur du haut *)
			begin
			      	if lab.(x-1).(y).state = Free then
					begin
						lab.(x-1).(y).state <- Marked;
					  	(*Set.add lab.(x-1).(y) f*)
						let f = add ((x-1), y) f in
					  	lab.(x-1).(y).father <- (x, y);
					  	lab.(x-1).(y).origin <- lab.(x).(y).origin
					end
			      	else if lab.(x-1).(y).state = Marked && lab.(x).(y).origin <> lab.(x-1).(y).origin then
					begin
						lab.(x).(y).state <- Path;
					  	lab.(x-1).(y).state <- Path;
					  	b := true
					end
			end;
		if t.(1) = 0 then (* test le mur de gauche *)
			begin
			      	if lab.(x).(y-1).state = Free then
					begin
						lab.(x).(y-1).state <- Marked;
					  	(*Set.add lab.(x).(y-1) f*)
						let f = add (x, (y-1)) f in
					  	lab.(x).(y-1).father <- (x, y);
					  	lab.(x).(y-1).origin <- lab.(x).(y).origin
					end
			      	else if lab.(x).(y-1).state = Marked && lab.(x).(y).origin <> lab.(x).(y-1).origin then
					begin
						lab.(x).(y).state <- Path;
					  	lab.(x).(y-1).state <- Path;
					  	b := true
					end
			end;
		if t.(2) = 0 then (* test le mur du bas *)
			begin
			      	if lab.(x+1).(y).state = Free then
					begin
						lab.(x+1).(y).state <- Marked;
					  	(*Set.add lab.(x+1).(y) f*)
						let f = add ((x+1),y) f in
					  	lab.(x+1).(y).father <- (x, y);
					  	lab.(x+1).(y).origin <- lab.(x).(y).origin
					end
			      	else if lab.(x+1).(y).state = Marked && lab.(x).(y).origin <> lab.(x+1).(y).origin then
					begin
						lab.(x).(y).state <- Path;
					  	lab.(x+1).(y).state <- Path;
					  	b := true
					end
			end;
		if t.(3) = 0 then (* test le mur de droite *)
			begin
			      	if lab.(x).(y+1).state = Free then
					begin
						lab.(x).(y+1).state <- Marked;
					  	(*Set.add lab.(x).(y+1) f*)
						let f = add (x,(y+1)) f in
					  	lab.(x).(y+1).father <- (x, y);
					  	lab.(x).(y+1).origin <- lab.(x).(y).origin
					end
			      	else if lab.(x).(y+1).state = Marked && lab.(x).(y).origin <> lab.(x).(y+1).origin then
					begin
						lab.(x).(y).state <- Path;
					  	lab.(x).(y+1).state <- Path;
					  	b := true
					end
			end;
		pere_fils lab f !b;;*)

let maze = [|[|{walls = [|1; 1; 0; 1|]; state = Free; father = (-1, -1); origin = None};
    {walls = [|1; 1; 0; 1|]; state = Free; father = (-1, -1); origin = None};
    {walls = [|1; 1; 1; 0|]; state = Free; father = (-1, -1); origin = None};
    {walls = [|1; 0; 0; 1|]; state = Free; father = (-1, -1); origin = None}|];
  [|{walls = [|0; 1; 0; 0|]; state = Free; father = (-1, -1); origin = None};
    {walls = [|0; 0; 1; 1|]; state = Free; father = (-1, -1); origin = None};
    {walls = [|1; 1; 0; 0|]; state = Free; father = (-1, -1); origin = None};
    {walls = [|0; 0; 1; 1|]; state = Free; father = (-1, -1); origin = None}|];
  [|{walls = [|0; 1; 0; 1|]; state = Free; father = (-1, -1); origin = None};
    {walls = [|1; 1; 1; 0|]; state = Free; father = (-1, -1); origin = None};
    {walls = [|0; 0; 1; 0|]; state = Free; father = (-1, -1); origin = None};
    {walls = [|1; 0; 0; 1|]; state = Free; father = (-1, -1); origin = None}|];
  [|{walls = [|0; 1; 1; 0|]; state = Free; father = (-1, -1); origin = None};
    {walls = [|1; 0; 1; 0|]; state = Free; father = (-1, -1); origin = None};
    {walls = [|1; 0; 1; 0|]; state = Free; father = (-1, -1); origin = None};
    {walls = [|0; 0; 1; 1|]; state = Free; father = (-1, -1); origin = None}|]|];;

let solve = fun lab start finish -> 
	let (x, y) = start in
	lab.(x).(y).origin <- Start;
	let (xs, ys) = finish in
	lab.(xs).(ys).origin <- End 1;
	let f = add (x, y) empty in
	let f = add (xs, ys) f in
	pere_fils lab f false;;

  (*let f = Set.add lab.(x).(y) Set.empty in
  Set.add lab.(xs).(ys) f;;*)
  (* let t = false in
  while t = false do
    (f, t) = pere_fils lab f; *)

