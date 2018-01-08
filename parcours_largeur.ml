module Gui =  Interface
module G = Generation
  
(*type state =
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
   };;*)

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
	| (_,true) -> (laby, file, true) (* sortie trouvée *)
	| (-1,false) -> (laby, file, false)  
	| (d,_) -> if laby.(x).(y).G.walls.(d) = 0 then (*pas de mur*)
			begin 
			let (xv,yv) = coord_voisin x y dir in
			match laby.(xv).(yv).G.state with 
			| G.Free -> laby.(xv).(yv).G.state <- G.Marked;
				let f = add (xv, yv) file in
			  	laby.(xv).(yv).G.father <- (x, y);
			  	laby.(xv).(yv).G.origin <- laby.(x).(y).G.origin;
				Gui.change_color xv yv;
				test_voisins laby f x y (dir-1) false
			| G.Marked -> 
				if laby.(x).(y).G.origin = laby.(xv).(yv).G.origin then test_voisins laby file x y (dir-1) false
				else 
					begin 
					laby.(x).(y).G.state <- G.Path;
					laby.(xv).(yv).G.state <- G.Path;
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



let maze = [|[|{G.walls = [|1; 1; 0; 1|]; G.state = G.Free; G.father = (-1, -1); G.origin = None};
               {G.walls = [|1; 1; 0; 1|]; G.state = G.Free; G.father = (-1, -1); G.origin = None};
               {G.walls = [|1; 1; 1; 0|]; G.state = G.Free; G.father = (-1, -1); G.origin = None};
               {G.walls = [|1; 0; 0; 1|]; G.state = G.Free; G.father = (-1, -1); G.origin = None}|];
             [|{G.walls = [|0; 1; 0; 0|]; G.state = G.Free; G.father = (-1, -1); G.origin = None};
               {G.walls = [|0; 0; 1; 1|]; G.state = G.Free; G.father = (-1, -1); G.origin = None};
               {G.walls = [|1; 1; 0; 0|]; G.state = G.Free; G.father = (-1, -1); G.origin = None};
               {G.walls = [|0; 0; 1; 1|]; G.state = G.Free; G.father = (-1, -1); G.origin = None}|];
             [|{G.walls = [|0; 1; 0; 1|]; G.state = G.Free; G.father = (-1, -1); G.origin = None};
               {G.walls = [|1; 1; 1; 0|]; G.state = G.Free; G.father = (-1, -1); G.origin = None};
               {G.walls = [|0; 0; 1; 0|]; G.state = G.Free; G.father = (-1, -1); G.origin = None};
               {G.walls = [|1; 0; 0; 1|]; G.state = G.Free; G.father = (-1, -1); G.origin = None}|];
             [|{G.walls = [|0; 1; 1; 0|]; G.state = G.Free; G.father = (-1, -1); G.origin = None};
               {G.walls = [|1; 0; 1; 0|]; G.state = G.Free; G.father = (-1, -1); G.origin = None};
               {G.walls = [|1; 0; 1; 0|]; G.state = G.Free; G.father = (-1, -1); G.origin = None};
               {G.walls = [|0; 0; 1; 1|]; G.state = G.Free; G.father = (-1, -1); G.origin = None}|]|];;

let solve = fun lab start finish -> 
	let (x, y) = start in
	lab.(x).(y).G.origin <- Start;
	let (xs, ys) = finish in
	lab.(xs).(ys).G.origin <- End 1;
	let f = add (x, y) empty in
	let f = add (xs, ys) f in
	pere_fils lab f false;
	lab;;

