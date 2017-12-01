(*module Set = Fifo;*)
  
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

let rec pere_fils = fun lab (file, b) -> (* prend en parametre le labyrinthe et la file et revoie la file mise a jour *)
	if (b = true) then
		Printf.printf "labyrinthe terminé"
	else
		let (c, f) = Set.take file in
	 	let t = c.walls in
	 	let b = false in
		match t with
	    	[|0; a; b; c|] -> (* test le mur du haut *)
	      	if lab.(x-1).(y).state = Free then
			begin
				lab.(x-1).(y).state = Marked;
			  	(*Set.add lab.(x-1).(y) f*)
				add lab.(x-1).(y) f;
			  	lab.(x-1).(y).father = lab.(x).(y)
			  	lab.(x-1).(y).origin = lab.(x).(y).origin
			end
	      	else if lab.(x-1).(y).state = Marked && lab.(x).(y).origin <> lab.(x-1).(y).origin then
			begin
				lab.(x).(y).state = Path
			  	lab.(x-1).(y).state = Path
			  	b = true
			end
	  	| [|a; 0; b; c|] -> (* test le mur de gauche *)
	      	if lab.(x).(y-1).state = Free then
			begin
				lab.(x).(y-1).state = Marked;
			  	(*Set.add lab.(x).(y-1) f*)
				add lab.(x).(y-1) f
			  	lab.(x).(y-1).father = lab.(x).(y)
			  	lab.(x).(y-1).origin = lab.(x).(y).origin;
			end
	      	else if lab.(x).(y-1).state = Marked && lab.(x).(y).origin <> lab.(x).(y-1).origin then
			begin
				lab.(x).(y).state = Path
			  	lab.(x).(y-1).state = Path
			  	b = true
			end
	  	| [|a; b; 0; c|] -> (* test le mur du bas *)
	      	if lab.(x+1).(y).state = Free then
			begin
				lab.(x+1).(y).state = Marked;
			  	(*Set.add lab.(x+1).(y) f*)
				add lab.(x+1).(y) f
			  	lab.(x+1).(y).father = lab.(x).(y)
			  	lab.(x+1).(y).origin = lab.(x).(y).origin;
			end
	      	else if lab.(x+1).(y).state = Marked && lab.(x).(y).origin <> lab.(x+1).(y).origin then
			begin
				lab.(x).(y).state = Path
			  	lab.(x+1).(y).state = Path
			  	b = true
			end
	  	| [|a; b; c; 0|] -> (* test le mur de droite *)
	      	if lab.(x).(y+1).state = Free then
			begin
				lab.(x).(y+1).state = Marked;
			  	(*Set.add lab.(x).(y+1) f*)
				add lab.(x).(y+1) f
			  	lab.(x).(y+1).father = lab.(x).(y)
			  	lab.(x).(y+1).origin = lab.(x).(y).origin;;
			end
	      	else if lab.(x).(y+1).state = Marked && lab.(x).(y).origin <> lab.(x).(y+1).origin then
			begin
				lab.(x).(y).state = Path
			  	lab.(x).(y+1).state = Path
			  	b = true
			end
		in
		pere_fils lab (f, b);;

 
let () =
	let lab = maze in 
	let (x, y) = (2, 3) in
	lab.(x).(y).origin = Start;
	let (xs, ys) = (1, 1) in
	lab.(xs).(ys).origin = End 1;
	let f = add lab.(x).(y) empty in
	add lab.(xs).(ys) f;
	pere_fils lab (f, false);;

  (*let f = Set.add lab.(x).(y) Set.empty in
  Set.add lab.(xs).(ys) f;;*)
  (* let t = false in
  while t = false do
    (f, t) = pere_fils lab f; *)


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
