open Tk ;;

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

let width = 2;; (*epaisseur paire sinon erreur*)
let length = 10;;     

(*let size = 4;;*)
(*let maze = Array.make 4 (Array.make 4 ({walls = Array.make 4 1; state = Free;
  father = None ; origin = None}) );;*)

(*let maze = [|[|{walls = [|1; 1; 0; 0; 1|]; state = Free};
    {walls = [|1; 0; 1; 0; 1|]; state = Free};
    {walls = [|1; 0; 0; 0; 1|]; state = Free};
    {walls = [|1; 0; 1; 1; 1|]; state = Free}|];
  [|{walls = [|0; 1; 0; 0; 1|]; state = Free};
    {walls = [|1; 0; 0; 1; 1|]; state = Free};
    {walls = [|0; 1; 1; 0; 1|]; state = Free};
    {walls = [|1; 0; 0; 1; 1|]; state = Free}|];
  [|{walls = [|0; 1; 1; 0; 1|]; state = Free};
    {walls = [|0; 0; 1; 0; 1|]; state = Free};
    {walls = [|1; 0; 1; 1; 1|]; state = Free};
    {walls = [|0; 1; 0; 1; 1|]; state = Free}|];
  [|{walls = [|1; 1; 1; 0; 1|]; state = Free};
    {walls = [|1; 0; 1; 0; 1|]; state = Free};
    {walls = [|1; 0; 1; 0; 1|]; state = Free};
    {walls = [|0; 0; 1; 1; 1|]; state = Free}|]|];;*)


(* let maze =
[|[|{walls = [|1; 1; 0; 0|]; state = Free; father = (-1, -1); origin = None};
    {walls = [|1; 0; 0; 0|]; state = Free; father = (-1, -1); origin = None};
    {walls = [|1; 0; 0; 0|]; state = Free; father = (-1, -1); origin = None};
    {walls = [|1; 0; 0; 1|]; state = Free; father = (-1, -1); origin = None};
    {walls = [|1; 1; 0; 1|]; state = Free; father = (-1, -1); origin = None}|];
  [|{walls = [|0; 1; 0; 0|]; state = Free; father = (-1, -1); origin = None};
    {walls = [|0; 0; 0; 0|]; state = Free; father = (-1, -1); origin = None};
    {walls = [|0; 0; 0; 0|]; state = Free; father = (-1, -1); origin = None};
    {walls = [|0; 0; 1; 0|]; state = Free; father = (-1, -1); origin = None};
    {walls = [|0; 0; 1; 1|]; state = Free; father = (-1, -1); origin = None}|];
  [|{walls = [|0; 1; 0; 1|]; state = Free; father = (-1, -1); origin = None};
    {walls = [|0; 1; 0; 1|]; state = Free; father = (-1, -1); origin = None};
    {walls = [|0; 1; 1; 0|]; state = Free; father = (-1, -1); origin = None};
    {walls = [|1; 0; 1; 0|]; state = Free; father = (-1, -1); origin = None};
    {walls = [|1; 0; 1; 1|]; state = Free; father = (-1, -1); origin = None}|];
  [|{walls = [|0; 1; 0; 0|]; state = Free; father = (-1, -1); origin = None};
    {walls = [|0; 0; 1; 0|]; state = Free; father = (-1, -1); origin = None};
    {walls = [|1; 0; 0; 0|]; state = Free; father = (-1, -1); origin = None};
    {walls = [|1; 0; 1; 0|]; state = Free; father = (-1, -1); origin = None};
    {walls = [|1; 0; 0; 1|]; state = Free; father = (-1, -1); origin = None}|];
  [|{walls = [|0; 1; 1; 0|]; state = Free; father = (-1, -1); origin = None};
    {walls = [|1; 0; 1; 0|]; state = Free; father = (-1, -1); origin = None};
    {walls = [|0; 0; 1; 0|]; state = Free; father = (-1, -1); origin = None};
    {walls = [|1; 0; 1; 0|]; state = Free; father = (-1, -1); origin = None};
    {walls = [|0; 0; 1; 1|]; state = Free; father = (-1, -1); origin = None}|]|]*)

   
let width_maze = Array.length maze.(0);;
let height_maze = Array.length maze;;



let top = openTk ();;
Wm.title_set top "Labyrinthe" 
Wm.geometry_set top "500x500"


let c = Canvas.create
    ~width: (length * width_maze + ( width_maze + 1 ) * width)
    ~height: (length * height_maze + ( height_maze + 1 ) * width)
    ~borderwidth:0
    ~relief:`Raised
    top ;;




(* rappel : x horizontal et y vertical*) (*les demi epaisseur pour le visuel*)
let draw_case = fun x y color ->
  let d = width/2 in
  ignore (Canvas.create_polygon
          (*~xys:[(x-d,y-d);  (x-d, y + length+d) ; (x + length+d, y + length+d); (x + length+d, y-d)]*)
          ~xys:[(x,y);  (x, y + length) ; (x + length, y + length); (x + length, y)]
	  ~fill:color
	    c);;
  pack [c];;

let horizontal_wall = fun x y ->
  ignore (Canvas.create_polygon
	    ~xys:[(x,y);  (x + length + 2 *  width, y) ;
                  ( x + length + 2 * width, y + width); (x, y + width)]
	    ~fill:`Black
            c);;
  
let vertical_wall = fun x y ->
  ignore (Canvas.create_polygon
	    ~xys:[(x,y);  (x + width, y) ;
                  ( x + width, y + length + 2 * width); (x , y + length + 2 * width)]
	    ~fill:`Black
	    c);;
  
let horizontal_passage = fun x y color ->
  ignore (Canvas.create_polygon
	    ~xys:[(x,y);  (x + length, y) ;
                  ( x + length, y + width); (x, y + width)]
	    ~fill:color
            c);;
  
let vertical_passage = fun x y color ->
  ignore (Canvas.create_polygon
	    ~xys:[(x,y);  (x + width, y) ;
                  ( x + width, y + length); (x , y + length)]
	    ~fill:color
	    c);;
  
let draw_walls = fun i j color_case -> (* color case est la couleur de la case, si elle est marquée le passage se met de sa couleur *)
  
  let y_case = width * (i+1) + i * length in
  let x_case = width * (j+1) + j * length in
  
  if maze.(i).(j).walls.(0) == 1 then horizontal_wall (x_case - width) (y_case - width) else horizontal_passage (x_case) (y_case - width) color_case;
  if maze.(i).(j).walls.(2) == 1 then horizontal_wall (x_case - width) (y_case + length) else horizontal_passage (x_case) (y_case + length) color_case;
  if maze.(i).(j).walls.(1) == 1 then vertical_wall (x_case - width) (y_case - width) else vertical_passage (x_case-width) (y_case) color_case ;
  if maze.(i).(j).walls.(3) == 1 then vertical_wall (x_case + length) (y_case - width) else vertical_passage (x_case + length) (y_case) color_case;;
      




let change_color = fun i j -> (* on va définir les couleurs comme on veut *)

  let color = `White in
  let y_case = width * (i+1) + i * length in
  let x_case = width * (j+1) + j * length in

  let def_col = fun i j -> match maze.(i).(j).state with
    Free -> `Red
  | Marked -> `Yellow
  | Dead -> `Black
  | Exit -> `Blue
  | Path -> `Green in
  let color = def_col i j in
      
  draw_case (width * (j+1) + j * length) (width * (i+1) + i * length) color;
  if maze.(i).(j).walls.(0) == 0 then horizontal_passage (x_case) (y_case - width) color;
  if maze.(i).(j).walls.(2) == 0 then horizontal_passage (x_case) (y_case + length) color;
  if maze.(i).(j).walls.(1) == 0 then vertical_passage (x_case - width) (y_case) color;
  if maze.(i).(j).walls.(3) == 0 then vertical_passage (x_case + length) (y_case) color;;



	
  

  

for i = 0 to height_maze - 1 do
  for j = 0 to width_maze - 1 do
    draw_case (width * (j+1) + j * length) (width * (i+1) + i * length) `White;
    draw_walls i j `White;
  done
done


  
let _ = Printexc.print mainLoop ();;

(*let b = Button.create
    ~text:"Change color 1 0"
    ~command:(fun () ->
      change_color 1 0)
    top ;;
pack [b] ;;*)



