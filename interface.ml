open Tk;;
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

(*type case = Generation.case;;*)

module G = Generation
let width = 8 ;; (*epaisseur paire sinon risque erreur*)
let length = 40;;


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


let maze =
  [|[|{G.walls = [|1; 1; 0; 0|]; G.state = G.Free; G.father = (-1, -1); G.origin = None};
      {G.walls = [|1; 0; 0; 0|]; G.state = G.Free; G.father = (-1, -1); G.origin = None};
      {G.walls = [|1; 0; 0; 0|]; G.state = G.Free; G.father = (-1, -1); G.origin = None};
      {G.walls = [|1; 0; 0; 1|]; G.state = G.Free; G.father = (-1, -1); G.origin = None};
      {G.walls = [|1; 1; 0; 1|]; G.state = G.Free; G.father = (-1, -1); G.origin = None}|];
    [|{G.walls = [|0; 1; 0; 0|]; G.state = G.Free; G.father = (-1, -1); G.origin = None};
      {G.walls = [|0; 0; 0; 0|]; G.state = G.Free; G.father = (-1, -1); G.origin = None};
      {G.walls = [|0; 0; 0; 0|]; G.state = G.Free; G.father = (-1, -1); G.origin = None};
      {G.walls = [|0; 0; 1; 0|]; G.state = G.Free; G.father = (-1, -1); G.origin = None};
      {G.walls = [|0; 0; 1; 1|]; G.state = G.Free; G.father = (-1, -1); G.origin = None}|];
    [|{G.walls = [|0; 1; 0; 1|]; G.state = G.Free; G.father = (-1, -1); G.origin = None};
      {G.walls = [|0; 1; 0; 1|]; G.state = G.Free; G.father = (-1, -1); G.origin = None};
      {G.walls = [|0; 1; 1; 0|]; G.state = G.Free; G.father = (-1, -1); G.origin = None};
      {G.walls = [|1; 0; 1; 0|]; G.state = G.Free; G.father = (-1, -1); G.origin = None};
      {G.walls = [|1; 0; 1; 1|]; G.state = G.Free; G.father = (-1, -1); G.origin = None}|];
    [|{G.walls = [|0; 1; 0; 0|]; G.state = G.Free; G.father = (-1, -1); G.origin = None};
      {G.walls = [|0; 0; 1; 0|]; G.state = G.Free; G.father = (-1, -1); G.origin = None};
      {G.walls = [|1; 0; 0; 0|]; G.state = G.Free; G.father = (-1, -1); G.origin = None};
      {G.walls = [|1; 0; 1; 0|]; G.state = G.Free; G.father = (-1, -1); G.origin = None};
      {G.walls = [|1; 0; 0; 1|]; G.state = G.Free; G.father = (-1, -1); G.origin = None}|];
    [|{G.walls = [|0; 1; 1; 0|]; G.state = G.Free; G.father = (-1, -1); G.origin = None};
      {G.walls = [|1; 0; 1; 0|]; G.state = G.Free; G.father = (-1, -1); G.origin = None};
      {G.walls = [|0; 0; 1; 0|]; G.state = G.Free; G.father = (-1, -1); G.origin = None};
      {G.walls = [|1; 0; 1; 0|]; G.state = G.Free; G.father = (-1, -1); G.origin = None};
      {G.walls = [|0; 0; 1; 1|]; G.state = G.Free; G.father = (-1, -1); G.origin = None}|]|];;

(*let width_maze = 30;;
let height_maze = 30;;*)



let win = openTk ();;
Wm.title_set win "Labyrinthe"; 
Wm.geometry_set win "500x500"


let c = Canvas.create
   (* ~width: (length * width_maze + ( width_maze + 1 ) * width)
      ~height: (length * height_maze + ( height_maze + 1 ) * width)*)
    ~width:400
    ~height:400
    ~borderwidth:0
    ~relief:`Raised
    win ;;


(* rappel : x horizontal et y vertical*) (*les demi epaisseurs pour le visuel*)
let draw_case = fun x y color ->
  let d = width/2 in
  ignore (Canvas.create_polygon
          (*~xys:[(x-d,y-d);  (x-d, y + length+d) ; (x + length+d, y + length+d); (x + length+d, y-d)]*)
          ~xys:[(x,y);  (x, y + length) ; (x + length, y + length); (x + length, y)]
	  ~fill:color
	    c);;
  pack ~side:`Left [c];;

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
  
let draw_walls = fun maze i j color_case -> (* color case est la couleur de la case, si elle est marquée le passage se met de sa couleur *)
  
  let y_case = width * (i+1) + i * length in
  let x_case = width * (j+1) + j * length in
  
  if maze.(i).(j).G.walls.(0) == 1 then horizontal_wall (x_case - width) (y_case - width) else horizontal_passage (x_case) (y_case - width) color_case;
  if maze.(i).(j).G.walls.(2) == 1 then horizontal_wall (x_case - width) (y_case + length) else horizontal_passage (x_case) (y_case + length) color_case;
  if maze.(i).(j).G.walls.(1) == 1 then vertical_wall (x_case - width) (y_case - width) else vertical_passage (x_case-width) (y_case) color_case ;
  if maze.(i).(j).G.walls.(3) == 1 then vertical_wall (x_case + length) (y_case - width) else vertical_passage (x_case + length) (y_case) color_case;;
      




let change_color = fun maze i j -> (* on va définir les couleurs comme on veut *)

  let width_maze = Array.length maze.(0) in
  let height_maze = Array.length maze in
  
  let color = `White in
  let y_case = width * (i+1) + i * length in
  let x_case = width * (j+1) + j * length in

  let def_col = fun i j -> match maze.(i).(j).G.state with
    G.Free -> `Red
  | G.Marked -> `Yellow
  | G.Dead -> `Black
  | G.Exit -> `Blue
  | G.Path -> `Green in
  let color = def_col i j in
      
  draw_case (width * (j+1) + j * length) (width * (i+1) + i * length) color;
  if maze.(i).(j).G.walls.(0) == 0 then horizontal_passage (x_case) (y_case - width) color;
  if maze.(i).(j).G.walls.(2) == 0 then horizontal_passage (x_case) (y_case + length) color;
  if maze.(i).(j).G.walls.(1) == 0 then vertical_passage (x_case - width) (y_case) color;
  if maze.(i).(j).G.walls.(3) == 0 then vertical_passage (x_case + length) (y_case) color;;


let init_maze maze =
  let width_maze = Array.length maze.(0) in
  let height_maze = Array.length maze in
  for i = 0 to height_maze - 1 do
    for j = 0 to width_maze - 1 do
      draw_case (width * (j+1) + j * length) (width * (i+1) + i * length) `White;
      draw_walls maze i j `White;
    done
  done;;







let frame = Frame.create ~relief:`Sunken ~borderwidth:2 win;;

let w = Entry.create ~width:10 ~relief:`Sunken frame;;
let l = Entry.create ~width:10 ~relief:`Sunken frame;;
let b = Button.create
    ~width:15
    ~text:"Generate maze"
    ~command:(fun () ->
      Printf.printf "%s\nBye.\n" (Entry.get w) ;
             )
    frame ;;
pack ~side:`Top [coe w; coe l; coe b] ;;

let largeur = Button.create ~width:15
    ~text:"largeur"
    ~command:(fun () ->
      Printf.printf "resol largeur";
             )
    frame;;
pack ~side:`Top [largeur];;

let a_star = Button.create ~width:15
    ~text:"a_star"
    ~command:(fun () ->
      Printf.printf "resol a_star";
             )
    frame;;
pack ~side:`Top [a_star];;

let init = Button.create ~width:15
    ~text:"Initialize"
    ~command:(fun () ->
      init_maze maze
              )
    frame;;
pack ~side:`Top [init];;

let quit = Button.create ~width:15
    ~text:"Quit"
    ~command:(fun () ->
      let d = Dialog.create
	  ~parent:win
	  ~title:"Exit"
	  ~message:"Do you really want to quit?"
	  ~buttons:["Yes";"No"]
	  ~default:1
	  ()
      in
      if d=0 then (print_endline "Bye"; flush stdout; closeTk ())
      else (print_endline "OK"; flush stdout))
    frame;;
pack ~side:`Top [quit];;

pack ~side:`Right [frame];;

let _ = Printexc.print mainLoop ();;




