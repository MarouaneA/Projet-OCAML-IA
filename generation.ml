Random.self_init ();

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

type 'a pile = {
    stack: 'a list;
    size: int
  };;

exception Empty;;

let empty_stack = {stack = []; size = 0};;

let is_empty = fun s ->
  s.size = 0;;

let add = fun x s ->
  {stack = x :: s.stack; size = s.size + 1};;

let take = fun s ->
  match s.stack with
    [] -> raise Empty
  | x :: xs -> (x, {stack = xs; size = s.size - 1});;


let size = fun s ->
  s.size;;

let iter = fun f s ->
  List.iter f s.stack;;

let coord_next = fun i j k ->
  match k with 
  | 0 -> (i-1,j)
  | 1 -> (i,j-1)
  | 2 -> (i+1,j)
  | _ -> (i,j+1);;


let out_of_bounds = fun couple n m  ->
  let x = fst couple and y = snd couple in 
  if x >= 0 && x<n && y>=0 && y<m then 0 else 1;;


let next_case = fun maze i j stack n m -> (*choix de la case suivante*)
  let start = Random.int 4 in (*On choisit par quel coté on va commencer*)
  let rec find_case = fun direction remain -> (*direction = le coté étudié, remain = le nombre de cotés restants à étudier*)
    if remain = 0 && size stack>0 then let res = take stack in res (*si on a étudié tous les cotés et qu'aucun n'est possible, on revient en arrière si la pile des cases précédemment explorées est non vide*)
    else if remain = 0 && size stack = 0 then ((-1,-1), stack)
    else if maze.(i).(j).walls.(direction) = 1 then  (*sinon, si un mur existe, on a trouvé la prochaine potentielle*)
      begin 
        let next = coord_next i j direction in (*on recupere les coord de la case suivante*)
        if (out_of_bounds next n m) = 0 &&maze.(fst next).(snd next).state == Marked then 
          begin
            let stack = add (i,j) stack in 
            maze.(i).(j).walls.(direction)<-0;
            maze.(fst next).(snd next).walls.( (2+direction) mod 4 )<-0;
            (next , stack) (*si elle est non visitée, on y va*)
          end
        else find_case ((direction+1) mod 4) (remain-1) (*sinon on passe à la direction suivante*)
      end
    else find_case ((direction+1) mod 4) (remain-1); (*sinon on passe à la direction suivante*)
      in 
  find_case start 4;;

let create_maze = fun n m ->
  let init_maze = Array.init n (fun _ -> Array.init   m (fun _ -> { walls =  Array.make 4 1 ; state = Marked ; father = (-1,-1) ;  origin = None })) in
  let explo=empty_stack in
  let starti = Random.int (n) and startj = Random.int (m) in

  let rec explore_maze = fun maze i j  stackexplo ->
    let suivant = next_case maze i j stackexplo n m in
    maze.(i).(j).state <- Free;(* case n'est plus à visiter*)
    match suivant with 
    | ((-1,-1),stack) ->  maze
    | (next , stack) -> explore_maze maze (fst next) (snd next) stack
  in 
  explore_maze init_maze starti startj explo;;

let powbin = fun x -> int_of_float (2. ** (float_of_int x));;

let array_to_int = fun arr -> 
  let l = Array.length arr and a = ref 0 in 
  for i = 0 to l-1 do 
    a := !a + powbin(l-1-i)*arr.(i)
    done;
!a;;
    
let int_to_array = fun k arr ->
  let bits=3 in

  let rec decomp = fun tot i -> 
    
    let puiss = powbin i in  
    
    if i >=0 && puiss  <= tot then begin
      arr.(bits-i) <- 1; 
      decomp (tot-puiss) (i-1)  end
    else if i>=0 then decomp tot (i-1)
  in 
 decomp k bits;;



let save = fun maze name ->
  let n = Array.length maze and m = Array.length maze.(0) and oc = open_out name in 
  Printf.fprintf oc "%d %d " n m ;
  for i = 0 to n-1 do 
    for j = 0 to m-1 do 
      Printf.fprintf oc "%d " (array_to_int maze.(i).(j).walls);
  done;
done;
close_out oc;;

let parse_integers s =
  let stream = (Scanf.Scanning.from_string s) in
  let rec do_parse acc =
    try
      do_parse (Scanf.bscanf stream " %d " (fun x -> x :: acc))
    with
      Scanf.Scan_failure _ -> acc
    | End_of_file -> acc
  in 
  let (n::m::q) = List.rev (do_parse []) in 
  let maze = Array.init n (fun _ -> Array.init   m (fun _ -> {walls =  Array.make 4 0 ; state = Free ; father = (-1,-1) ;  origin = None })) in
  let rec fill = fun list step -> 
    let x = step/m and y = step mod m in
    match list with 
    | [] -> maze
    | t::queue ->  int_to_array t maze.(x).(y).walls ;  fill queue (step+1) 
  in
  fill q 0;;


let read_maze addr = 
  let ic = open_in addr  in
  try 
    let line = input_line ic in 
    flush stdout; (* write on the underlying device now *)
    close_in ic;
    parse_integers line; 
  
  with e ->               
    close_in_noerr ic;(* emergency closing *)
    raise e;;


let break_wall = fun maze i j n m dir -> 
  let murs = maze.(i).(j).walls in 
  let rec tour = fun itter ->
    match itter with  
    | 4 -> false 
    | _ -> let cn = (coord_next i j ((dir + itter) mod 4)) in
      if murs.((dir + itter) mod 4) = 1 && (out_of_bounds cn n m) = 0 then
        begin murs.((dir + itter) mod 4) <- 0; maze.(fst cn).(snd cn).walls.((2+dir + itter) mod 4) <- 0; true end 
      else tour (itter+1)
in tour 0 ;;
    

(*Enlever mur aleatoire*)
let break_rand_wall = fun k maze -> 
  let n = Array.length maze and m = Array.length maze.(0) in 
  let rec seek_destroy = fun rest -> 
    match rest with 
    |0 -> () 
    |_ -> let i = Random.int n and j = Random.int m and dir = Random.int 4 in 
          if break_wall maze i j n m dir then () else seek_destroy (rest-1)
in seek_destroy k;;


let rec openwalls = fun nb k maze ->
  match nb with
  |0 -> ()
  | _ -> break_rand_wall k maze ; openwalls (nb-1) k maze;;
  
(*let  a = int_of_string  Sys.argv.(1)

save (create_maze a a) "centcent.txt";;*)

(*let () =

    if Array.length Sys.argv = 4 then 
      let  n = int_of_string  Sys.argv.(1) and m = int_of_string Sys.argv.(2) in
      save (create_maze n m) Sys.argv.(3)
    else Printf.printf "Il faut trois arguments n m et name.txt\n";;*)
