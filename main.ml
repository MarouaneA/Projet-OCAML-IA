

let main () =
  let maze = Generation.create_maze 30 30 in
  Interface.init_maze maze;;

main()
