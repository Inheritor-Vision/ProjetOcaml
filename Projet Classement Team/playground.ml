(* Reads a comment or fail. *)
let read_comment graph line =
  try Scanf.sscanf line " %%" graph
  with _ ->
    Printf.printf "Unknown line:\n%s\n%!" line ;
    failwith "from_file";;

let init_graph  = (new_node (new_node empty_graph 0) 1);;

let read_schedule graph line n trash = graph;;

(* place + 2 car il y a la source et le puit *)
let read_classement graph line n wk rk= 
  try Scanf.sscanf line "%d %s %d %d" (fun place name wi ri  -> (new_arc (new_node graph (place+2)) n 1 (wk + rk - wi)))
  with e ->
    Printf.printf "Cannot read team in line - %s:\n%s\n%!" (Printexc.to_string e) line ;
    failwith "from_file";;


let read_data graph line n wk rk= 

  try (read_schedule graph line n (int_of_char line.[0]))
  with _ -> 
    read_classement graph line n wk rk;;


let new_from_file path wk rk= 
  let infile = open_in path in

  (* Read all lines until end of file. 
   * n is the current node counter. *)
  let rec loop n graph =
    try
      let line = input_line infile in

      (* Remove leading and trailing spaces. *)
      let line = String.trim line in

      let (n2, graph2) =
        (* Ignore empty lines *)
        if line = "" then (n, graph)
        else if (line.[0] = '%' ) then (n,read_comment graph line)


        else (n,read_data graph line n wk rk)
        (*match line.[0] with
          | 'n' -> (n+1, read_node n graph line)
          | 'e' -> (n, read_arc graph line)

          (* It should be a comment, otherwise we complain. *)
          | _ -> (n, read_comment graph line)*)
      in      
        loop n2 graph2

    with End_of_file -> graph (* Done *)
  in

  let final_graph = loop 2 init_graph in

    close_in infile ;
    final_graph;;
