open Graph
open Printf
open Tools

type path = string

(* Format of text files:
   % This is a comment

   %Classement d'abord

   % A team is represented by Classement Team_Name Win Game_Left_To_Play.
   3 Miami_Heats 15 10


   % Match Schedule is represented by DD/MM/YY: Team_Name1 Team_Name2 
   05/07/20: Miami_Heats Toronto_Raptors

   % 0 is source and 1 is sink cf init_graph

   %


*)

let write_file path graph =

  (* Open a write-file. *)
  let ff = open_out path in

  (* Write in this file. *)
  fprintf ff "%% This is a graph.\n\n" ;

  (* Write all nodes (with fake coordinates) *)
  n_iter_sorted graph (fun id -> fprintf ff "n %.1f 1.0\n" (float_of_int id)) ;
  fprintf ff "\n" ;

  (* Write all arcs *)
  e_iter graph (fun id1 id2 lbl -> fprintf ff "e %d %d %s\n" id1 id2 lbl) ;

  fprintf ff "\n%% End of graph\n" ;

  close_out ff ;
  ()

(* Reads a line with a node. *)
let read_node id graph line =
  try Scanf.sscanf line "n %f %f" (fun _ _ -> new_node graph id)
  with e ->
    Printf.printf "Cannot read node in line - %s:\n%s\n%!" (Printexc.to_string e) line ;
    failwith "from_file"

(* Reads a line with an arc. *)
let read_arc graph line =
  try Scanf.sscanf line "e %d %d %s" (fun id1 id2 label -> new_arc graph id1 id2 label)
  with e ->
    Printf.printf "Cannot read arc in line - %s:\n%s\n%!" (Printexc.to_string e) line ;
    failwith "from_file"

(* Reads a comment or fail. *)
let read_comment graph line =
  try Scanf.sscanf line " %%" graph
  with _ ->
    Printf.printf "Unknown line:\n%s\n%!" line ;
    failwith "from_file"

let init_graph  = (new_node (new_node empty_graph 0) 1)

 
(* Attention au +2 car 0 et 1 sont le puit et la source*)
let find_key l v =
let rec aux l v x = 
match l with
|[] -> failwith "find_key error"
| a::b -> if (a = v) then (x+2) else (aux b v (x+1))
in
aux l v 0
(*Note Some x finir avec 3 recup de la valeur des arcs a modfier*)

let read_schedule graph line n tl namek= 
match (find_arc graph 0 n) with
| None -> (
try ( 
  Scanf.sscanf line "%s %s %s" (fun _ team1 team2 ->
  ((new_arc (new_arc (new_arc (new_node graph n) 0 n 1) n (find_key tl team1) 1) n (find_key tl team2) 1),tl)))
  with e ->
    (Printf.printf "Cannot read_schedule None team in line - %s:\n%s\n%!" (Printexc.to_string e) line ;
    failwith "from_file"))
| Some x -> (try ( 
  Scanf.sscanf line "%s %s %s" (fun _ team1 team2 ->
  ((new_arc (new_arc (new_arc graph 0 n (x+1)) n (find_key tl team1) (x+1)) n (find_key tl team2) (x+1)),tl)))
  with e ->
    Printf.printf "Cannot read_schedule Some x team in line - %s:\n%s\n%!" (Printexc.to_string e) line ;
    failwith "from_file")

(* place + 2 car il y a la source et le puit *)
let read_classement graph line n wk rk tl namek= 
  try Scanf.sscanf line "%s %s %d %d" (fun _ name wi ri  -> if ((wk + rk - wi) < 0) then ((new_node graph n), (List.append tl [name] ))  else ((new_arc (new_node graph n) n 1 (wk + rk - wi)), (List.append tl [name] )))
  with e ->
    Printf.printf "Cannot read_classement team in line - %s:\n%s\n%!" (Printexc.to_string e) line ;
    failwith "from_file"


let read_data graph line n wk rk tl namek= 
  try (if (not((line.[1] = '/') || (line.[2]='/'))) 
  then ((read_classement graph line n wk rk tl namek)) 
  else (read_schedule graph line n tl namek))
    
  with e -> 
    Printf.printf "Cannot read data" ;
    failwith "from_file"


let new_from_file path wk rk namek= 
  let infile = open_in path in

  (* Read all lines until end of file. 
   * n is the current node counter. *)
  let rec loop n graph tl=
    try
      let line = input_line infile in

      (* Remove leading and trailing spaces. *)
      let line = String.trim line in

      let (n2, (graph2,tl)) =
        (* Ignore empty lines *)
        if line = "" then (n, (graph,tl))
        else if (line.[0] = '%' ) then (n,(read_comment graph line,tl))


        else (
          try (if ((Scanf.sscanf line "%s %s %s" (fun a b c  -> (if ((a=namek) || (b=namek) || (c=namek)) then (true) else (false))  ))) then ((n, (graph,tl))) else (((n+1),read_data graph line n wk rk tl namek)))
          with e ->
          (Printf.printf "Cannot scan file before read data in line - %s:\n%s\n%!" (Printexc.to_string e) line ;
          failwith "from_file"))
          
        (*match line.[0] with
          | 'n' -> (n+1, read_node n graph line)
          | 'e' -> (n, read_arc graph line)

          (* It should be a comment, otherwise we complain. *)
          | _ -> (n, read_comment graph line)*)
      in      
      loop n2 graph2 tl

    with End_of_file -> graph (* Done *)
  in

  let final_graph = loop 2 init_graph [] in

  close_in infile ;
  final_graph

let from_file path =

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

        (* The first character of a line determines its content : n or e. *)
        else match line.[0] with
          | 'n' -> (n+1, read_node n graph line)
          | 'e' -> (n, read_arc graph line)

          (* It should be a comment, otherwise we complain. *)
          | _ -> (n, read_comment graph line)
      in      
      loop n2 graph2

    with End_of_file -> graph (* Done *)
  in

  let final_graph = loop 0 empty_graph in

  close_in infile ;
  final_graph

let export graph = 
  let ff = open_out "visualGraph" in
  fprintf ff "digraph finite_state_machine {
	rankdir=LR;
	size=\"8,5\"
  node [shape = doublecircle]; 
  ";
  n_iter graph (fun x ->  fprintf ff "LR_%i " x );
  fprintf ff ";
	node [shape = circle];";
  e_iter graph (fun x y z -> (fprintf ff "LR_%i -> LR_%i [ label = \"%i\" ];\n" x y z));
  fprintf ff "}"

;;