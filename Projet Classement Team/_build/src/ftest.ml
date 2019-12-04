open Gfile
open Tools
open Ford_Fulkerson

let () =

  (* Check the number of command-line arguments *)
  if Array.length Sys.argv <> 5 then
    begin
      Printf.printf "\nUsage: %s infile source sink outfile\n\n%!" Sys.argv.(0) ;
      exit 0
    end ;


  (* Arguments are : infile(1) source-id(2) sink-id(3) outfile(4) *)

  let infile = Sys.argv.(1)
  and outfile = Sys.argv.(4)

  (* These command-line arguments are not used for the moment. *)
  and _source = int_of_string Sys.argv.(2)
  and _sink = int_of_string Sys.argv.(3)
  in

  (* Open file *)
  let graph = from_file infile in

  (* Rewrite the graph that has been read. *)
  let () = write_file outfile ( graph) and intgraph = (gmap graph (fun x -> int_of_string(x)))in
  let res = ford_fulkerson intgraph _source _sink in
  (*(gmap (add_arc (gmap graph (fun x -> int_of_string(x))) 2 1 100) (fun x -> string_of_int(x)))*)
  (); Printf.printf "La variation de flot maximum est %d\n%!" res;;

(* MAIN
   match (boucle_FF (intgraph,0,0,7))with 
   | (a,b,c,d) -> Printf.printf (" flot : %d \n %!") b; export a
*)

(*AFFICHAGE CHEMIN FLOT UPDATED GRAPH
  match (parcours_prof intgraph 0 [] 5) with
  | (a,b) -> List.iter (fun x -> Printf.printf "%d %!" x) b; Printf.printf "\nflot: %d \n %!" (var_flot intgraph b); export (update_graph intgraph b (var_flot intgraph b));;*)
(* AFFICHE CHEMIN ET FLOT
   match (parcours_prof intgraph 0 [] 5) with
   | (a,b) -> List.iter (fun x -> Printf.printf "%d %!" x) b; Printf.printf "\nflot: %d \n %!" (var_flot intgraph b)*)
(*AFFICHAGE D UN CHEMIN
  match (parcours_prof intgraph 0 [] 5) with 
  | (a,b) -> List.iter (fun x -> Printf.printf "%d \n%!" x) b*)



