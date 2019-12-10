open Gfile
open Tools
open Ford_Fulkerson
open TeamTournament



let () =

  (* Check the number of command-line arguments *)
  if Array.length Sys.argv <> 6 then
    begin
      Printf.printf "\nUsage: %s [infile] [team_name] [win_of_the_team] [game_left_to_play_of_the_team] [outfile]\n\n%!" Sys.argv.(0) ;
      exit 0
    end ;


  (* Arguments are : infile(1) source-id(2) sink-id(3) outfile(4) *)

  let infile = Sys.argv.(1)
  and outfile = Sys.argv.(5)

  (* These command-line arguments are not used for the moment. *)
  and _namek =  Sys.argv.(2)
  and _wk = int_of_string Sys.argv.(3)
  and _rk = int_of_string Sys.argv.(4)
  in

  (* Open file *)
  let graph = new_from_file infile _wk _rk _namek in

  (* Rewrite the graph that has been read. *)
  let () = write_file (outfile^"_initial") ( gmap graph (fun x -> string_of_int(x))) in
  let (resgraph,flot) = ford_fulkerson graph 0 1 in
  
  ();if not (isTeamEliminated graph flot) then (Printf.printf "\nL'équipe %s peut encore gagner le championnat :D \n%!" _namek) else (Printf.printf "\nL'équipe %s ne peut plus gagner le championnat :( \n%!" _namek);
  Printf.printf "Le flot max de la coupe s/reste: %d \n%!"  (flot_max_coupe_s_rest graph) ;
  Printf.printf "Le flot max trouvé est de %d \n\n%!" flot;
  write_file (outfile^"_result") ( gmap resgraph (fun x -> string_of_int(x)));
  export graph (outfile^"_dot_initial");
  export resgraph (outfile^"_dot_result");;
