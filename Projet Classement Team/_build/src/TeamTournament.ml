open Graph 
open Ford_Fulkerson

let rec flot_max_coupe_s_rest_aux arclist = 
match(arclist) with
| (_,a)::rest -> a+(flot_max_coupe_s_rest_aux rest)
| [] -> 0;;

let flot_max_coupe_s_rest graph = flot_max_coupe_s_rest_aux (out_arcs graph 0);;

let isTeamEliminated graph flot = 
if ((flot_max_coupe_s_rest graph) = flot )
then false else true;;