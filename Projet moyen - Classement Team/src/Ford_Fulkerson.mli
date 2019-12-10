open Graph
val find_voisin_non_marque : int graph-> id -> id list -> id list

type flot = int

val parcours_prof : int graph -> id -> id list -> id -> (id list * id list)

val var_flot : int graph -> id list -> int

val update_graph : int graph -> id list -> flot -> int graph

val boucle_FF: (int graph * flot * id * id) -> (int graph * flot * id * id)

val ford_fulkerson: int graph -> id -> id -> (int graph*flot)