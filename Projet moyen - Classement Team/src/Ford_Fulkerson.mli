open Graph
(* Find all the nodes directly linked to this node, except if they are marked. That means their are on the given list
   Args: Graph -> Id of the node -> List of marked nodes 
   Return: List of the neighbouring nodes *)
val find_voisin_non_marque : int graph-> id -> id list -> id list

(* The flow is represented by an int *)
type flot = int

(* Find an available path through the graph from the sink to the sink.
   Args: Graph -> Source -> list of marked nodes (initially empty) -> Sink
   Return: (list of marked nodes * list of nodes representing the path*)
val parcours_prof : int graph -> id -> id list -> id -> (id list * id list)

(* Find the minimum value of the arc on the given path
   Args: Graph -> path 
   Result: Minimum arc of the path*)
val var_flot : int graph -> id list -> int

(* Update the graph by reducing the value of the arcs of the path and by creating reverse arcs of the amount of flow.
   Args: Graph -> path -> min arc of the path 
   Return: Updated graph*)
val update_graph : int graph -> id list -> flot -> int graph

(* Final Ford Fulkerson algorithm  with manual initialization 
   Args: (Graph * flow (zero) * source * sink)
   Return: (Graph after FF algorithm * max flow * source * sink) *)
val boucle_FF: (int graph * flot * id * id) -> (int graph * flot * id * id)

(* Final Ford Fulkerson algorithm with initialized value
   Agrs: Graph -> source -> sink 
   Result: (Result graph * max flow) *)
val ford_fulkerson: int graph -> id -> id -> (int graph*flot)