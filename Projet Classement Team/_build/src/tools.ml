open Graph
let clones_nodes_aux graF id = new_node graF id;;

let clone_nodes gr = 
  n_fold gr clones_nodes_aux empty_graph;;



let gmap_aux graF pid nid prec f= 
  if (node_exists graF pid)=false then 
    if (node_exists graF nid) = false then 
      (new_arc (new_node (new_node graF pid) nid) pid nid (f prec)) 
    else (new_arc (new_node graF pid) pid nid (f prec)) 
  else if (node_exists graF nid) = false then 
    (new_arc (new_node graF nid) pid nid (f prec))
  else
    (new_arc graF pid nid (f prec));;

let gmap gr f =
  let temp a b c d = gmap_aux a b c d f 
  in 
  e_fold gr temp (n_fold gr (fun graph id -> new_node graph id ) empty_graph);;

let add_arc gr pid nid n = match (find_arc gr pid nid) with
  | None -> new_arc gr pid nid n
  | Some x -> new_arc gr pid nid (x+n)
;;
