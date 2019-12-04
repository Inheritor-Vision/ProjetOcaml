open Graph 
open Tools

let rec find_voisin_aux l = match l with
  |(id,x)::rest -> id::find_voisin_aux rest 
  |[] -> [];;

let find_voisin graph node = 
  find_voisin_aux (out_arcs graph node);;



let rec find_voisin_non_marque_aux lmarque lvoisin = 
  match (lvoisin) with 
  | idvoisin::rest -> 
    if (List.mem idvoisin lmarque) then find_voisin_non_marque_aux lmarque rest else idvoisin::(find_voisin_non_marque_aux lmarque rest)
  | [] -> []
;;
let find_voisin_non_marque graph node lmarque = find_voisin_non_marque_aux lmarque (find_voisin graph node)
;;

let add_list_marque lmarque lelement = List.append lelement lmarque ;;



type flot = int ;;



let rec parcours_prof graph actual lmarque final = 
  if(actual = final && ((List.mem final lmarque) = false)) then (add_list_marque lmarque [final],  [final])
  else 
    let voisin = find_voisin_non_marque graph actual lmarque in
    if(voisin = []) 
    then (add_list_marque lmarque [actual],  [])
    else 
      let child_res = (List.fold_left (fun (lm,chem) x -> match (parcours_prof graph x lm final) with |(lma,chemr) -> (lma, add_list_marque chemr chem)) (add_list_marque lmarque [actual], []) voisin ) in 
      match (child_res) with
      | (x,[]) -> ((actual::x),[])
      | (x,y) -> ((actual::x), (actual::y))
;;

let rec var_flot_aux graph chemin flot = 
  match chemin with 
  |a::b::rest ->( 
      match (find_arc graph a b) with 
      | None -> raise (Graph_error ("Error arc inexistant flot"))
      | Some x -> 
        if (flot = (-1)) 
        then (var_flot_aux graph (b::rest) x) 
        else (
          if(flot > x ) then (var_flot_aux graph (b::rest) x) else (var_flot_aux graph (b::rest) flot )))
  |b::[] -> flot
  |[] -> flot;;

let var_flot graph chemin = var_flot_aux graph chemin (-1);;

let rec contenue_dans_chemin node1 node2 lchemin = 
  match lchemin  with
  | a::b::rest -> if ((a=node1) && (b=node2)) then true else (false || (contenue_dans_chemin node1 node2 (b::rest)))
  | a::[] -> false
  | [] -> false;;

let update_graph_aux graph node1 node2 label lchemin flot= 
  if (contenue_dans_chemin  node1 node2 lchemin) 
  then 
    if((label-flot)=0) then (add_arc graph node2 node1 label) 
    else (add_arc  (add_arc graph node2 node1 (flot)) node1 node2 (label-flot))

  else (add_arc graph node1 node2 label)

let update_graph graph lchemin flot = 
  let res_graph = clone_nodes graph in
  let nv_add_arc a b c d = (update_graph_aux a b c d lchemin flot) in 
  e_fold graph nv_add_arc res_graph;;

let rec boucle_FF (graph,flot,depart,arrive) = 
  match (parcours_prof graph depart [] arrive) with
  | (_,y) ->
    if (y=[]) 
    then (graph,flot,depart,arrive)
    else (
      let vari_flot = (var_flot graph y) in 
      let nv_graph = (update_graph graph y vari_flot) in

      boucle_FF (nv_graph,(flot + vari_flot),depart,arrive)
    );;


(*let rec parcours_prof graph actual final file lmarque = 
  if (file = []) 
  then raise Graph_error("Erreur File vide")
  else
  let voisin = find_voisin_non_marque graph actual lmarque in
    if(voisin = []) 
    then (false,[])
    else
      if(List.mem final voisin) 
      then (true,[actual;final]) 
      else 
        match (take_LIFO_marque (add_list_marque file voisin)) with
        | (None,_) -> raise Graph_error("Erreur File vide")
        | (Some x, y) -> 
          match (parcours_prof graph x final y (add_list_marque l_marque voisin)) with 
          | (true, chemin) -> (true, x::chemin)
          | (false, chemin) -> ();;*)

(* init flot a 0, actual a s , final a p, l marque a [], graph a graph , lchemin a [] *)
(*let rec parcours_largeur graph lmarque actual final flot lchemin = 
  match(find_voisin_non_marque graph actual lmarque) with
  | a::rest -> 

    let boucle_FF (graph, flot) = 
  ;;*)



