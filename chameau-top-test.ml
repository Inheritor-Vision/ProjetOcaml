let add_marque list  element = element::list;;

let  rec add_list_marque lmarque lelement = match (lelement) with 
  | x::rest -> add_list_marque (add_marque lmarque x) rest 
  | [] -> lmarque ;;

let add_list_marque lmarque lelement = List.append lelement lmarque ;;

add_list_marque [3;4] [1;2];;





