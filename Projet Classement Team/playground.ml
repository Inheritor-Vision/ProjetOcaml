let f = input_line (open_in "/home/doganis/Bureau/ProjetOcaml/OCAML_Projet_v1/test");;

Scanf.sscanf f "%s %s %s %s " (fun a b c d -> Printf.printf "%s %s %!" a b;);;
