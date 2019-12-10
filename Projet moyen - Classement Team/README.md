Compilation:
	Build gtest.ml ( -> gtest.native)

Execution:
	./gtest.native [source_file] [Team_Name] [Wins_of_the_team] [Matchs_left_to_play] [output_file]
	
[source_file]: Chemin relatif du fichier source
[Team_Name]: Nom de l'equipe (dans le fichier source)
[Wins_of_the_team]: Nombre de victoire de l'equipe
[Matchs_left_to_play]: Nombre de matchs restant à jouer pour cette équipe
[output_file]: Chemin et nom des fichiers produits. Il y en a 4: 
					[output_file]_initial	(Graph avant résolution)
					[output_file]_result	(Graph apres résolution)
					[output_file]_dot_initial	(Graph avant résolution format dot)
					[output_file]_dot_result	(Graph apres résolution format dot)
