Compilation:

	Build ftest.ml ( -> ftest.native)
	
Execution:

	./ftest.native [source_file] [source] [sink] [output_file]
	
[source_file]: Chemin relatif du fichier source

[source]: Numéro du noeud source

[sink]: Numéro du noeud puit

[output_file]: Chemin et nom des fichiers produits. Il y en a 4: 

					[output_file]_initial	(Graph avant résolution)
					
					[output_file]_result	(Graph apres résolution)
					
					[output_file]_dot_initial	(Graph avant résolution format dot)
					
					[output_file]_dot_result	(Graph apres résolution format dot)
					
