####################################
##         PROJET CHOPIN
## 
##          GRAPHIQUES
##  NIVEAUX NORMALISES PAR LA SOMME
##      
####################################



#-------------------------------------------------
# Chargement de la fonction graphique

source("Script/CHOPIN_FUN_graph_normalised_group_zone_saison.R")



#-------------------------------------------------
# BENTHOS


# PCB
sapply(X=PCB, FUN = GRAPH_normalised_group_zone_saison,
       label_contam = PCB_lab,
       data = benthos_contam,
       label_data = "BENTHOS",
       wd = "Output/BENTHOS/")


# PFAS
sapply(X=PFAS, FUN = GRAPH_normalised_group_zone_saison,
       label_contam = PFAS_lab,
       data = benthos_contam,
       label_data = "BENTHOS", wd = "Output/BENTHOS/")


# HBCDD
sapply(X=HBCDD, FUN = GRAPH_normalised_group_zone_saison,
       label_contam = HBCDD,
       data = benthos_contam,
       label_data = "BENTHOS",
       wd = "Output/BENTHOS/")




#-------------------------------------------------
# SOLES


# PCB
sapply(X=PCB, 
       FUN = GRAPH_normalised_group_zone_saison,
       label_contam = PCB_lab,
       data = soles_contam,
       label_data = "SOLES",
       wd = "Output/SOLES/")


# PFAS
sapply(X=PFAS, FUN = GRAPH_normalised_group_zone_saison,
       label_contam = PFAS_lab,
       data = soles_contam,
       label_data = "SOLES",
       wd = "Output/SOLES/")


# HBCDD
sapply(X=HBCDD, FUN = GRAPH_normalised_group_zone_saison,
       label_contam = HBCDD,
       data = soles_contam,
       label_data = "SOLES",
       wd = "Output/SOLES/")


