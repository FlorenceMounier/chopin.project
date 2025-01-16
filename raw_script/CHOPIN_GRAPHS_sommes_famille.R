####################################
##         PROJET CHOPIN
## 
##          GRAPHIQUES
##     SOMMES PAR FAMILLE
##      
####################################


#-------------------------------------------------
# Chargement de la fonction graphique

source("Script/CHOPIN_FUN_graph_group_zone_saison.R")


#-------------------------------------------------
# SEDIMENTS

# Representation graphique des sommes par famille par taxon, zone et saison

GRAPH_group_zone_saison(data = sed_contam, label_data = "SEDIMENTS", variable = "sommePCB_ng_gdw", label_y=expression(paste(sum(),"PCB")), wd="Output/SEDIMENTS/")
GRAPH_group_zone_saison(data = sed_contam,label_data = "SEDIMENTS",  variable = "sommePFCA_ng_gdw", label_y=expression(paste(sum(),"PFCA")), wd="Output/SEDIMENTS/")
GRAPH_group_zone_saison(data = sed_contam,label_data = "SEDIMENTS",  variable = "sommePFSA_ng_gdw", label_y=expression(paste(sum(),"PFSA")), wd="Output/SEDIMENTS/")
GRAPH_group_zone_saison(data = sed_contam,label_data = "SEDIMENTS",  variable = "sommeFOSA_ng_gdw", label_y=expression(paste(sum(),"FOSA et FOSAA")), wd="Output/SEDIMENTS/")
GRAPH_group_zone_saison(data = sed_contam, label_data = "SEDIMENTS", variable = "sommeHBCDD_ng_gdw", label_y=expression(paste(sum(),"HBCDD")), wd="Output/SEDIMENTS/")


#-------------------------------------------------
# BENTHOS


# Representation graphique des sommes par famille par taxon, zone et saison

GRAPH_group_zone_saison(data = benthos_contam, label_data = "BENTHOS", variable = "sommePCB_ng_gdw", label_y=expression(paste(sum(),"PCB")), wd="Output/BENTHOS/")
GRAPH_group_zone_saison(data = benthos_contam,label_data = "BENTHOS",  variable = "sommePFCA_ng_gdw", label_y=expression(paste(sum(),"PFCA")), wd="Output/BENTHOS/")
GRAPH_group_zone_saison(data = benthos_contam,label_data = "BENTHOS",  variable = "sommePFSA_ng_gdw", label_y=expression(paste(sum(),"PFSA")), wd="Output/BENTHOS/")
GRAPH_group_zone_saison(data = benthos_contam,label_data = "BENTHOS",  variable = "sommeFOSA_ng_gdw", label_y=expression(paste(sum(),"FOSA et FOSAA")), wd="Output/BENTHOS/")
GRAPH_group_zone_saison(data = benthos_contam, label_data = "BENTHOS", variable = "sommeHBCDD_ng_gdw", label_y=expression(paste(sum(),"HBCDD")), wd="Output/BENTHOS/")




#-------------------------------------------------
# SOLES


# Representation graphique des sommes par famille par classe d'âge, zone et saison

GRAPH_group_zone_saison(data = soles_contam, label_data = "soles", variable = "sommePCB_ng_gdw", label_y=expression(paste(sum(),"PCB")), wd = "Output/SOLES/")
GRAPH_group_zone_saison(data = soles_contam,label_data = "soles",  variable = "sommePFCA_ng_gdw", label_y=expression(paste(sum(),"PFCA")), wd = "Output/SOLES/")
GRAPH_group_zone_saison(data = soles_contam,label_data = "soles",  variable = "sommePFSA_ng_gdw", label_y=expression(paste(sum(),"PFSA")), wd = "Output/SOLES/")
GRAPH_group_zone_saison(data = soles_contam,label_data = "soles",  variable = "sommeFOSA_ng_gdw", label_y=expression(paste(sum(),"FOSA et FOSAA")), wd = "Output/SOLES/")
GRAPH_group_zone_saison(data = soles_contam,label_data = "soles",  variable = "sommeautres_ng_gdw", label_y=expression(paste(sum(),"autres PFAS")), wd = "Output/SOLES/")
GRAPH_group_zone_saison(data = soles_contam, label_data = "soles", variable = "sommeHBCDD_ng_gdw", label_y=expression(paste(sum(),"HBCDD")), wd = "Output/SOLES/")


