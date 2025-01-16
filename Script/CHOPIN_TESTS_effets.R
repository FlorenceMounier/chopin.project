####################################
##         PROJET CHOPIN
## 
##            TESTS
##           EFFETS 
##      
####################################



###############################################################
## FONCTION DES TESTS

test_effect = function(dataset, variable, factor){
  print(paste(variable,factor, sep=" / "))
  print(kruskal.test(dataset[,variable], g = dataset[,factor]))
  print(pairwise.wilcox.test(dataset[,variable], g = dataset[,factor]))
  print(wilcox.test(dataset[,variable], g = dataset[,factor]))
  
}



###############################################################
##     BENTHOS


#------------------------------------
# EFFET GROUPE TAXONOMIQUE


# sommes par famille
test_effect(dataset = benthos_contam, variable = "sommePCB_ng_gdw", factor = "grp")
test_effect(dataset = benthos_contam, variable = "sommePFAS_ng_gdw", factor = "grp")
test_effect(dataset = benthos_contam, variable = "sommeHBCDD_ng_gdw", factor = "grp")

# contaminant majoritaire dans la famille
test_effect(dataset = benthos_contam, variable = "CB153_ng_g.1ps", factor = "grp")
test_effect(dataset = benthos_contam, variable = "L.PFOS", factor = "grp")
test_effect(dataset = benthos_contam, variable = "g.HBCDD_ng_g.1ps", factor = "grp")



#------------------------------------
# EFFET SAISON

bivalves_contam = split(x = benthos_contam, f = benthos_contam$grp)$Bivalves
crustaces_contam = split(x = benthos_contam, f = benthos_contam$grp)$Crustaces
polychetes_contam = split(x = benthos_contam, f = benthos_contam$grp)$Polychetes


# somme PCB

# toutes espèces confondues
test_effect(dataset = benthos_contam, variable = "sommePCB_ng_gdw", factor = "season")
# bivalves
test_effect(dataset = bivalves_contam, variable = "sommePCB_ng_gdw", factor = "season")
# crustaces
test_effect(dataset = crustaces_contam, variable = "sommePCB_ng_gdw", factor = "season")
# polychetes
test_effect(dataset = polychetes_contam, variable = "sommePCB_ng_gdw", factor = "season")


# sommePFAS

# toutes espèces confondues
test_effect(dataset = benthos_contam, variable = "sommePFAS_ng_gdw", factor = "season")
# bivalves
test_effect(dataset = bivalves_contam, variable = "sommePFAS_ng_gdw", factor = "season")
# crustaces
test_effect(dataset = crustaces_contam, variable = "sommePFAS_ng_gdw", factor = "season")
# polychetes
test_effect(dataset = polychetes_contam, variable = "sommePFAS_ng_gdw", factor = "season")


# sommeHBCDD

# toutes espèces confondues
test_effect(dataset = benthos_contam, variable = "sommeHBCDD_ng_gdw", factor = "season")
# bivalves
test_effect(dataset = bivalves_contam, variable = "sommeHBCDD_ng_gdw", factor = "season")
# crustaces
test_effect(dataset = crustaces_contam, variable = "sommeHBCDD_ng_gdw", factor = "season")
# polychetes
test_effect(dataset = polychetes_contam, variable = "sommeHBCDD_ng_gdw", factor = "season")



###############################################################
##     SOLES


#------------------------------------
# EFFET classe d'age


# sommes par famille
test_effect(dataset = soles_contam, variable = "sommePCB_ng_gdw", factor = "grp")
test_effect(dataset = soles_contam, variable = "sommeFAS_ng_gdw", factor = "grp")
test_effect(dataset = soles_contam, variable = "sommeHBCDD_ng_gdw", factor = "grp")

# contaminant majoritaire dans la famille
test_effect(dataset = soles_contam, variable = "CB153_ng_g.1ps", factor = "grp")
test_effect(dataset = soles_contam, variable = "L.PFOS", factor = "grp")
test_effect(dataset = soles_contam, variable = "g.HBCDD_ng_g.1ps", factor = "grp")

# isotopes

