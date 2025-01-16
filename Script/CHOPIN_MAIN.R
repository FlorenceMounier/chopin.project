####################################
##         PROJET CHOPIN
## 
##         MAIN SCRIPT
##      
####################################


# clear workspace
rm(list=ls())

#-------------------------------------------------------------
## MISE EN FORME & TRANSFORMATIONS DES DONNEES BRUTES


# chargement des données à partir de la base excel
# transformations d'unités (concentration pf, ps, Corg, normalisations par la somme par famille)


# library("viridis")
# # load packages
# library(tidyverse) # contient ggplot2
# library(gridExtra)
# library(ggpubr)
# library("RColorBrewer")
# library(openxlsx)
# library(data.table)
# library(cowplot)
# library(gg.gap)
# library(devtools)
# library(FactoMineR)
# require(Factoshiny)
# library(factoextra)

library(openxlsx) # Chargement des packages
source('Script/CHOPIN_DATA_FORMATTING_MAIN.R')



#-------------------------------------------------------------
## ANALYSES DESCRIPTIVES DES DONNEES

# fréquence de détection des contaminants

source('Script/CHOPIN_ANALYSES_MAIN.R')



#-------------------------------------------------------------
## REPRESENTATIONS GRAPHIQUES DES DONNEES


library(tidyverse) # contient ggplot2
library(gg.gap)
library(cowplot)
library(viridis)
library(gridExtra)
library(ggmap)
library(devtools)

source('Script/CHOPIN_GRAPHS_MAIN.R')



#-------------------------------------------------------------
## TESTS D'HYPOTHESES SUR LES DONNEES

source('Script/CHOPIN_TESTS_MAIN.R')

