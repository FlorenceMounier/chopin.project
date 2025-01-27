####################################
##         PROJET CHOPIN
##
##    MISE EN FORME DES DONNEES
##        CONTENUS STOMACAUX
####################################


# Loadings

## Load packages

library(readxl)

## Load data

stomacG0G1 <- as.data.frame(read_excel("data-raw/BDD_contenus_stomacaux_CHOPIN_avril_2020.xlsx"))
stomacG2 <- read_excel("data-raw/BDD_contenus_stomacaux_CHOPIN_soles_G2_octeville.xlsx")

stomac_soles = rbind(stomacG0G1[which(stomacG0G1$`espèce/stade`=="Sole G0"|stomacG0G1$`espèce/stade`=="Sole G1"),],
                     stomacG2)


# remove parasites & detritus & empty
stomac_soles = stomac_soles[-which(stomac_soles$Statut=="déchet"|
                                     stomac_soles$Statut=="Déchet"|
                                     stomac_soles$Statut=="Parasite"|
                                     stomac_soles$Statut=="RAS"|
                                     stomac_soles$ScientificName_accepted=="Divers"|
                                     stomac_soles$ScientificName_accepted=="Divers_débris"|
                                     stomac_soles$ScientificName_accepted=="Non identifié"|
                                     stomac_soles$ScientificName_accepted=="Ponte_nd"|
                                     stomac_soles$ScientificName_accepted=="Animalia 4"|
                                     stomac_soles$ScientificName_accepted=="Animalia 5"|
                                     stomac_soles$ScientificName_accepted=="Pleuronectes platessa_écailles"|
                                     stomac_soles$ScientificName_accepted=="Actinopterygii_écaille"),]

# Output data

usethis::use_data(stomacG0G1, overwrite = TRUE)
usethis::use_data(stomacG2, overwrite = TRUE)
usethis::use_data(stomac_soles, overwrite = TRUE)
