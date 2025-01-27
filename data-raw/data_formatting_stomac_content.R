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

# summary(as.actor(stomac_soles$Statut))


stomac_soles$`Nbr total tractus` = as.numeric(stomac_soles$`Nbr total tractus`)
stomac_soles$ScientificName_accepted = as.factor(stomac_soles$ScientificName_accepted)
# levels(stomac_soles$ScientificName_accepted)
stomac_soles$`espèce/stade` = as.factor(stomac_soles$`espèce/stade`)
# levels(stomac_soles$`espèce/stade`)
stomac_soles$`N.poissons` = as.factor(stomac_soles$`N°poissons`)
# length(levels(stomac_soles$`N°poissons`))
stomac_soles$secteur = as.factor(stomac_soles$secteur)
# levels(stomac_soles$secteur)

stomac_soles = data.frame(stomac_soles)

# Table of species names and corresponding taxa

table_species = levels(stomac_soles$ScientificName_accepted)
vect_taxon = c()
for(s in 1:length(levels(stomac_soles$ScientificName_accepted))){
  vect_taxon = c(vect_taxon, stomac_soles$Groupe.faunistique[which(stomac_soles$ScientificName_accepted==levels(stomac_soles$ScientificName_accepted)[s])][1])
}

table_species = data.frame("species"=table_species, "taxon"=vect_taxon)

# Output data

usethis::use_data(stomacG0G1, overwrite = TRUE)
usethis::use_data(stomacG2, overwrite = TRUE)
usethis::use_data(stomac_soles, overwrite = TRUE)
usethis::use_data(table_species, overwrite = TRUE)
