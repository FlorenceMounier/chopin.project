####################################
##         PROJET CHOPIN
##
##    MISE EN FORME DES DONNEES
##           BENTHOS
####################################

# Loadings

## Load packages

library(tidyverse)
library(readxl)

## Load contaminant lists and labels
# File "1_data_formatting_contaminants.R" should be ran previously
# If not, run the following script
# source(file = "data-raw/data_formatting_contaminants.R")

## Load data

benthos_contam = read_excel("data-raw/CHOPIN_BASE_DE_DONNEES_GENERALE.xlsx",
                           sheet = "benthos")


benthos_contam$zone = as.factor(benthos_contam$zone)
benthos_contam$species = as.factor(benthos_contam$species)


# Remplacement des valeurs <LOD/LOQ par 0 pour les HBCDD

benthos_contam$`a-HBCDD_ng_g.1ps_censored` = benthos_contam$`a-HBCDD_ng_g-1ps`
benthos_contam$`a-HBCDD_ng_g.1ps_censored`[which(benthos_contam$sample_TAG=="CP14"|
                                                 benthos_contam$sample_TAG=="CP06"|
                                                 benthos_contam$sample_TAG=="CP52"|
                                                 benthos_contam$sample_TAG=="CP53")]=0

benthos_contam$`b-HBCDD_ng_g.1ps_censored` = benthos_contam$`b-HBCDD_ng_g-1ps`
benthos_contam$`b-HBCDD_ng_g.1ps_censored`[which(benthos_contam$sample_TAG=="CP12"|
                                                 benthos_contam$sample_TAG=="CP14"|
                                                 benthos_contam$sample_TAG=="CP06"|
                                                 benthos_contam$sample_TAG=="CP52"|
                                                 benthos_contam$sample_TAG=="CP41"|
                                                 benthos_contam$sample_TAG=="CP44"|
                                                 benthos_contam$sample_TAG=="T2 FN8 crevettes"|
                                                 benthos_contam$sample_TAG=="CP43"|
                                                 benthos_contam$sample_TAG=="CP53")]=0

benthos_contam$`g-HBCDD_ng_g.1ps_censored` = benthos_contam$`g-HBCDD_ng_g-1ps`
benthos_contam$`g-HBCDD_ng_g.1ps_censored`[benthos_contam$sample_TAG=="CP14"|
                                           benthos_contam$sample_TAG=="CP06"|
                                           benthos_contam$sample_TAG=="CP52"|
                                           benthos_contam$sample_TAG=="CP41"|
                                           benthos_contam$sample_TAG=="CP44"|
                                           benthos_contam$sample_TAG=="T2 FN8 crevettes"|
                                           benthos_contam$sample_TAG=="CP53"] = 0


# Transformation variable saison juin=>Printemps, octobre=> Automne

benthos_contam$season = factor(benthos_contam$season,labels =c("Printemps","Automne"))

# Creation des variables taxon (grp) et labels

alim = list("Susp.Dep.sur." = c("Abra_alba","Cerastoderma_edule", "Limecola_balthica","Scrobicularia_plana",
                                "Corophium_volutator","Lanice_conchilega",
                                "Owenia_fusiformis"),
            "Susp."=c("Corbula_gibba","Donax_vittatus","Ensis_directus","Spisula_subtruncata"),
            "Dep.sur."=c("Nucula_nitidosa"),
            "Dep.sub."=c("Lagis_koreni"),
            "Omnivore"=c("Nephtys_sp","Hediste_diversicolor","Crangon_crangon"))

labels = list("Bivalves" = c("Abra a."="Abra_alba","Spisula s."="Spisula_subtruncata",
                             "Limecola b."="Limecola_balthica","Cerastoderma e."="Cerastoderma_edule",
                             "Donax v."="Donax_vittatus","Corbula g."="Corbula_gibba",
                             "Scrobicularia p."="Scrobicularia_plana","Nucula n."="Nucula_nitidosa",
                             "Ensis d."="Ensis_directus"),
              "Polychetes" = c("Nephtys sp."="Nephtys_sp", "Owenia f."="Owenia_fusiformis",
                               "Lagis k."="Lagis_koreni", "Lanice c."="Lanice_conchilega",
                               "Hediste d."="Hediste_diversicolor"),
              "Crustaces" = c("Crangon c."="Crangon_crangon", "Corophium v."="Corophium_volutator"))

taxons = list("Bivalves" = c("Abra_alba","Spisula_subtruncata", "Limecola_balthica", "Cerastoderma_edule", "Donax_vittatus",
                             "Corbula_gibba", "Scrobicularia_plana","Nucula_nitidosa","Ensis_directus"),
              "Polychetes" = c("Nephtys_sp", "Owenia_fusiformis", "Lagis_koreni", "Lanice_conchilega", "Hediste_diversicolor"),
              "Crustaces" = c("Crangon_crangon", "Corophium_volutator"))

benthos_contam$labels = rep.int(NA, times = dim(benthos_contam)[1])
benthos_contam$grp = rep.int(NA, times = dim(benthos_contam)[1])
benthos_contam$alim = rep.int(NA, times = dim(benthos_contam)[1])
for(l in 1:3){
  for(s in 1:length(taxons[[l]])){
    benthos_contam$labels[which(as.character(benthos_contam$species) == as.character(labels[[l]][s]))] = names(labels[[l]])[s]
    benthos_contam$grp[which(as.character(benthos_contam$species) == as.character(taxons[[l]][s]))] = names(taxons)[l]
  }
}

for(l in 1:5){
  for(s in 1:length(alim[[l]])){
    benthos_contam$alim[which(as.character(benthos_contam$species) == as.character(alim[[l]][s]))] = names(alim)[l]
  }
}

# Calcul des concentrations en poids de lipides ( ng_g.1pl )

for(c in 1:length(PCB)){
  benthos_contam[,paste(PCB[c],"_ng_g.1pl",sep="")] = benthos_contam[,paste(PCB[c],"_ng_g-1ps",sep="")]/(benthos_contam$lip_PS_percent/100)
}

for(c in 1:length(HBCDD)){
  benthos_contam[,paste(HBCDD[c],"_ng_g.1pl",sep="")] = benthos_contam[,paste(HBCDD[c],"_ng_g.1ps_censored",sep="")]/(benthos_contam$lip_PS_percent/100)
}

# Calcul des concentrations en poids frais ( ng_g.1ww )

for(c in 1:length(PCB)){
  benthos_contam[,paste(PCB[c],"_ng_g.1ww",sep="")] = benthos_contam[,paste(PCB[c],"_ng_g-1ps",sep="")]*((benthos_contam$mat_seche_percent)/100)
}

for(c in 1:length(PFAS)){
  benthos_contam[,paste(PFAS[c],"_ng_g.1ww",sep="")] = benthos_contam[,PFAS[c]]*((benthos_contam$mat_seche_percent)/100)
}

for(c in 1:length(HBCDD)){
  benthos_contam[,paste(HBCDD[c],"_ng_g.1ww",sep="")] = benthos_contam[,paste(HBCDD[c],"_ng_g.1ps_censored",sep="")]*((benthos_contam$mat_seche_percent)/100)
}

# Calcul des sommes par famille en poids sex ( ng_g.1ps )

benthos_contam$sommePCB_ng_gdw = apply(benthos_contam[,paste(PCB,"_ng_g-1ps",sep="")], MARGIN = 1, FUN = sum)
benthos_contam$sommePFAS_ng_gdw = apply(benthos_contam[,PFAS], MARGIN = 1, FUN = sum)
benthos_contam$sommePFCA_ng_gdw = apply(benthos_contam[,PFCAs], MARGIN = 1, FUN = sum)
benthos_contam$sommeFOSA_ng_gdw = apply(benthos_contam[,FOSAs], MARGIN = 1, FUN = sum)
benthos_contam$sommePFSA_ng_gdw = apply(benthos_contam[,PFSAs], MARGIN = 1, FUN = sum)
benthos_contam$sommeHBCDD_ng_gdw = apply(benthos_contam[,paste(HBCDD,"_ng_g.1ps_censored",sep="")], MARGIN = 1, FUN = sum)

# Calcul des concentrations normalisées par la somme au sein d'une famille ( ng_gdw )

benthos_contam[, paste(PCB, "normalised_sum_ng.gdw", sep = "_")] = benthos_contam[, paste(PCB, "_ng_g-1ps", sep = "")] / benthos_contam$sommePCB_ng_gdw

benthos_contam[, paste(PFAS, "normalised_sum_ng.gdw", sep = "_")] = benthos_contam[, PFAS] /
  benthos_contam$sommePFAS_ng_gdw

benthos_contam[, paste(HBCDD, "normalised_sum_ng.gdw", sep = "_")] = benthos_contam[, paste(HBCDD, "_ng_g.1ps_censored", sep =
                                                                                              "")] / benthos_contam$sommeHBCDD_ng_gdw
# Save dataset
write_csv(x = benthos_contam, file = "data-raw/benthos_contam.csv")


# Subset par saison (tableau complet)

benthos_contam_juin = benthos_contam[which(benthos_contam$season=="Printemps"),]
benthos_contam_oct = benthos_contam[which(benthos_contam$season=="Automne"),]


# Output data

usethis::use_data(benthos_contam, overwrite = TRUE)
usethis::use_data(benthos_contam_juin, overwrite = TRUE)
usethis::use_data(benthos_contam_oct, overwrite = TRUE)

usethis::use_data(alim, overwrite = TRUE)
usethis::use_data(labels, overwrite = TRUE)
usethis::use_data(taxons, overwrite = TRUE)
