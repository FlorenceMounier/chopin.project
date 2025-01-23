####################################
##         PROJET CHOPIN
##
##    MISE EN FORME DES DONNEES
##           ISOTOPES
####################################


#--------------------------------------
# Load packages

library(tidyverse)
library(readxl)
library(chopin.project)


#--------------------------------------
# Data isotopes benthos

## Load data
benthos_isotopes = read_excel("data-raw/CHOPIN_BASE_DE_DONNEES_GENERALE.xlsx",
                            sheet = "isotopes_benth_CAPES")

## Gestion des noms d'espèces dans une variable factorielle 'species'
benthos_isotopes$species = as.character(benthos_isotopes$espece)
benthos_isotopes$species[which(benthos_isotopes$espece=="Nephtys"|
                                 benthos_isotopes$espece=="Nephtys_assimilis"|
                                 benthos_isotopes$espece=="Nephtys_caeca"|
                                 benthos_isotopes$espece=="Nephtys_cirrosa"|
                                 benthos_isotopes$espece=="Nephtys_hombergii")] = "Nephtys_sp"
benthos_isotopes$species[which(benthos_isotopes$species=="Limecola_baltica")] = "Limecola_balthica"
benthos_isotopes$species = as.factor(benthos_isotopes$species)

benthos_isotopes$labels = rep.int(NA, times = dim(benthos_isotopes)[1])
benthos_isotopes$grp = rep.int(NA, times = dim(benthos_isotopes)[1])
for(l in 1:3){
  for(s in 1:length(taxons[[l]])){
    benthos_isotopes$labels[which(as.character(benthos_isotopes$species) == as.character(labels[[l]][s]))] = names(labels[[l]])[s]
    benthos_isotopes$grp[which(as.character(benthos_isotopes$species) == as.character(taxons[[l]][s]))] = names(taxons)[l]
  }
}

## Tri des échantillons par projet source

isotopes_CAPES = data.frame("species"=benthos_isotopes$species,
                            "d13C" = benthos_isotopes$d13C,
                            "d15N" = benthos_isotopes$d15N,
                            "sample_TAG" = benthos_isotopes$CODE_ECHANTILLON,
                            "zone" = benthos_isotopes$`Secteur/zone`,
                            "season" = factor(benthos_isotopes$Campagne,labels = c("Automne","Printemps")),
                            "grp" = benthos_isotopes$grp,
                            "labels" = benthos_isotopes$labels,
                            "source" = rep.int("CAPES", times = dim(benthos_isotopes)[1]))

isotopes_CHOPIN = data.frame("species"=benthos_contam$species[-which(is.na(benthos_contam$`d 13C/12C ‰`)==T)],
                             "d13C"= benthos_contam$`d 13C/12C ‰`[-which(is.na(benthos_contam$`d 13C/12C ‰`)==T)],
                             "d15N" = benthos_contam$`d 15N/14N ‰`[-which(is.na(benthos_contam$`d 13C/12C ‰`)==T)],
                             "sample_TAG" = benthos_contam$sample_TAG[-which(is.na(benthos_contam$`d 13C/12C ‰`)==T)],
                             "zone" = benthos_contam$zone[-which(is.na(benthos_contam$`d 13C/12C ‰`)==T)],
                             "season" = benthos_contam$season[-which(is.na(benthos_contam$`d 13C/12C ‰`)==T)],
                             "grp" = benthos_contam$grp[-which(is.na(benthos_contam$`d 13C/12C ‰`)==T)],
                             "labels" = benthos_contam$labels[-which(is.na(benthos_contam$`d 13C/12C ‰`)==T)],
                             "source" = rep.int("CHOPIN", times = length(benthos_contam$labels[-which(is.na(benthos_contam$`d 13C/12C ‰`)==T)])))

## Save dataset

ISOTOPES_benthos = rbind(isotopes_CAPES,isotopes_CHOPIN)
ISOTOPES_benthos$md13C = -ISOTOPES_benthos$d13C

write_csv(x = ISOTOPES_benthos, file = "data-raw/benthos_isotopes.csv")

## Subset par saison (tableau complet)

ISOTOPES_benthos_juin = ISOTOPES_benthos[which(ISOTOPES_benthos$season=="Printemps"),]
ISOTOPES_benthos_juin = ISOTOPES_benthos_juin[order(ISOTOPES_benthos_juin$zone),]
ISOTOPES_benthos_oct = ISOTOPES_benthos[which(ISOTOPES_benthos$season=="Automne"),]
ISOTOPES_benthos_oct = ISOTOPES_benthos_oct[order(ISOTOPES_benthos_oct$zone),]

#--------------------------------------
# Data isotopes benthos

## Load data

CAPES_fish_isotopes = read_excel("data-raw/CHOPIN_BASE_DE_DONNEES_GENERALE.xlsx",
                                 sheet = "isotopes_fish_CAPES")

## Mise en forme des variables d'intérêt
soles_isotopes = CAPES_fish_isotopes[CAPES_fish_isotopes$espece == "SOLESOL", ]
soles_isotopes$`Secteur/zone` = as.factor(soles_isotopes$`Secteur/zone`)
soles_isotopes$`Secteur/zone` = factor(
  soles_isotopes$`Secteur/zone`,
  levels = levels(soles_isotopes$`Secteur/zone`),
  labels = c("Chenal", "Embouchure", "Fosse Nord", "Fosse Sud")
)

ISOTOPES_soles = data.frame(
  "grp" = c(
    as.character(soles_isotopes$stade),
    as.character(soles_contam$grp)
  ),
  "season" = c(
    as.character(soles_isotopes$Campagne),
    as.character(soles_contam$season)
  ),
  "zone" = c(
    as.character(soles_isotopes$`Secteur/zone`),
    as.character(soles_contam$zone)
  ),
  "d13C" = c(soles_isotopes$d13C, soles_contam$`d 13C/12C ‰`),
  "d15N" = c(soles_isotopes$d15N, soles_contam$`d 15N/14N ‰`)
)

ISOTOPES_soles$zone = as.factor(ISOTOPES_soles$zone)
ISOTOPES_soles$zone = factor(
  ISOTOPES_soles$zone,
  levels = levels(ISOTOPES_soles$zone),
  labels = c("CH", "EMB", "FN", "FS")
)

## Save dataset
write_csv(x = ISOTOPES_soles, file = "data-raw/soles_isotopes.csv")

## Subset par classe d'âge
ISOTOPES_soles_G0 = ISOTOPES_soles[which(ISOTOPES_soles$grp == "G0"), ]
ISOTOPES_soles_G1_G2 = ISOTOPES_soles[which(ISOTOPES_soles$grp == "G1" |
                                  ISOTOPES_soles$grp == "G2"), ]



#--------------------------------------
# Output data

usethis::use_data(benthos_isotopes, overwrite = TRUE)
usethis::use_data(isotopes_CAPES, overwrite = TRUE)
usethis::use_data(isotopes_CHOPIN, overwrite = TRUE)
usethis::use_data(ISOTOPES_benthos, overwrite = TRUE)
usethis::use_data(ISOTOPES_benthos_juin, overwrite = TRUE)
usethis::use_data(ISOTOPES_benthos_oct, overwrite = TRUE)

usethis::use_data(soles_isotopes, overwrite = TRUE)
usethis::use_data(ISOTOPES_soles, overwrite = TRUE)
usethis::use_data(ISOTOPES_soles_G0, overwrite = TRUE)
usethis::use_data(ISOTOPES_soles_G1_G2, overwrite = TRUE)
