####################################
##         PROJET CHOPIN
##
##    MISE EN FORME DES DONNEES
##            SOLES
####################################

# Loadings

## Load packages

library(tidyverse)
library(readxl)

## Load contaminant lists and labels
# File "1_data_formatting_contaminants.R" should be ran previously
# If not, run the following script
source(file = "data-raw/data_formatting_contaminants.R")


## Load data

# contam générale
soles_contam = read_excel(here("data-raw/CHOPIN_BASE_DE_DONNEES_GENERALE.xlsx"),
                        sheet= "soles")

# détail des pools G0
solesG0_pools = read_excel(here("data-raw/CHOPIN_BASE_DE_DONNEES_GENERALE.xlsx"),
                          sheet= "solesG0pools")

# Save sub-datasets
write_csv(x = solesG0_pools[,c("zone","season")],
          file = "data-raw/effectifs_poolG0_zone_saison.csv")

write_csv(x = solesG0_pools[,c("zone","season","sample_TAG")],
          file = "data-raw/effectifs_poolG0_pools.csv")

# organotropisme G2

soles_contam_G2orga = read_excel(here("data-raw/CHOPIN_BASE_DE_DONNEES_GENERALE.xlsx"),
                                 sheet= "solesG2organotr")

soles_contam_G2orga$tissue = as.character(soles_contam_G2orga$tissue)
soles_contam_G2orga$tissue = factor(soles_contam_G2orga$tissue,
                                    levels = levels(as.factor(soles_contam_G2orga$tissue)),
                                    labels = c("gonades","foie","reste","entier"))
colnames(soles_contam_G2orga)[2] = "Tissus"

# --------------------------------------------------------------
# # pg => ng

soles_contam$`a-HBCDD_ng_g-1ps` = soles_contam$`a-HBCDD_pg_g-1ps`/1000
soles_contam$`b-HBCDD_ng_g-1ps` = soles_contam$`b-HBCDD_pg_g-1ps`/1000
soles_contam$`g-HBCDD_ng_g-1ps` = soles_contam$`g-HBCDD_pg_g-1ps`/1000

# --------------------------------------------------------------
# Suppression des poissons non analyses

soles_contam = soles_contam[-which(soles_contam$sample_TAG=="112018 G2 08"|
                                     soles_contam$sample_TAG=="112018-G2-16"),]


# --------------------------------------------------------------
# Suppression des poissons G2 "WB_reconst" pour ceux en doublon

soles_contam = soles_contam[-which(soles_contam$sample_TAG=="112018-G2-12"&
                                     soles_contam$sample_type=="WB_reconst"),]
soles_contam = soles_contam[-which(soles_contam$sample_TAG=="112018-G2-13"&
                                     soles_contam$sample_type=="WB_reconst"),]
soles_contam = soles_contam[-which(soles_contam$sample_TAG=="112018-G2-15"&
                                     soles_contam$sample_type=="WB_reconst"),]

# --------------------------------------------------------------
# Calcul des masses seches

soles_contam$mass_tot_gdw = soles_contam$mass_tot_gww*((100-soles_contam$water_percent)/100)


# --------------------------------------------------------------
# Remplacement des valeurs <LOD/LOQ par 0 pour les HBCDD

soles_contam$`a-HBCDD_ng_g-1ps`[which(soles_contam$sample_TAG=="112018-G2-10")]=0

soles_contam$`b-HBCDD_ng_g-1ps`[which(is.na(soles_contam$'a-HBCDD_ng_g-1ps')==F&
                                        soles_contam$sample_TAG!="FN G0 juin 2017 pool2"&
                                        soles_contam$sample_TAG!="FS G0 juin 2017 pool2"&
                                        soles_contam$sample_TAG!="0617 Pool CH Soles G0"&
                                        soles_contam$sample_TAG!="092018-G1-10"&
                                        soles_contam$sample_TAG!="092018-G1-12")]=0

soles_contam$`g-HBCDD_ng_g-1ps`[which(is.na(soles_contam$'g-HBCDD_ng_g-1ps')==F&
                                        soles_contam$sample_TAG!="FN G0 juin 2017 pool2"&
                                        soles_contam$sample_TAG!="0617 Pool FS Soles G0"&
                                        soles_contam$sample_TAG!="FS G0 juin 2017 pool2"&
                                        soles_contam$sample_TAG!="0617 Pool CH Soles G0"&
                                        soles_contam$sample_TAG!="1017 FS G0 pool 3"&
                                        soles_contam$sample_TAG!="092018-G1-10"&
                                        soles_contam$sample_TAG!="092018-G1-12"&
                                        soles_contam$sample_TAG!="092018-G1-17")]=0


# --------------------------------------------------------------
# Calcul des concentrations en poids de lipides

for(c in 1:length(PCB)){
  soles_contam[,paste(PCB[c],"_ng_g.1pl",sep="")] = soles_contam[,paste(PCB[c],"_ng_g-1ps",sep="")]/(soles_contam$lipid_percent_dw/100)
}

for(c in 1:length(HBCDD)){
  soles_contam[,paste(HBCDD[c],"_ng_g.1pl",sep="")] = soles_contam[,paste(HBCDD[c],"_ng_g-1ps",sep="")]/(soles_contam$lipid_percent_dw/100)
}


# --------------------------------------------------------------
# Calcul des concentrations en poids frais

for(c in 1:length(PCB)){
  soles_contam[,paste(PCB[c],"_ng_g.1ww",sep="")] = soles_contam[,paste(PCB[c],"_ng_g-1ps",sep="")]*((100-soles_contam$water_percent)/100)
}

for(c in 1:length(PFAS)){
  soles_contam[,paste(PFAS[c],"_ng_g.1ww",sep="")] = soles_contam[,PFAS[c]]*((100-soles_contam$water_percent)/100)
}

for(c in 1:length(HBCDD)){
  soles_contam[,paste(HBCDD[c],"_ng_g.1ww",sep="")] = soles_contam[,paste(HBCDD[c],"_ng_g-1ps",sep="")]*((100-soles_contam$water_percent)/100)
}


# --------------------------------------------------------------
# Calcul des quantites de contaminants

for(c in 1:length(PCB)){
  soles_contam[,paste(PCB[c],"_ng",sep="")] = soles_contam[,paste(PCB[c],"_ng_g-1ps",sep="")]*soles_contam$mass_tot_gdw
}
for(c in 1:length(PFAS)){
  soles_contam[,paste(PFAS[c],"_ng",sep="")] = soles_contam[,PFAS[c]]*soles_contam$mass_tot_gdw
}

for(c in 1:length(HBCDD)){
  soles_contam[,paste(HBCDD[c],"_ng",sep="")] = soles_contam[,paste(HBCDD[c],"_ng_g-1ps",sep="")]*soles_contam$mass_tot_gdw
}


# --------------------------------------------------------------
# Transformation variable saison en date =>Printemps & Automne

soles_contam$season = as.character(soles_contam$season)
soles_contam$season[which(soles_contam$season=="2017-06-01")] = "Printemps"
soles_contam$season[which(soles_contam$season=="2017-10-01"|
                            soles_contam$season=="2018-09-01"|
                            soles_contam$season=="2018-11-01")]  ="Automne"
soles_contam$season = as.factor(soles_contam$season)


# --------------------------------------------------------------
# Transformation zone NA des G2 en "Embouchure"

soles_contam$zone = as.character(soles_contam$zone)
soles_contam$zone[which(is.na(soles_contam$zone==T))] = "Embouchure"
soles_contam$zone = as.factor(soles_contam$zone)


# --------------------------------------------------------------
# Calcul des sommes par famille

soles_contam$sommePCB_ng_gdw = apply(soles_contam[,paste(PCB,"_ng_g-1ps",sep="")], MARGIN = 1, FUN = sum)

soles_contam$sommePFAS_ng_gdw = apply(soles_contam[,PFAS], MARGIN = 1, FUN = sum)

soles_contam$sommePFCA_ng_gdw = apply(soles_contam[,PFCAs], MARGIN = 1, FUN = sum)
soles_contam$sommeFOSA_ng_gdw = apply(soles_contam[,FOSAs], MARGIN = 1, FUN = sum)
soles_contam$sommePFSA_ng_gdw = apply(soles_contam[,PFSAs], MARGIN = 1, FUN = sum)
soles_contam$sommeautres_ng_gdw = apply(soles_contam[,c(other_PFAS, FTSAs, diPAP)], MARGIN = 1, FUN = sum)

soles_contam$sommeHBCDD_ng_gdw = apply(soles_contam[,paste(HBCDD,"_ng_g-1ps",sep="")], MARGIN = 1, FUN = sum)


# --------------------------------------------------------------
# Changement nom classe d'age

colnames(soles_contam)[which(colnames(soles_contam)=="type")] ="grp"


# --------------------------------------------------------------
# Calcul indices condition hépatique et gonadique

soles_contam$RHS = soles_contam$mass_liver_gww_sd/soles_contam$mass_tot_gww*100
soles_contam$RGS = soles_contam$mass_gods_gww_sd/soles_contam$mass_tot_gww*100


# --------------------------------------------------------------
# Calcul des concentrations normalisées par la somme au sein d'une famille ( ng_gdw )

soles_contam[,paste(PCB,"normalised_sum_ng.gdw",sep="_")] = soles_contam[,paste(PCB,"_ng_g-1ps",sep="")]/soles_contam$sommePCB_ng_gdw
soles_contam[,paste(PFAS,"normalised_sum_ng.gdw",sep="_")] = soles_contam[,PFAS]/soles_contam$sommePFAS_ng_gdw
soles_contam[,paste(HBCDD,"normalised_sum_ng.gdw",sep="_")] = soles_contam[,paste(HBCDD,"_ng_g-1ps",sep="")]/soles_contam$sommeHBCDD_ng_gdw

# --------------------------------------------------------------
# Save dataset
write_csv(x = soles_contam, file = "data-raw/sole_contam.csv")


# --------------------------------------------------------------
# Subset par classe d'age

solesG0_contam = soles_contam[which(soles_contam$grp=="G0"),]
solesG1_contam = soles_contam[which(soles_contam$grp=="G1"),]
solesG2_contam = soles_contam[which(soles_contam$grp=="G2"),]


# --------------------------------------------------------------
# Subset par cohorte (tableau complet)

soles_contam_cohorte2017 = rbind(soles_contam[which(soles_contam$grp=="G0"),],
                                 soles_contam[which(soles_contam$grp=="G1"&soles_contam$year=="2018"),])
soles_contam_cohorte2016 = rbind(soles_contam[which(soles_contam$grp=="G1"&soles_contam$year=="2017"),],
                                 soles_contam[which(soles_contam$grp=="G2"&soles_contam$year=="2018"),])


# --------------------------------------------------------------
# Subset par saison (tableau complet)

soles_contam_juin = soles_contam[which(soles_contam$season=="Printemps"),]
soles_contam_oct = soles_contam[which(soles_contam$season=="Automne"),]

# --------------------------------------------------------------
# Output data

usethis::use_data(soles_contam, overwrite = TRUE)
usethis::use_data(solesG0_pools, overwrite = TRUE)
usethis::use_data(soles_contam_G2orga, overwrite = TRUE)

usethis::use_data(solesG0_contam, overwrite = TRUE)
usethis::use_data(solesG1_contam, overwrite = TRUE)
usethis::use_data(solesG2_contam, overwrite = TRUE)

usethis::use_data(soles_contam_cohorte2017, overwrite = TRUE)
usethis::use_data(soles_contam_cohorte2016, overwrite = TRUE)

usethis::use_data(soles_contam_juin, overwrite = TRUE)
usethis::use_data(soles_contam_oct, overwrite = TRUE)

