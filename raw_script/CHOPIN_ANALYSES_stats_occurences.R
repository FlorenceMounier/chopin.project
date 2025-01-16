####################################
##         PROJET CHOPIN
## 
##      ANALYSES DESCRIPTIVES
##   FRQ DETECTION CONTAMINANTS
##      
####################################


# fonction de calcul de la fréquence d'occurence (concentration != 0) parmi les échantillons analysés (non NA)
occ = function(vector){
  length(which(na.omit(vector)!=0)) / length(na.omit(vector))
}

# --------------------------------------------------------------
# SEDIMENT

# year season zone echantillon

PCB_selected_sed_abund = PCB[which(apply(sed_contam[,paste(PCB, "normalised_sum_ng.gdw", sep="_")], MARGIN = 2, FUN = max, na.rm=T)>0.05)]
PCB_selected_sed_occ = PCB[which(apply(sed_contam[,paste(PCB, "normalised_sum_ng.gdw", sep="_")], MARGIN = 2, FUN = occ)>0.5)]
PCB_selected_sed = intersect(PCB_selected_sed_abund, PCB_selected_sed_occ)

PFAS_selected_sed_abund = PFAS[which(apply(sed_contam[,paste(PFAS, "normalised_sum_ng.gdw", sep="_")], MARGIN = 2, FUN = max, na.rm=T)>0.05)]
PFAS_selected_sed_occ = PFAS[which(apply(sed_contam[,paste(PFAS, "normalised_sum_ng.gdw", sep="_")], MARGIN = 2, FUN = occ)>0.5)]
PFAS_selected_sed = intersect(PFAS_selected_sed_abund, PFAS_selected_sed_occ)

HBCDD_selected_sed_abund = HBCDD[which(apply(sed_contam[,paste(HBCDD, "normalised_sum_ng.gdw", sep="_")], MARGIN = 2, FUN = max, na.rm=T)>0.05)]
HBCDD_selected_sed_occ = HBCDD[which(apply(sed_contam[,paste(HBCDD, "normalised_sum_ng.gdw", sep="_")], MARGIN = 2, FUN = occ)>0.5)]
HBCDD_selected_sed = intersect(HBCDD_selected_sed_abund, HBCDD_selected_sed_occ)




# --------------------------------------------------------------
# BENTHOS

#  benthos_contam_PCB_normalised : season zone species sample_TAG grp alim

PCB_selected_benthos_abund = PCB[which(apply(benthos_contam[,paste(PCB, "normalised_sum_ng.gdw", sep="_")], MARGIN = 2, FUN = max, na.rm=T)>0.05)]
PCB_selected_benthos_occ = PCB[which(apply(benthos_contam[,paste(PCB, "normalised_sum_ng.gdw", sep="_")], MARGIN = 2, FUN = occ)>0.5)]
PCB_selected_benthos = intersect(PCB_selected_benthos_abund, PCB_selected_benthos_occ)

PFAS_selected_benthos_abund = PFAS[which(apply(benthos_contam[,paste(PFAS, "normalised_sum_ng.gdw", sep="_")], MARGIN = 2, FUN = max, na.rm=T)>0.05)]
PFAS_selected_benthos_occ = PFAS[which(apply(benthos_contam[,paste(PFAS, "normalised_sum_ng.gdw", sep="_")], MARGIN = 2, FUN = occ)>0.5)]
PFAS_selected_benthos = intersect(PFAS_selected_benthos_abund, PFAS_selected_benthos_occ)

HBCDD_selected_benthos_abund = HBCDD[which(apply(benthos_contam[,paste(HBCDD, "normalised_sum_ng.gdw", sep="_")], MARGIN = 2, FUN = max, na.rm=T)>0.05)]
HBCDD_selected_benthos_occ = HBCDD[which(apply(benthos_contam[,paste(HBCDD, "normalised_sum_ng.gdw", sep="_")], MARGIN = 2, FUN = occ)>0.5)]
HBCDD_selected_benthos = intersect(HBCDD_selected_benthos_abund, HBCDD_selected_benthos_occ)



# --------------------------------------------------------------
# SOLES

# c("year"         "season"       "zone"         "grp"          "sample_TAG"   "sexe"         "length_TL_cm" "mass_tot_gww")

PCB_selected_soles_abund = PCB[which(apply(soles_contam[,paste(PCB, "normalised_sum_ng.gdw", sep="_")], MARGIN = 2, FUN = max, na.rm=T)>0.05)]
PCB_selected_soles_occ = PCB[which(apply(soles_contam[,paste(PCB, "normalised_sum_ng.gdw", sep="_")], MARGIN = 2, FUN = occ)>0.5)]
PCB_selected_soles = intersect(PCB_selected_soles_abund, PCB_selected_soles_occ)

PFAS_selected_soles_abund = PFAS[which(apply(soles_contam[,paste(PFAS, "normalised_sum_ng.gdw", sep="_")], MARGIN = 2, FUN = max, na.rm=T)>0.05)]
PFAS_selected_soles_occ = PFAS[which(apply(soles_contam[,paste(PFAS, "normalised_sum_ng.gdw", sep="_")], MARGIN = 2, FUN = occ)>0.5)]
PFAS_selected_soles = intersect(PFAS_selected_soles_abund, PFAS_selected_soles_occ)

HBCDD_selected_soles_abund = HBCDD[which(apply(soles_contam[,paste(HBCDD, "normalised_sum_ng.gdw", sep="_")], MARGIN = 2, FUN = max, na.rm=T)>0.05)]
HBCDD_selected_soles_occ = HBCDD[which(apply(soles_contam[,paste(HBCDD, "normalised_sum_ng.gdw", sep="_")], MARGIN = 2, FUN = occ)>0.5)]
HBCDD_selected_soles = intersect(HBCDD_selected_soles_abund, HBCDD_selected_soles_occ)




