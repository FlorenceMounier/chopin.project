####################################
##         PROJET CHOPIN
## 
##    MISE EN FORME DES DONNEES
##           SEDIMENTS
####################################



# --------------------------------------------------------------
# Chargement des donnees depuis la base excel

sed_contam = read.xlsx(path_CHOPIN_BDD,
                       sheet= "sediments")


# --------------------------------------------------------------
# Suppression des composes non analyses (le cas echeant)

PFAS = PFAS_ALL
PFAS_lab = PFAS_ALL_lab
ss_famille = ss_famille_ALL
n_C = n_C_ALL
FOSAs = FOSAs_ALL
FOSAs_lab = FOSAs_ALL_lab

# --------------------------------------------------------------
# Transformation variable saison juin=>Printemps, octobre=> Automne

sed_contam$season = factor(sed_contam$season,labels =c("Printemps","Automne"))


# --------------------------------------------------------------
# Calcul des concentrations en poids de Corg

for(c in 1:length(PCB)){
  sed_contam[,paste(PCB[c],"_ng_g.1Corg",sep="")] = sed_contam[,PCB[c]] / (sed_contam$`Corg_mg_g-1`/1000)
}

for(c in 1:length(PFAS)){
  sed_contam[,paste(PFAS[c],"_ng_g.1Corg",sep="")] = sed_contam[,PFAS[c]] / (sed_contam$`Corg_mg_g-1`/1000)
}

for(c in 1:length(HBCDD)){
  sed_contam[,paste(HBCDD[c],"_ng_g.1Corg",sep="")] = sed_contam[,HBCDD[c]] / (sed_contam$`Corg_mg_g-1`/1000)
}


# --------------------------------------------------------------
# Calcul des sommes par famille /!\ ng/gCorg

sed_contam$sommePCB_ng_g.1Corg = apply(sed_contam[,paste(PCB,"_ng_g.1Corg",sep="")], MARGIN = 1, FUN = sum)


sed_contam$sommePFAS_ng_g.1Corg = apply(sed_contam[,paste(PFAS,"_ng_g.1Corg",sep="")], MARGIN = 1, FUN = sum)

sed_contam$sommePFCA_ng_g.1Corg = apply(sed_contam[,paste(PFCA,"_ng_g.1Corg",sep="")], MARGIN = 1, FUN = sum)
sed_contam$sommeFOSA_ng_g.1Corg = apply(sed_contam[,paste(FOSAs,"_ng_g.1Corg",sep="")], MARGIN = 1, FUN = sum)
sed_contam$sommePFSA_ng_g.1Corg = apply(sed_contam[,paste(PFSA,"_ng_g.1Corg",sep="")], MARGIN = 1, FUN = sum)
sed_contam$sommeautres_ng_g.1Corg = apply(sed_contam[,paste(c(otherPFASs, FTS, diPAP),"_ng_g.1Corg",sep="")], MARGIN = 1, FUN = sum)


sed_contam$sommeHBCDD_ng_g.1Corg = apply(sed_contam[,paste(HBCDD,"_ng_g.1Corg",sep="")], MARGIN = 1, FUN = sum)


# --------------------------------------------------------------
# Calcul des sommes par famille ng/gps


sed_contam$sommePCB_ng_gdw = apply(sed_contam[,PCB], MARGIN = 1, FUN = sum)

sed_contam$sommePFAS_ng_gdw = apply(sed_contam[,PFAS], MARGIN = 1, FUN = sum)

sed_contam$sommePFCA_ng_gdw = apply(sed_contam[,PFCA], MARGIN = 1, FUN = sum)

sed_contam$sommeFOSA_ng_gdw = apply(sed_contam[,FOSAs], MARGIN = 1, FUN = sum)

sed_contam$sommePFSA_ng_gdw = apply(sed_contam[,PFSA], MARGIN = 1, FUN = sum)

sed_contam$sommeautres_ng_gdw = apply(sed_contam[,c(otherPFASs, FTS, diPAP)], MARGIN = 1, FUN = sum)

sed_contam$sommeHBCDD_ng_gdw = apply(sed_contam[,HBCDD], MARGIN = 1, FUN = sum)


# --------------------------------------------------------------
# Calcul des concentrations normalisées par la somme au sein d'une famille ( ng_gdw )


# Add normalised concentrations for PCBs
sed_contam = cbind(sed_contam, sed_contam[,PCB]/sed_contam$sommePCB_ng_gdw)
colnames(sed_contam)[c((dim(sed_contam)[2]-length(PCB)+1):dim(sed_contam)[2])] =  paste(
  colnames(sed_contam[,c((dim(sed_contam)[2]-length(PCB)+1):dim(sed_contam)[2])]),
  "normalised_sum_ng.gdw", sep="_")


# Add normalised concentrations for PFASs
sed_contam = cbind(sed_contam, sed_contam[,PFAS]/sed_contam$sommePFAS_ng_gdw)
colnames(sed_contam)[c((dim(sed_contam)[2]-length(PFAS)+1):dim(sed_contam)[2])] =  paste(
  colnames(sed_contam[,c((dim(sed_contam)[2]-length(PFAS)+1):dim(sed_contam)[2])]),
  "normalised_sum_ng.gdw", sep="_")

# Add normalised concentrations for HBCDD
sed_contam = cbind(sed_contam, sed_contam[,HBCDD]/sed_contam$sommeHBCDD_ng_gdw)
colnames(sed_contam)[c((dim(sed_contam)[2]-length(HBCDD)+1):dim(sed_contam)[2])] =  paste(
  colnames(sed_contam[,c((dim(sed_contam)[2]-length(HBCDD)+1):dim(sed_contam)[2])]),
  "normalised_sum_ng.gdw", sep="_")


# --------------------------------------------------------------
# Subset par saison (tableau complet)

sed_contam_juin = sed_contam[which(sed_contam$season=="Printemps"),]
sed_contam_oct = sed_contam[which(sed_contam$season=="Automne"),]

