####################################
##         PROJET CHOPIN
## 
##         CARTO
##      
####################################


# clear workspace
rm(list=ls())

#-------------------------------------------------------------
## MISE EN FORME & TRANSFORMATIONS DES DONNEES BRUTES


# chargement des données à partir de la base excel
# transformations d'unités (concentration pf, ps, Corg, normalisations par la somme par famille)



library(xlsx) # Chargement des packages
source('Script/CHOPIN_DATA_FORMATTING_MAIN.R')
# source('Script/CHOPIN_ANALYSES_stats_occurences.R')



library(tidyverse) # contient ggplot2
library(gg.gap)
library(cowplot)
library(viridis)
library(gridExtra)
library(ggmap)
library(devtools)
# 
# source('Script/CHOPIN_GRAPHS_MAIN.R')


cartesediment=read.csv("DATA/cartesediment.csv", header=TRUE, sep=";", dec=",")

bivalves_contam = benthos_contam[benthos_contam$grp=="Bivalves",]
crustaces_contam = benthos_contam[benthos_contam$grp=="Crustaces",]
polychetes_contam = benthos_contam[benthos_contam$grp=="Polychetes",]

bivalves_contam$zone = as.factor(bivalves_contam$zone)
crustaces_contam$zone[which(crustaces_contam$zone=="Chenal")] = "Fosse Nord"
crustaces_contam$zone = as.factor(crustaces_contam$zone)
polychetes_contam$zone = as.factor(polychetes_contam$zone)

benthos_carto = data.frame("Groupe"=c(rep.int("Bivalves", times = 3),
                                   rep.int("Crustaces", times = 3),
                                   rep.int("Polychetes", times = 3)),
                           "zone"=rep.int(x = levels(bivalves_contam$zone), times = 3),
                           "lat"=rep.int(c(49.436, 49.445, 49.423),times = 3),
                           "lon"=c( c(0.05,0.2,0.12), c(0.06,0.21,0.13),c(0.07,0.22,0.14)),
                           "PCB" = c(tapply(bivalves_contam$sommePCB_ng_gdw, INDEX = bivalves_contam$zone, FUN = median, na.rm=T),
                                     tapply(crustaces_contam$sommePCB_ng_gdw, INDEX = crustaces_contam$zone, FUN = median, na.rm=T),
                                     tapply(polychetes_contam$sommePCB_ng_gdw, INDEX = polychetes_contam$zone, FUN = median, na.rm=T)),
                           "PFAS" = c(tapply(bivalves_contam$sommePFAS_ng_gdw, INDEX = bivalves_contam$zone, FUN = median, na.rm=T),
                                      tapply(crustaces_contam$sommePFAS_ng_gdw, INDEX = crustaces_contam$zone, FUN = median, na.rm=T),
                                      tapply(polychetes_contam$sommePFAS_ng_gdw, INDEX = polychetes_contam$zone, FUN = median, na.rm=T)),
                           "HBCDD" = c(tapply(bivalves_contam$sommeHBCDD_ng_gdw, INDEX = bivalves_contam$zone, FUN = median, na.rm=T),
                                       tapply(crustaces_contam$sommeHBCDD_ng_gdw, INDEX = crustaces_contam$zone, FUN = median, na.rm=T),
                                       tapply(polychetes_contam$sommeHBCDD_ng_gdw, INDEX = polychetes_contam$zone, FUN = median, na.rm=T)))

solesG0_contam$zone[which(solesG0_contam$zone=="Chenal")]  = "Fosse Nord"
solesG0_contam$zone = factor(x = solesG0_contam$zone, levels = levels(solesG0_contam$zone)[-1] )
solesG1_contam$zone[which(solesG1_contam$zone=="Chenal")]  = "Fosse Nord"
solesG1_contam$zone = factor(x = solesG1_contam$zone, levels = levels(solesG1_contam$zone)[-1] )
solesG2_contam$zone[which(solesG2_contam$zone=="Chenal")]  = "Fosse Nord"
solesG2_contam$zone = factor(x = solesG2_contam$zone, levels = levels(solesG2_contam$zone)[-1] )

soles_carto = data.frame("Groupe"=c(rep.int("G0", times = 3),
                                 rep.int("G1", times = 3),
                                 rep.int("G2", times = 3)),
                         "zone"=rep.int(x = levels(soles_contam$zone)[-1], times = 3),
                         "lat"=rep.int(c(49.430, 49.439, 49.418), times = 3),
                         "lon"=c( c(0.05,0.2,0.12), c(0.06,0.21,0.13),c(0.07,0.22,0.14)),
                         "PCB" = c(tapply(solesG0_contam$sommePCB_ng_gdw, INDEX = solesG0_contam$zone, FUN = median, na.rm=T),
                                   tapply(solesG1_contam$sommePCB_ng_gdw, INDEX = solesG1_contam$zone, FUN = median, na.rm=T),
                                   tapply(solesG2_contam$sommePCB_ng_gdw, INDEX = solesG2_contam$zone, FUN = median, na.rm=T)),
                         "PFAS" = c(tapply(solesG0_contam$sommePFAS_ng_gdw, INDEX = solesG0_contam$zone, FUN = median, na.rm=T),
                                    tapply(solesG1_contam$sommePFAS_ng_gdw, INDEX = solesG1_contam$zone, FUN = median, na.rm=T),
                                    tapply(solesG2_contam$sommePFAS_ng_gdw, INDEX = solesG2_contam$zone, FUN = median, na.rm=T)),
                         "HBCDD" = c(tapply(solesG0_contam$sommeHBCDD_ng_gdw, INDEX = solesG0_contam$zone, FUN = median, na.rm=T),
                                     tapply(solesG1_contam$sommeHBCDD_ng_gdw, INDEX = solesG1_contam$zone, FUN = median, na.rm=T),
                                     tapply(solesG2_contam$sommeHBCDD_ng_gdw, INDEX = solesG2_contam$zone, FUN = median, na.rm=T)))

biote_carto = rbind(benthos_carto,soles_carto)

biote_carto$Groupe = factor(biote_carto$Groupe, levels = c("Bivalves","Crustaces","Polychetes","G0","G1","G2"),
                            labels = c("Bivalves","Crustacés","Polychètes","Soles G0","Soles G1","Soles G2"))

############################################################################

# CARTES CONTAMINATION
# sÃ©lection des limites de la carte ; coordonnÃ©es en degrÃ©s dÃ©cimaux (Ã  convertir pour les donnÃ©es)
seine<-c(left=0, right=0.28, bottom=49.395, top=49.49)
# importation de la carte
map<-get_map(seine, maptype="toner-lite")

png("Output/carto_biote_legend.jpeg",width = 22, height = 10, units = "cm", res=360)
ggplot(aes(x=Groupe, color=Groupe, fill=Groupe), 
       data=biote_carto, alpha=.8)+
  scale_colour_hue(c=80, l=50)+
  theme(legend.title = element_text(colour="black", size = 16, face = "bold"),
        legend.text = element_text(colour="black", size = 14), legend.key.size = unit(1,"cm"))+
  #guides(color=guide_legend(ncol=2))+
  geom_bar()
dev.off()

png("Output/carto_biote_PCB.jpeg",width = 22, height = 10, units = "cm", res=360)
par(mar=c(0,2,0,2))
ggmap(map) + 
  geom_point(aes(x=lon, y=lat, color=Groupe, size=PCB), 
             data=biote_carto, alpha=.8) +
  scale_colour_hue(c=80, l=50)+
  geom_segment(aes(y=49.4, yend=49.455,x=0.08, xend=0.125))+
  geom_segment(aes(y=49.435, yend=49.425,x=0.11, xend=0.22))+
  labs(x="Longitude", y="Latitude")+ 
  theme(legend.title = element_text(colour="black", size = 16, face = "bold"),
        legend.text = element_text(colour="black", size = 14))
dev.off()


# Concentration en PFAS
png("Output/carto_biote_PFAS.jpeg",width = 22, height = 10, units = "cm", res=360)
par(mar=c(0,2,0,2))
ggmap(map) + 
  geom_point(aes(x=lon, y=lat, color=Groupe, size=PFAS), 
             data=biote_carto, alpha=.8) +
  scale_colour_hue(c=80, l=50)+
  geom_segment(aes(y=49.4, yend=49.455,x=0.08, xend=0.125))+
  geom_segment(aes(y=49.435, yend=49.425,x=0.11, xend=0.22))+
  labs(x="Longitude", y="Latitude")+
  theme(legend.title = element_text(colour="black", size = 16, face = "bold"),
        legend.text = element_text(colour="black", size = 14))
dev.off()

# Concentration en HBCDD
png("Output/carto_biote_HBCDD.jpeg",width = 22, height = 10, units = "cm", res=360)
par(mar=c(0,2,0,2))
ggmap(map) + 
  geom_point(aes(x=lon, y=lat, color=Groupe, size=HBCDD), 
             data=biote_carto, alpha=.8) +
  scale_colour_hue(c=80, l=50)+
  geom_segment(aes(y=49.4, yend=49.455,x=0.08, xend=0.125))+
  geom_segment(aes(y=49.435, yend=49.425,x=0.11, xend=0.22))+
  labs(x="Longitude", y="Latitude")+
  theme(legend.title = element_text(colour="black", size = 16, face = "bold"),
        legend.text = element_text(colour="black", size = 14))
dev.off()

