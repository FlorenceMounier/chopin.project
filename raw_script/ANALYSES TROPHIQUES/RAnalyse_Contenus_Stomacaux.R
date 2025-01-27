####################################
##      RAPPORT FINAL CHOPIN
##
##    CONTENUS STOMACAUX
##    B. Chouquet
####################################


# clear workspace
rm(list=ls())

library("viridis")
# load packages
library(tidyverse) # contient ggplot2
library(gridExtra)
library(ggpubr)
library("RColorBrewer")
library(xlsx)
library(readxl)
library(data.table)
library(cowplot)
library(gg.gap)
library(devtools)
library(scales)


# save working directory
wd <- getwd()



#####################################################################################################################
###    FONCTIONS
#####################################################################################################################


distri_diet = function(data_stomac){

  fish_samp = names(which(table(data_stomac$N.poissons)!=0))
  preys = levels(data_stomac$ScientificName_accepted)

  diets = matrix(NA, nrow = length(preys), ncol=length(fish_samp))
  rownames(diets) = preys

  for(f in 1:length(fish_samp)){

    abundance_tot = sum( data_stomac[data_stomac$N.poissons==fish_samp[f],]$Nbr.total.tractus, na.rm=T)

    for(p in data_stomac[data_stomac$N.poissons==fish_samp[f],]$ScientificName_accepted){
      diets[which(rownames(diets)==p),f] = sum(data_stomac[data_stomac$N.poissons==fish_samp[f]&
                                                             data_stomac$ScientificName_accepted==p,]$Nbr.total.tractus,na.rm=T) / abundance_tot
    }}

  diets

}


get_res = function(main, grp, min_occur, min_abond){


  res_distri_diet = distri_diet(data_stomac = grp)

  # Taux d'occurence: #soles dans lesquelles la proie a ?t? retrouv?e
  occ = c()
  for(l in 1:dim(res_distri_diet)[1]){
    occ=c(occ, length(na.omit(res_distri_diet[l,])))
  }
  occ = occ/dim(res_distri_diet)[2]

  # M?diane des #proie/#total tractus dans les estomacs des soles dans lequelles la proie a ?t? retrouv?e
  min_abun = med_abun = max_abun = c()
  for(l in 1:dim(res_distri_diet)[1]){
    min_abun=c(min_abun, ifelse(occ[l]==0, yes = 0, no = min(na.omit(res_distri_diet[l,]))))
    med_abun=c(med_abun, ifelse(occ[l]==0, yes = 0, no = median(na.omit(res_distri_diet[l,]))))
    max_abun=c(max_abun, ifelse(occ[l]==0, yes = 0, no = max(na.omit(res_distri_diet[l,]))))
  }

  par(mar=c(1,4,3,2))

  pie(main = paste("Normalised %abundance for ", main,"\n for species with %abundance >",min_abond*100,"% and %occurance >",min_occur*100,"%", sep=""),
      med_abun[-which(med_abun<min_abond|occ<min_occur)]/
        sum(med_abun[-which(med_abun<min_abond|occ<min_occur)]),
      labels = rownames(res_distri_diet)[-which(med_abun<min_abond|occ<min_occur)]
      )

  res_pie = round((med_abun[-which(med_abun<min_abond|occ<min_occur)]/
          sum(med_abun[-which(med_abun<min_abond|occ<min_occur)]))*100,digits = 3)
  res_pie = data.frame("taxon" = table_species$taxon[-which(med_abun<min_abond|occ<min_occur)],
                       "species" = rownames(res_distri_diet)[-which(med_abun<min_abond|occ<min_occur)],
                       "tx.abun" = res_pie)

  res = cbind("%occ"=round(occ*100,digits = 3),
              "min %abun"=round(min_abun*100,digits = 3),
              "med %abun"=round(med_abun*100,digits = 3),
              "max %abun"=round(max_abun*100,digits = 3))

  rownames(res) = rownames(res_distri_diet)

res_pie$species = factor(res_pie$species, levels = levels(res_pie$species)[order(res_pie$taxon)],
                         labels = levels(res_pie$species)[order(res_pie$taxon)])

  list("res"=res,
       "med %abun"=data.frame(res_pie))
}


#-----------------------------------------------------------------------------------------------

res_all_soles = get_res(main = "ALL SOLES", grp=stomac_soles, min_abond =0.1, min_occur = 0.1)

res_G0 = get_res(main = "Sole G0", grp = stomac_soles[which(stomac_soles$esp?ce.stade=="Sole G0"),], min_abond =0.1, min_occur = 0.1)
res_G1 = get_res(main = "Sole G1", grp = stomac_soles[which(stomac_soles$esp?ce.stade=="Sole G1"),], min_abond =0.1, min_occur = 0.1)
res_G2 = get_res(main = "Sole G2", grp = stomac_soles[which(stomac_soles$esp?ce.stade=="Sole G2"),], min_abond =0.1, min_occur = 0.1)

res_G0_juin = get_res(main = "Sole G0 printemps", grp = stomac_soles[which(stomac_soles$esp?ce.stade=="Sole G0"&stomac_soles$Campagne=="Print-17"),], min_abond =0.1, min_occur = 0.1)
res_G0_oct = get_res(main = "Sole G0 automne", grp = stomac_soles[which(stomac_soles$esp?ce.stade=="Sole G0"&stomac_soles$Campagne=="Aut-17"),], min_abond =0.1, min_occur = 0.1)

res_G1_juin = get_res(main = "Sole G1 printemps", grp = stomac_soles[which(stomac_soles$esp?ce.stade=="Sole G1"&stomac_soles$Campagne=="Print-17"),], min_abond =0.1, min_occur = 0.1)
res_G1_oct = get_res(main = "Sole G1 automne", grp = stomac_soles[which(stomac_soles$esp?ce.stade=="Sole G1"&stomac_soles$Campagne=="Aut-17"),], min_abond =0.1, min_occur = 0.1)


res_che = get_res(main = "Chenal", grp=stomac_soles[which(stomac_soles$secteur=="Chenal"),], min_abond =0.1, min_occur = 0.1)
res_G0_che = get_res(main = "G0 Chenal", grp=stomac_soles[which(stomac_soles$secteur=="Chenal"&
                                              stomac_soles$esp?ce.stade=="Sole G0"),], min_abond =0.1, min_occur = 0.1)
res_G0_che_juin = get_res(main = "G0 Chenal printemps", grp = stomac_soles[which(stomac_soles$esp?ce.stade=="Sole G0"&
                                                                                   stomac_soles$esp?ce.stade=="Sole G0"&stomac_soles$Campagne=="Print-17"),], min_abond =0.1, min_occur = 0.1)
res_G0_che_oct = get_res(main = "G0 Chenal automne", grp = stomac_soles[which(stomac_soles$esp?ce.stade=="Sole G0"&
                                                                                stomac_soles$esp?ce.stade=="Sole G0"&stomac_soles$Campagne=="Aut-17"),], min_abond =0.1, min_occur = 0.1)


res_emb = get_res(main = "Embouchure", grp=stomac_soles[which(stomac_soles$secteur=="Embouchure"),], min_abond =0.1, min_occur = 0.1)
res_G0_emb = get_res(main = "G0 Embouchure", grp=stomac_soles[which(stomac_soles$secteur=="Embouchure"&
                                          stomac_soles$esp?ce.stade=="Sole G0"),], min_abond =0.1, min_occur = 0.1)
res_G0_emb_juin = get_res(main = "G0 Embouchure printemps", grp = stomac_soles[which(stomac_soles$secteur=="Embouchure"&stomac_soles$esp?ce.stade=="Sole G0"&stomac_soles$Campagne=="Print-17"),], min_abond =0.1, min_occur = 0.1)
res_G0_emb_oct = get_res(main = "G0 Embouchure automne", grp = stomac_soles[which(stomac_soles$secteur=="Embouchure"&stomac_soles$esp?ce.stade=="Sole G0"&stomac_soles$Campagne=="Aut-17"),], min_abond =0.1, min_occur = 0.1)


res_FS = get_res(main = "Fosse Sud", grp=stomac_soles[which(stomac_soles$secteur=="Fosse Sud"),], min_abond =0.1, min_occur = 0.1)
res_G0_FS = get_res(main = "G0 Fosse Sud", grp=stomac_soles[which(stomac_soles$secteur=="Fosse Sud"&
                                         stomac_soles$esp?ce.stade=="Sole G0"),], min_abond =0.1, min_occur = 0.1)
res_G0_FS_juin = get_res(main = "G0 Fosse Sud printemps", grp = stomac_soles[which(stomac_soles$secteur=="Fosse Sud"&stomac_soles$esp?ce.stade=="Sole G0"&stomac_soles$Campagne=="Print-17"),], min_abond =0.1, min_occur = 0.1)
res_G0_FS_oct = get_res(main = "G0 Fosse Sud automne", grp = stomac_soles[which(stomac_soles$secteur=="Fosse Sud"&stomac_soles$esp?ce.stade=="Sole G0"&stomac_soles$Campagne=="Aut-17"),], min_abond =0.1, min_occur = 0.1)


res_FN = get_res(main = "Fosse Nord", grp=stomac_soles[which(stomac_soles$secteur=="Fosse Nord"),], min_abond =0.1, min_occur = 0.1)
res_G0_FN = get_res(main = "G0 Fosse Nord", grp=stomac_soles[which(stomac_soles$secteur=="Fosse Nord"&
                                         stomac_soles$esp?ce.stade=="Sole G0"),], min_abond =0.1, min_occur = 0.1)
res_G0_FN_juin = get_res(main = "G0 Fosse Nord printemps", grp = stomac_soles[which(stomac_soles$secteur=="Fosse Nord"&stomac_soles$esp?ce.stade=="Sole G0"&stomac_soles$Campagne=="Print-17"),], min_abond =0.1, min_occur = 0.1)
res_G0_FN_oct = get_res(main = "G0 Fosse Nord automne", grp = stomac_soles[which(stomac_soles$secteur=="Fosse Nord"&stomac_soles$esp?ce.stade=="Sole G0"&stomac_soles$Campagne=="Aut-17"),], min_abond =0.1, min_occur = 0.1)


ALL_res = list(res_G0_che_juin$`med %abun`, res_G0_che_oct$`med %abun`,
               res_G0_emb_juin$`med %abun`, res_G0_emb_oct$`med %abun`,
               res_G0_FS_juin$`med %abun`, res_G0_FS_oct$`med %abun`,
               res_G0_FN_juin$`med %abun`, res_G0_FN_oct$`med %abun`,
               res_G1_juin$`med %abun`, res_G1_oct$`med %abun`,
               res_G2)


#---------------------------------------------------------------------------------------
# 1 color for each species with similar colors per taxon

list_species = c()
for(t in 1:length(ALL_res)){
  list_species = c(list_species, as.character(ALL_res[[t]]$species))
}
especes = levels(as.factor(list_species))

list_taxon = c()
for(s in 1:length(especes)){
  list_taxon = c(list_taxon, as.character(table_species$taxon[which(table_species$species==especes[s])]))
}
list_taxon = data.frame("species"=especes, "taxon" = list_taxon)
summary(list_taxon)

col_annelid1 = brewer.pal(n = 9, name = 'OrRd')[-c(1:3)]
col_annelid2 = brewer.pal(n = 9, name = 'YlOrRd')[c(1,3,4,6,7,8,9)]
col_arthro1 = brewer.pal(n = 9, name = 'Greens')[-1]
col_arthro2 = brewer.pal(n = 9, name = 'Dark2')[c(1,5)]
col_mollusc1 = brewer.pal(n = 9, name = 'Purples')[-c(1,2,4,6)]
col_mollusc2 = brewer.pal(n = 9, name = 'Blues')[c(4,6,8,9)]

list_taxon = list_taxon[order(list_taxon$taxon),]
list_taxon$col_species = c(col_annelid1,col_annelid2,
                           col_arthro1,col_arthro2, "grey", col_mollusc1,col_mollusc2)

#-------------------------------------------------------
piechart = function(res_pie){

  res_pie$col=rep.int(NA,times = length(res_pie$species))
  for(i in 1:dim(res_pie)[1]){
    res_pie$col[i] = list_taxon$col_species[which(list_taxon$species==as.character(res_pie$species[i]))]

  }

  res_pie <- res_pie %>%
    arrange(desc(species)) %>%
    mutate(prop = tx.abun / sum(res_pie$tx.abun) *100) %>%
    mutate(ypos = cumsum(prop)- 0.5*prop )

  bp<- ggplot(res_pie, aes(x="", y=tx.abun, fill=species))+
    geom_bar(width = 1, stat = "identity", color="white")
  pie <- bp + coord_polar("y", start=0)
  blank_theme <- theme_minimal()+
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.border = element_blank(),
      panel.grid=element_blank(),
      axis.ticks = element_blank(),
      plot.title=element_text(size=12, face="bold")
    )
  pie +  scale_fill_manual(values=res_pie$col[order(res_pie$species)]) +
    blank_theme+theme(axis.text.x=element_blank())+
    labs(fill="Esp?ces")+
    geom_text(aes(y = ypos,
                  label = percent(res_pie$tx.abun/100, accuracy = 1)),
                  size=4)

}



b1=piechart(res_G0_emb_juin$`med %abun`)+ ggtitle("G0 embouchure printemps")
b3=piechart(res_G0_che_juin$`med %abun`)+ ggtitle("G0 chenal printemps")
b5=piechart(res_G0_FN_juin$`med %abun`)+ ggtitle("G0 Fosse Nord printemps")
b7=piechart(res_G0_FS_juin$`med %abun`)+ ggtitle("G0 Fosse Sud printemps")

b2=piechart(res_G0_emb_oct$`med %abun`)+ ggtitle("G0 embouchure automne")
b4=piechart(res_G0_che_oct$`med %abun`)+ ggtitle("G0 chenal automne")
b6=piechart(res_G0_FN_oct$`med %abun`)+ ggtitle("G0 Fosse Nord automne")
b8=piechart(res_G0_FS_oct$`med %abun`)+ ggtitle("G0 Fosse Sud automne")

b9=piechart(res_G1_juin$`med %abun`)+ ggtitle("G1 printemps")
b10=piechart(res_G1_oct$`med %abun`)+ ggtitle("G1 automne")

b11=piechart(res_pie=res_G2$`med %abun`)+ ggtitle("G2 automne")



png("C:/Users/florence.mounier/Work Folders/Documents/Mes documents/CDD IR CHOPIN/2020.06 - Rapport FINAL/CONTENUS_STOMACAUX/Contenus_stomacaux_classes_age.jpeg",
    width = 25, height = 50, units = "cm",res=720)
grid.arrange(b1,b2,b3,b4,b5,b6,b7,b8,b9,b10,b11,ncol=2,nrow=6,
             layout_matrix = matrix(c(seq(1,10,by = 1),NA,11),ncol = 2, nrow=6,byrow = T))
dev.off()

p1=piechart(res_G0_che$`med %abun`) + ggtitle("Soles G0 Chenal")
p2=piechart(res_G0_emb$`med %abun`) + ggtitle("Soles G0 Embouchure")
p3=piechart(res_G0_FS$`med %abun`) + ggtitle("Soles G0 Fosse Sud")
p4=piechart(res_G0_FN$`med %abun`) + ggtitle("Soles G0 Fosse Nord")
b2=piechart(res_G1$`med %abun`)+ ggtitle("Tous les G1")
b3=piechart(res_G2$`med %abun`)+ ggtitle("Tous les G2")

png("C:/Users/florence.mounier/Work Folders/Documents/Mes documents/CDD IR CHOPIN/2020.06 - Rapport FINAL/CONTENUS_STOMACAUX/Contenus_stomacaux.jpeg",width = 30, height = 25, units = "cm",res=720)
grid.arrange(p1,p2,p3,p4,b2,b3,ncol=2,nrow=3,)
dev.off()


