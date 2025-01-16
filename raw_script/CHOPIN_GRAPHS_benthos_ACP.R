####################################
##         PROJET CHOPIN
##        RETOUR RAPPORT
## 
##       ANALYSES - GRAPH
##           BENTHOS
##             ACP
##      
#################################### 


rm(list=ls()) # clear workspace


library(xlsx) # Chargement des packages
source('Script/CHOPIN_DATA_FORMATTING_MAIN.R')  # Chargement des data
source('Script/CHOPIN_ANALYSES_stats_occurences.R')

#install.packages(c("FactoMineR", "factoextra"))
library("FactoMineR")
library("factoextra")
library("missMDA")

# graphiques
library(gridExtra) # graphiques
library(RColorBrewer)




######################################################################################
# TOUS contaminants sélectionnés (les valeurs manquantes = 0)

ACP_benthos = benthos_contam[,c("species","lip_PS_percent","zone","grp","season","alim",
                                paste(PCB_selected_benthos,"_ng_g.1ps",sep=""),
                                PFAS_selected_benthos,
                                paste(HBCDD,"_ng_g.1ps",sep=""))]

# valeurs manquantes = 0
for(c in 7:30){
  ACP_benthos[which(is.na(ACP_benthos[,c])),c] = 0
}


# suppression des echantillons non-dosed pour toutes les familles
ACP_benthos = ACP_benthos[-c(19,38,39,43,44,48, # non-dosed PCB & HBCDD
                             24,25,27,29,50,   # non-dosed PFAS
                             5,6,7,10,14, 26:29,32,35,36,45,46,50),]  # non-dosed HBCDD

dim(ACP_benthos)
# 27 echantillons, 30 variables

ACP_benthos$species = factor(x = ACP_benthos$species,
                             levels = levels(ACP_benthos$species),
                             labels = c("Abra(B)","Cerasto.(B)","Corbula(B)","Coroph.(C)",
                                        "Crangon(C)","Donax(B)","Ensis(B)",
                                        "Hedist.(P)","Lagis(P)", "Lanice(B)", "Lime.(B)",
                                        "Nephtis(P)","Nucula(B)","Owenia(P)", "Scrobi.(B)","Spisula(B)"))

colnames(ACP_benthos) = c("esp.","%lip","zone","grp","saison","alim.",PCB_selected_benthos, PFAS_selected_benthos,HBCDD_selected_benthos_abund)
colnames(ACP_benthos)[c(26,27)] = c("L-PFOS","6:2-FTSA")


plot(as.factor(ACP_benthos$grp), ACP_benthos$`L-PFOS`)
par(mar=c(8,2,2,2))
for(i in 18:30){
  plot(as.factor(ACP_benthos$esp.), ACP_benthos[,i], las=2, main = colnames(ACP_benthos)[i])
}


#-------------------------------------

res.pca = PCA(ACP_benthos, quanti.sup = 2, quali.sup = c(1,3,4,5,6))
summary(res.pca)


fviz_screeplot(res.pca)

fviz_pca_var(res.pca, col.var = "contrib", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel=T
)
fviz_pca_var(res.pca, col.var = "contrib", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel=T, axes = c(2,3)
)

#-------------------------------------
# contrib var axes

png(filename = "Output/BENTHOS/BENTHOS_acp_contrib_axes.png",
    width = 20,height = 10,units = "cm",res = 280)
# Contribution ? la premi?re dimension
p1=fviz_contrib (res.pca, "var", axes = 1)
# Contribution ? la deuxi?me dimension
p2=fviz_contrib (res.pca, "var", axes = 2)
grid.arrange(p1,p2,nrow=2)
dev.off()




#-------------------------------------
# var

png(filename = "Output/BENTHOS/BENTHOS_acp_var.png",
    width = 28,height = 15,units = "cm",res = 280)
p1=fviz_pca_var(res.pca, col.var = "contrib", 
                gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),repel=T)
p2= fviz_mfa_ind(res.pca, 
                 habillage = "esp.", # color by groups 
                 addEllipses = TRUE, ellipse.type = "convex", 
                 repel = TRUE # Avoid text overlapping
) 
grid.arrange(p1,p2,ncol=2)
dev.off()


#-------------------------------------
# ind

png(filename = "Output/BENTHOS/BENTHOS_acp_ind.png",
    width = 20,height = 15,units = "cm",res = 280)

# zone
p1=fviz_pca_ind(res.pca, 
                geom.ind = "point", # Montre les points seulement (mais pas le "text")
                col.ind=ACP_benthos$zone,
                palette = brewer.pal(n = length(levels(ACP_benthos$zone)), name = "Set1"),
                addEllipses = TRUE, ellipse.type = "convex", 
                repel = TRUE, # Avoid text overlapping
                legend.title="Zone",
                mean.point = FALSE
) 


#  saison
p2=fviz_pca_ind(res.pca, 
                geom.ind = "point",
                col.ind=ACP_benthos$saison, # color by groups 
                palette = brewer.pal(n = 2, name = "Set1"),
                addEllipses = TRUE, ellipse.type = "convex", 
                repel = TRUE, # Avoid text overlapping
                legend.title="Saison",
                mean.point = FALSE
) 

#  taxon
p3=fviz_pca_ind(res.pca, 
                geom.ind = "point",
                col.ind=ACP_benthos$grp, # color by groups 
                palette = brewer.pal(n = 3, name = "Set1"),
                addEllipses = TRUE, ellipse.type = "convex", 
                repel = TRUE, # Avoid text overlapping
                legend.title="Grp. Taxo.",
                mean.point = FALSE
) 


#  alim
p4=fviz_pca_ind(res.pca, 
                geom.ind = "point",
                col.ind=ACP_benthos$alim, # color by groups 
                palette = brewer.pal(n = 5, name = "Set1"),
                addEllipses = TRUE, ellipse.type = "convex", 
                repel = TRUE, # Avoid text overlapping
                legend.title="Alimentation",
                mean.point = FALSE
) 
grid.arrange(p1,p2,p3,p4,ncol=2, nrow=2)
dev.off()




















# 
# 
# ####################################################################
# # fig 8
# 
# ACP_benthos = benthos_contam[,c("species","lip_PS_percent","zone",paste(c("CB28","CB52","CB101","CB149","CB153","CB180"),"_ng_g.1ps",sep=""),
#                                 c("FOSA","L.PFOS","Br.PFOS","X6.2.FTSA","PFDA","PFUnDA","PFDoA","PFOA"),
#                                 paste(HBCDD_selected_benthos_abund,"_ng_g.1ps",sep=""))]
# 
# colnames(ACP_benthos) = c("species","lip","zone","CB28","CB52","CB101","CB149","CB153","CB180",
#                           "FOSA","L.PFOS","Br.PFOS","X6.2.FTSA","PFDA","PFUnDA","PFDoA","PFOA",
#                           HBCDD_selected_benthos_abund)
# 
# ACP_benthos$species = factor(x = ACP_benthos$species,
#                              levels = levels(ACP_benthos$species),
#                              labels = c("Abra(B)","Cerasto.(B)","Corbula(B)","Coroph.(C)",
#                                         "Crangon(C)","Donax(B)","Ensis(B)",
#                                         "Hedist.(P)","Lagis(P)", "Lanice(B)", "Lime.(B)",
#                                         "Nephtis(P)","Nucula(B)","Owenia(P)", "Scrobi.(B)","Spisula(B)"))
# 
# res.pca = PCA(ACP_benthos, quanti.sup = 2, quali.sup = c(1,3))
# 
# summary(res.pca)
# plot(res.pca)
# fviz_contrib (res.pca, "var", axes = 1)
# fviz_contrib (res.pca, "var", axes = 2)
# 
# fviz_pca_var(res.pca, col.var = "contrib", 
#              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
#              repel=T
# )
# 
# 
# fviz_mfa_ind(res.pca, 
#              habillage = "species", # color by groups 
#              addEllipses = TRUE, ellipse.type = "convex", 
#              repel = TRUE # Avoid text overlapping
# ) 
# fviz_ellipses(res.pca, "species",
#               ellipse.type = "convex",repel = TRUE)
# fviz_ellipses(res.pca, "zone",
#               ellipse.type = "convex",repel = TRUE)
# 
# 
# 
# 
# ######################################################################################
# # TOUS contaminants avec missMDA pour les valeurs manquantes
# 
# res.comp = imputePCA(ACP_benthos, quanti.sup = 2, quali.sup = 1, ncp=5)
# res.pca = PCA(res.comp$completeObs, quanti.sup = 2, quali.sup = 1)
# 
# #######################################################################################
# 
# ACP_benthos = benthos_contam[,c("species",paste(PCB_selected_benthos,"_ng_g.1pl",sep=""),
#                                PFAS_selected_benthos,
#                                paste(HBCDD_selected_benthos_abund,"_ng_g.1pl",sep=""))]
# 
# colnames(ACP_benthos) = c("species",PCB_selected_benthos, PFAS_selected_benthos,HBCDD_selected_benthos_abund)
# 
# ACP_benthos$species = factor(x = ACP_benthos$species,
#                             levels = levels(ACP_benthos$species),
#                               labels = c("Abra(B)","Cerasto.(B)","Corbula(B)","Coroph.(C)",
#                                          "Crangon(C)","Donax(B)","Ensis(B)",
#                                          "Hedist.(P)","Lagis(P)", "Lanice(B)", "Lime.(B)",
#                                          "Nephtis(P)","Nucula(B)","Owenia(P)", "Scrobi.(B)","Spisula(B)"))
# res.comp = imputePCA(ACP_benthos, quali.sup = 1, ncp=5)
# res.pca = PCA(res.comp$completeObs, quali.sup = 1)
# summary(res.pca)
# plot(res.pca)
# 
# fviz_contrib (res.pca, "var", axes = 1)
# fviz_contrib (res.pca, "var", axes = 2)
# 
# fviz_pca_var(res.pca, col.var = "contrib", 
#               gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
#               repel=T
# )
# 
# 
# fviz_mfa_ind(res.pca, 
#              habillage = "species", # color by groups 
#              addEllipses = TRUE, ellipse.type = "convex", 
#              repel = TRUE # Avoid text overlapping
# ) 
# fviz_ellipses(res.pca, "species",
#               ellipse.type = "convex",repel = TRUE)
# 
# 
# ########################################################
# ACP_benthos = benthos_contam[,c("species","zone",paste(PCB_selected_benthos,"_ng_g.1pl",sep=""),
#                                 PFAS_selected_benthos,
#                                 paste(HBCDD_selected_benthos_abund,"_ng_g.1pl",sep=""))]
# 
# colnames(ACP_benthos) = c("species","zone", PCB_selected_benthos, PFAS_selected_benthos,HBCDD_selected_benthos_abund)
# ACP_benthos$species = factor(x = ACP_benthos$species,
#                             levels = levels(ACP_benthos$species),
#                               labels = c("Abra(B)","Cerasto.(B)","Corbula(B)","Coroph.(C)",
#                                          "Crangon(C)","Donax(B)","Ensis(B)",
#                                          "Hedist.(P)","Lagis(P)", "Lanice(B)", "Lime.(B)",
#                                          "Nephtis(P)","Nucula(B)","Owenia(P)", "Scrobi.(B)","Spisula(B)"))
# 
# res.comp = imputeFAMD(ACP_benthos, ncp=5)
# res.famd = FAMD(res.comp$completeObs)
# 
# fviz_famd_var (res.famd, repel = TRUE)
# fviz_contrib (res.famd, "var", axes = 1)
# fviz_contrib (res.famd, "var", axes = 2)
# 
# fviz_famd_var(res.famd, repel = TRUE,
#               col.var = "contrib", 
#               gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))
# 
# 
# # contribution des variables quantitatives aux axes
# fviz_famd_var(res.famd, "quanti.var", repel = TRUE,
#               col.var = "contrib", 
#               gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))
# 
# # qualité de représentation de la variable sur ces axes
# fviz_famd_var(res.famd, "quanti.var", repel = TRUE,
#               col.var = "cos2", 
#               gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))
# 
# # contribution des variables qualitatives aux axes
# fviz_famd_var(res.famd, "quali.var", col.var = "contrib", 
#               gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
#               repel=T
# )
# 
# fviz_famd_ind(res.famd, col.ind = "cos2", 
#               gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
#               repel = TRUE)
# 
# 
# fviz_mfa_ind(res.famd, 
#              habillage = "zone", # color by groups 
#              addEllipses = TRUE, ellipse.type = "convex", 
#              repel = TRUE # Avoid text overlapping
# ) 
# 
# fviz_mfa_ind(res.famd, 
#              habillage = c("zone","species"), # color by groups 
#              addEllipses = TRUE, ellipse.type = "convex", 
#              repel = TRUE # Avoid text overlapping
# ) 
# 
# fviz_ellipses(res.famd, "zone",
#               ellipse.type = "convex",repel = TRUE)
# 
# fviz_ellipses(res.famd, "species",
#               ellipse.type = "convex",repel = TRUE)
