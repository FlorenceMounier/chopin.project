####################################
##         PROJET CHOPIN
##        RETOUR RAPPORT
## 
##       ANALYSES - GRAPH
##           BENTHOS
##          ACP PFAS
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

ACP_benthos = benthos_contam[,c("species","zone","grp","season","alim",
                                PFAS_selected_benthos)]

# suppression des echantillons non-dosed pour toutes les familles
ACP_benthos = ACP_benthos[-c(19,38,39,43,44,48, # non-dosed PCB & HBCDD
                             24,25,27,29,50,   # non-dosed PFAS
                             5,6,7,10,14, 26:29,32,35,36,45,46,50),]  # non-dosed HBCDD


dim(ACP_benthos)
# 27 echantillons, 15 variables

# valeurs manquantes = 0
for(c in 6:15){
  ACP_benthos[which(is.na(ACP_benthos[,c])),c] = 0
}


ACP_benthos$species = factor(x = ACP_benthos$species,
                             levels = levels(ACP_benthos$species),
                             labels = c("Abra(B)","Cerasto.(B)","Corbula(B)","Coroph.(C)",
                                        "Crangon(C)","Donax(B)","Ensis(B)",
                                        "Hedist.(P)","Lagis(P)", "Lanice(B)", "Lime.(B)",
                                        "Nephtis(P)","Nucula(B)","Owenia(P)", "Scrobi.(B)","Spisula(B)"))

colnames(ACP_benthos) = c("esp.","zone","grp","saison","alim.",PFAS_selected_benthos)
colnames(ACP_benthos)[c(14,15)] = c("L-PFOS","6:2-FTSA")


#-------------------------------------

res.pca = PCA(ACP_benthos, quali.sup = c(1,2,3,4,5))
summary(res.pca)


fviz_screeplot(res.pca)

fviz_pca_var(res.pca, col.var = "contrib", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel=T,axes = c(1,2)
)
fviz_pca_var(res.pca, col.var = "contrib", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel=T,axes = c(1,3)
)

#-------------------------------------
# contrib var axes

png(filename = "Output/BENTHOS/BENTHOS_acp_contrib_axes.png",
    width = 20,height = 10,units = "cm",res = 280)
# Contribution ? la premi?re dimension
p1=fviz_contrib (res.pca, "var", axes = 1)
# Contribution ? la deuxi?me dimension
p2=fviz_contrib (res.pca, "var", axes = 2)
p3=fviz_contrib (res.pca, "var", axes = 3)
grid.arrange(p1,p2,p3,nrow=3)
dev.off()




#-------------------------------------
# var

png(filename = "Output/BENTHOS/ACP_PFAS/BENTHOS_acp_PFAS_var.png",
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

png(filename = "Output/BENTHOS/ACP_PFAS/BENTHOS_acp_PFAS_ind.png",
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


