####################################
##         PROJET CHOPIN
## 
##           ANALYSES
##           BENTHOS
##       ACP --  PCB, PFAS & lip
##      
####################################


rm(list=ls()) # clear workspace


library(xlsx) # Chargement des packages
source('Script/CHOPIN_DATA_FORMATTING_MAIN.R')  # Chargement des data


#install.packages(c("FactoMineR", "factoextra"))
library("FactoMineR")
library("factoextra")
library("missMDA")

# graphiques
library(gridExtra) # graphiques
library(RColorBrewer)

#####################################################################################################################
#####################################################################################################################

###    ACP BENTHOS --  PCB, PFAS & lip

#####################################################################################################################
#####################################################################################################################


benthos_ACP = benthos_contam[-c(19,38,39,43,44,48, # non-dosed PCB
                                24,25,27,29,50),  # non-dosed PFAS
                             c(2,4,6,10,16:34,35:63,68,69,70)] # selection variables analyse
colnames(benthos_ACP)[5:23] = PCB_lab


res.pca <- PCA(benthos_ACP_HBCDD, graph = FALSE, quali.sup = c(1:3,9,10))


test = colnames(benthos_ACP)=="NaDONA"|colnames(benthos_ACP)=="HFPO.DA"|
  colnames(benthos_ACP)=="X4.2.FTSA"|colnames(benthos_ACP)=="PFHpS"|
  colnames(benthos_ACP)=="PFBS"|colnames(benthos_ACP)=="FOSAA"|
  colnames(benthos_ACP)=="X6.2.diPAP"|colnames(benthos_ACP)=="PFDS"|
  colnames(benthos_ACP)=="PFECHS"
benthos_ACP = benthos_ACP[,-which(test==T)]


res.pca <- PCA(benthos_ACP, graph = FALSE, quali.sup = c(1:3,44,45))

fviz_screeplot(res.pca)

#-------------------------------------
# contrib var axes

png(filename = "Output/BENTHOS/BENTHOS_acp_PCB-PFAS-lip_contrib_axes.png",
    width = 20,height = 10,units = "cm",res = 280)
# Contribution ? la premi?re dimension
p1=fviz_contrib (res.pca, "var", axes = 1)
# Contribution ? la deuxi?me dimension
p2=fviz_contrib (res.pca, "var", axes = 2)
grid.arrange(p1,p2,nrow=2)
dev.off()

#-------------------------------------
# var

png(filename = "Output/BENTHOS/BENTHOS_acp_PCB-PFAS-lip_var.png",
    width = 20,height = 10,units = "cm",res = 280)
# Graphique des variables
grp=c("lip",rep.int(x = "PCB", 19), rep.int(x = "PFAS", 20))
p1=fviz_pca_var(res.pca,  col.var = grp)
quali.var <- get_pca_var(res.pca)
quali.var
p2=fviz_pca_var(res.pca, col.var = "contrib", 
                gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))
grid.arrange(p1,p2,ncol=2)
dev.off()

#-------------------------------------
# ind

png(filename = "Output/BENTHOS/BENTHOS_acp_PCB-PFAS-lip_ind.png",
    width = 20,height = 15,units = "cm",res = 280)

# zone
p1=fviz_pca_ind(res.pca, 
                geom.ind = "point", # Montre les points seulement (mais pas le "text")
                col.ind=benthos_ACP$zone,
                palette = brewer.pal(n = length(levels(benthos_ACP$zone)), name = "Set1"),
                addEllipses = TRUE, ellipse.type = "convex", 
                repel = TRUE, # Avoid text overlapping
                legend.title="Zone",
                mean.point = FALSE
) 


#  saison
p2=fviz_pca_ind(res.pca, 
                geom.ind = "point",
                col.ind=benthos_ACP$season, # color by groups 
                palette = brewer.pal(n = 2, name = "Set1"),
                addEllipses = TRUE, ellipse.type = "convex", 
                repel = TRUE, # Avoid text overlapping
                legend.title="Saison",
                mean.point = FALSE
) 

#  taxon
p3=fviz_pca_ind(res.pca, 
                geom.ind = "point",
                col.ind=benthos_ACP$grp, # color by groups 
                palette = brewer.pal(n = 3, name = "Set1"),
                addEllipses = TRUE, ellipse.type = "convex", 
                repel = TRUE, # Avoid text overlapping
                legend.title="Grp. Taxo.",
                mean.point = FALSE
) 


#  alim
p4=fviz_pca_ind(res.pca, 
                geom.ind = "point",
                col.ind=benthos_ACP$alim, # color by groups 
                palette = brewer.pal(n = 5, name = "Set1"),
                addEllipses = TRUE, ellipse.type = "convex", 
                repel = TRUE, # Avoid text overlapping
                legend.title="Alimentation",
                mean.point = FALSE
) 
grid.arrange(p1,p2,p3,p4,ncol=2, nrow=2)
dev.off()








#####################################################################################################################
#####################################################################################################################

###    ACP BENTHOS --  HBCDD, CB153 & lip

#####################################################################################################################
#####################################################################################################################



benthos_ACP_HBCDD = benthos_contam[-c(19,38,39,43,44,48, # non dos?s PCB
                                      5,6,7,10,14,26:29,32,35,36,45,46,50),
                                   c(2,4,6,10,29,65:67,68,69,70)] # s?lection des variables ? analyser
colnames(benthos_ACP_HBCDD)[4:8] = c("Lip","CB153","a.HBCDD","b.HBCDD","g.HBCDD")

res.pca <- PCA(benthos_ACP_HBCDD, graph = FALSE, quali.sup = c(1:3,9,10))


fviz_screeplot(res.pca)

#-------------------------------------
# contrib var axes

png(filename = "C:/Users/florence.mounier/Work Folders/Documents/Mes documents/CDD IR CHOPIN/2020.06 - Rapport FINAL/BENTHOS/BENTHOS_acp_HBCDD_contrib_axes.png",
    width = 20,height = 5,units = "cm",res = 280)
# Contribution ? la premi?re dimension
p1=fviz_contrib (res.pca, "var", axes = 1)
# Contribution ? la deuxi?me dimension
p2=fviz_contrib (res.pca, "var", axes = 2)
grid.arrange(p1,p2,ncol=2)
dev.off()

#-------------------------------------
# var

png(filename = "C:/Users/florence.mounier/Work Folders/Documents/Mes documents/CDD IR CHOPIN/2020.06 - Rapport FINAL/BENTHOS/BENTHOS_acp_HBCDD_var.png",
    width = 20,height = 10,units = "cm",res = 280)
# Graphique des variables
grp=c("lip","PCB",rep.int("HBCDD", 3))
p1=fviz_pca_var(res.pca,  col.var = grp)
quali.var <- get_pca_var(res.pca)
quali.var
p2=fviz_pca_var(res.pca, col.var = "contrib", 
                gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))
grid.arrange(p1,p2,ncol=2)
dev.off()



#-------------------------------------
# ind

png(filename = "C:/Users/florence.mounier/Work Folders/Documents/Mes documents/CDD IR CHOPIN/2020.06 - Rapport FINAL/BENTHOS/BENTHOS_acp_HBCDD_ind.png",
    width = 20,height = 15,units = "cm",res = 280)

# zone
p1=fviz_pca_ind(res.pca, 
                geom.ind = "point", # Montre les points seulement (mais pas le "text")
                col.ind=benthos_ACP_HBCDD$zone,
                palette = brewer.pal(n = length(levels(benthos_ACP_HBCDD$zone)), name = "Set1"),
                addEllipses = TRUE, ellipse.type = "convex", 
                repel = TRUE, # Avoid text overlapping
                legend.title="Zone",
                mean.point = FALSE
) 


#  saison
p2=fviz_pca_ind(res.pca, 
                geom.ind = "point",
                col.ind=benthos_ACP_HBCDD$season, # color by groups 
                palette = brewer.pal(n = 2, name = "Set1"),
                addEllipses = TRUE, ellipse.type = "convex", 
                repel = TRUE, # Avoid text overlapping
                legend.title="Saison",
                mean.point = FALSE
) 

#  taxon
p3=fviz_pca_ind(res.pca, 
                geom.ind = "point",
                col.ind=benthos_ACP_HBCDD$grp, # color by groups 
                palette = brewer.pal(n = 3, name = "Set1"),
                addEllipses = TRUE, ellipse.type = "convex", 
                repel = TRUE, # Avoid text overlapping
                legend.title="Grp. Taxo.",
                mean.point = FALSE
) 


#  alim
p4=fviz_pca_ind(res.pca, 
                geom.ind = "point",
                col.ind=benthos_ACP_HBCDD$alim, # color by groups 
                palette = brewer.pal(n = 5, name = "Set1"),
                addEllipses = TRUE, ellipse.type = "convex", 
                repel = TRUE, # Avoid text overlapping
                legend.title="Alimentation",
                mean.point = FALSE
) 
grid.arrange(p1,p2,p3,p4,ncol=2, nrow=2)
dev.off()

