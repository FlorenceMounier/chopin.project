####################################
##         PROJET CHOPIN
## 
##         MAIN SCRIPT
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

#-------------------------------------------------------------
## ANALYSES DESCRIPTIVES DES DONNEES

# fréquence de détection des contaminants

source('Script/CHOPIN_ANALYSES_MAIN.R')



#-------------------------------------------------------------
## REPRESENTATIONS GRAPHIQUES DES DONNEES


library(tidyverse) # contient ggplot2
library(gg.gap)
library(cowplot)
library(viridis)
library(gridExtra)
library(ggmap)
library(devtools)

sed_contam$zone = factor(sed_contam$zone,levels = levels(as.factor(sed_contam$zone)),
                         labels = c("EMB","FN","FS"))


par(mar=c(4,4,3,2))
p1 = ggplot(sed_contam, aes(x=zone, y=sommePCB_ng_gdw, col=zone, fill=zone)) + 
  geom_dotplot(binaxis='y', stackdir='center')+
  theme_bw() +
  theme(legend.position="none") +
  theme(axis.title.y = element_text(size=8), title = element_text(size=8))+
  #theme(axis.text.x=element_text(size=9, angle=45, hjust=1), legend.position="bottom") +
  stat_summary(fun = mean, geom="crossbar", size=0.5, color="darkgrey", width=.5) +
  stat_summary(fun = median, geom="crossbar", size=0.5, color="black", width=.5) +
  expand_limits(y=c(1, 2500))+
  #guides(col=guide_legend(nrow=2)) +
  labs(title=paste("SEDIMENT"), x=NULL, y=expression(paste(sum(),"PCB (log ng.",g^-1,"ps)"))) +
  scale_y_log10()

p2 = ggplot(benthos_contam, aes(x=grp, y=sommePCB_ng_gdw, color=grp, fill=grp))+
  geom_dotplot(binaxis='y', stackdir='center')+
  theme_bw() +
  theme(legend.position="none") +
  theme(axis.title.y = element_text(size=6),title = element_text(size=8))+
  #theme(axis.text.x=element_text(size=9, angle=45, hjust=1), legend.position="bottom") +
  stat_summary(fun = mean, geom="crossbar", size=0.5, color="darkgrey", width=.5) +
  stat_summary(fun = median, geom="crossbar", size=0.5, color="black", width=.5) +
  #guides(col=guide_legend(nrow=2)) +
  expand_limits(y=c(1, 2500))+
  labs(title=paste("BENTHOS"), x=NULL, y=NULL) +
  scale_y_log10()

p3 = ggplot(soles_contam, aes(x=grp, y=sommePCB_ng_gdw, color=grp, fill=grp)) + 
  geom_dotplot(binaxis='y', stackdir='center')+
  theme_bw() +
  theme(legend.position="none") +
  theme(axis.title.y = element_text(size=6), title = element_text(size=8))+
  #theme(axis.text.x=element_text(size=9, angle=45, hjust=1), legend.position="bottom") +
  stat_summary(fun = mean, geom="crossbar", size=0.5, color="darkgrey", width=.5) +
  stat_summary(fun = median, geom="crossbar", size=0.5, color="black", width=.5) +
  expand_limits(y=c(1, 2500))+
  #guides(col=guide_legend(nrow=2)) +
  labs(title=paste("SOLES"), x=NULL,y=NULL) +
  scale_y_log10()

plot = ggdraw()+
  draw_plot(p1, x=0, y=0, width=0.3, height=1) +
  draw_plot(p2, x=0.3,y=0, 0.4, 1) +
  draw_plot(p3, x=0.7,y=0, 0.3, 1)


ggsave(filename = "Output/transfert_PCB.jpeg",
       plot = plot,device = "jpeg",width = 20,height = 7, units = "cm",dpi=300)



par(mar=c(4,4,3,2))
p1 = ggplot(sed_contam, aes(x=zone, y=sommePFAS_ng_gdw, col=zone, fill=zone)) + 
  geom_dotplot(binaxis='y', stackdir='center')+
  theme_bw() +
  theme(legend.position="none") +
  theme(axis.title.y = element_text(size=8), title = element_text(size=8))+
  #theme(axis.text.x=element_text(size=9, angle=45, hjust=1), legend.position="bottom") +
  stat_summary(fun = mean, geom="crossbar", size=0.5, color="darkgrey", width=.5) +
  stat_summary(fun = median, geom="crossbar", size=0.5, color="black", width=.5) +
  #guides(col=guide_legend(nrow=2)) +
  expand_limits(y=c(0.1,200))+
  labs(title=paste("SEDIMENT"), x=NULL, y=expression(paste(sum(),"PFAS (log ng.",g^-1,"ps)"))) +
  scale_y_log10()

p2 = ggplot(benthos_contam, aes(x=grp, y=sommePFAS_ng_gdw, color=grp, fill=grp))+
  geom_dotplot(binaxis='y', stackdir='center')+
  theme_bw() +
  theme(legend.position="none") +
  theme(axis.title.y = element_text(size=6),title = element_text(size=8))+
  #theme(axis.text.x=element_text(size=9, angle=45, hjust=1), legend.position="bottom") +
  stat_summary(fun = mean, geom="crossbar", size=0.5, color="darkgrey", width=.5) +
  stat_summary(fun = median, geom="crossbar", size=0.5, color="black", width=.5) +
  #guides(col=guide_legend(nrow=2)) +
  expand_limits(y=c(0.1,200))+
  labs(title=paste("BENTHOS"), x=NULL, y=NULL) +
  scale_y_log10()

p3 = ggplot(soles_contam, aes(x=grp, y=sommePFAS_ng_gdw, color=grp, fill=grp)) + 
  geom_dotplot(binaxis='y', stackdir='center')+
  theme_bw() +
  theme(legend.position="none") +
  theme(axis.title.y = element_text(size=6), title = element_text(size=8))+
  #theme(axis.text.x=element_text(size=9, angle=45, hjust=1), legend.position="bottom") +
  stat_summary(fun = mean, geom="crossbar", size=0.5, color="darkgrey", width=.5) +
  stat_summary(fun = median, geom="crossbar", size=0.5, color="black", width=.5) +
  #guides(col=guide_legend(nrow=2)) +
  expand_limits(y=c(0.1,200))+
  labs(title=paste("SOLES"), x=NULL,y=NULL) +
  scale_y_log10()

plot = ggdraw()+
  draw_plot(p1, x=0, y=0, width=0.3, height=1) +
  draw_plot(p2, x=0.3,y=0, 0.4, 1) +
  draw_plot(p3, x=0.7,y=0, 0.3, 1)

plot 
ggsave(filename = "Output/transfert_PFAS.jpeg",
       plot = plot,device = "jpeg",width = 20,height = 7, units = "cm",dpi=300)


par(mar=c(4,4,3,2))
p1 = ggplot(sed_contam, aes(x=zone, y=sommeHBCDD_ng_gdw, col=zone, fill=zone)) + 
  geom_dotplot(binaxis='y', stackdir='center')+
  theme_bw() +
  theme(legend.position="none") +
  theme(axis.title.y = element_text(size=8), title = element_text(size=8))+
  #theme(axis.text.x=element_text(size=9, angle=45, hjust=1), legend.position="bottom") +
  stat_summary(fun = mean, geom="crossbar", size=0.5, color="darkgrey", width=.5) +
  stat_summary(fun = median, geom="crossbar", size=0.5, color="black", width=.5) +
  #guides(col=guide_legend(nrow=2)) +
  expand_limits(y=c(0.0001,110))+
  labs(title=paste("SEDIMENT"), x=NULL, y=expression(paste(sum(),"HBCDD (log ng.",g^-1,"ps)"))) +
  scale_y_log10()

p2 = ggplot(benthos_contam, aes(x=grp, y=sommeHBCDD_ng_gdw, color=grp, fill=grp))+
  geom_dotplot(binaxis='y', stackdir='center')+
  theme_bw() +
  theme(legend.position="none") +
  theme(axis.title.y = element_text(size=6),title = element_text(size=8))+
  #theme(axis.text.x=element_text(size=9, angle=45, hjust=1), legend.position="bottom") +
  stat_summary(fun = mean, geom="crossbar", size=0.5, color="darkgrey", width=.5) +
  stat_summary(fun = median, geom="crossbar", size=0.5, color="black", width=.5) +
  #guides(col=guide_legend(nrow=2)) +
  expand_limits(y=c(0.0001,110))+
  labs(title=paste("BENTHOS"), x=NULL, y=NULL) +
  scale_y_log10()

p3 = ggplot(soles_contam, aes(x=grp, y=sommeHBCDD_ng_gdw, color=grp, fill=grp)) + 
  geom_dotplot(binaxis='y', stackdir='center')+
  theme_bw() +
  theme(legend.position="none") +
  theme(axis.title.y = element_text(size=6), title = element_text(size=8))+
  #theme(axis.text.x=element_text(size=9, angle=45, hjust=1), legend.position="bottom") +
  stat_summary(fun = mean, geom="crossbar", size=0.5, color="darkgrey", width=.5) +
  stat_summary(fun = median, geom="crossbar", size=0.5, color="black", width=.5) +
  #guides(col=guide_legend(nrow=2)) +
  expand_limits(y=c(0.0001,110))+
  labs(title=paste("SOLES"), x=NULL,y=NULL) +
  scale_y_log10()

plot = ggdraw()+
  draw_plot(p1, x=0, y=0, width=0.3, height=1) +
  draw_plot(p2, x=0.3,y=0, 0.4, 1) +
  draw_plot(p3, x=0.7,y=0, 0.3, 1)

ggsave(filename = "Output/transfert_HBCDD.jpeg",
       plot = plot,device = "jpeg",width = 20,height = 7, units = "cm",dpi=300)
