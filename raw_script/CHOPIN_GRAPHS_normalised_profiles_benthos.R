####################################
##         PROJET CHOPIN
## 
##          GRAPHIQUES
##     PROFILS PAR FAMILLE
##           BENTHOS
##      
####################################


source('Script/CHOPIN_GRAPHS_colors.R')
source('Script/CHOPIN_DATA_FORMATTING_contaminants.R')


# --------------------------------------------------------------
# Suppression des composes non analyses (le cas echeant)

PFAS_ALL_lab = PFAS_ALL_lab[-which(PFAS_ALL=="MeFOSA"|PFAS_ALL=="EtFOSA")]
ss_famille_ALL = ss_famille_ALL[-which(PFAS_ALL=="MeFOSA"|PFAS_ALL=="EtFOSA")]
n_C_ALL = n_C_ALL[-which(PFAS_ALL=="MeFOSA"|PFAS_ALL=="EtFOSA")]
PFAS_ALL = PFAS_ALL[-which(PFAS_ALL=="MeFOSA"|PFAS_ALL=="EtFOSA")]


FOSAs_ALL = FOSAs_ALL[-which(FOSAs_ALL=="MeFOSA"|FOSAs_ALL=="EtFOSA")]
FOSAs_ALL_lab = FOSAs_ALL_lab[-which(FOSAs_ALL_lab=="MeFOSA"|FOSAs_ALL_lab=="EtFOSA")]


#-------------------------------------------
# Mise en forme jeu de donnees pour les PCB selectionnes

other_PCBs_benthos = setdiff(PCB, PCB_selected_benthos)
PCB_benthos = c(PCB_selected_benthos,"otherPCBs")
PCB_benthos_lab = c(PCB_selected_benthos,"Autres")

benthos_contam$otherPCBs_normalised_sum_ng.gdw = apply(benthos_contam[, paste(other_PCBs_benthos,"_ng_g.1ps",sep="")], MARGIN = 1, FUN = sum, na.rm=T)/benthos_contam$sommePCB_ng_gdw


normalised_PCB = data.frame("contamination"=unlist(benthos_contam[,paste(PCB_benthos,"_normalised_sum_ng.gdw",sep="")])*100, 
                            "contam"=unlist(lapply(PCB_benthos, FUN = rep.int, times=dim(benthos_contam)[1])))
normalised_PCB$sample_TAG = rep.int(benthos_contam$sample_TAG, times = length(PCB_benthos))
normalised_PCB$season = rep.int(benthos_contam$season, times = length(PCB_benthos))
normalised_PCB$zone = rep.int(benthos_contam$zone, times = length(PCB_benthos))
normalised_PCB$species = rep.int(benthos_contam$species, times = length(PCB_benthos))
normalised_PCB$labels = rep.int(benthos_contam$labels, times = length(PCB_benthos))

normalised_PCB$contam = factor(x = normalised_PCB$contam,levels = PCB_benthos,labels = PCB_benthos)

normalised_PCB_juin = normalised_PCB[which(normalised_PCB$season=="Printemps"),]
normalised_PCB_oct = normalised_PCB[which(normalised_PCB$season=="Automne"),]


#-------------------------------------------
# Mise en forme jeu de donnees PFAS

other_PFASs_benthos = setdiff(PFAS_ALL, PFAS_selected_benthos)

PFAS_benthos = c(PFAS_selected_benthos,"otherPFASs")
PFAS_benthos_lab = c(PFAS_ALL_lab[which(PFAS_ALL %in% PFAS_selected_benthos==T)],
                 "Autres")

benthos_contam$otherPFASs_normalised_sum_ng.gdw = apply(benthos_contam[, other_PFASs_benthos], MARGIN = 1, FUN = sum, na.rm=T)/benthos_contam$sommePFAS_ng_gdw

normalised_PFAS = data.frame("contamination"=unlist(benthos_contam[,paste(PFAS_benthos,"_normalised_sum_ng.gdw",sep="")])*100, 
                             "contam"=unlist(lapply(PFAS_benthos, FUN = rep.int, times=dim(benthos_contam)[1])))
normalised_PFAS$sample_TAG = rep.int(benthos_contam$sample_TAG, times = length(PFAS_benthos))
normalised_PFAS$season = rep.int(benthos_contam$season, times = length(PFAS_benthos))
normalised_PFAS$zone = rep.int(benthos_contam$zone, times = length(PFAS_benthos))
normalised_PFAS$species = rep.int(benthos_contam$species, times = length(PFAS_benthos))
normalised_PFAS$labels = rep.int(benthos_contam$labels, times = length(PFAS_benthos))

normalised_PFAS$contam = factor(x = normalised_PFAS$contam,levels = PFAS_benthos,labels = PFAS_benthos)

normalised_PFAS_juin = normalised_PFAS[which(normalised_PFAS$season=="Printemps"),]
normalised_PFAS_oct = normalised_PFAS[which(normalised_PFAS$season=="Automne"),]


#-------------------------------------------
# Mise en forme jeu de donnees HBCDD

normalised_HBCDD = data.frame("contamination"=unlist(benthos_contam[,paste(HBCDD_selected_benthos,"_normalised_sum_ng.gdw",sep="")])*100, 
                              "contam"=unlist(lapply(HBCDD_selected_benthos, FUN = rep.int, times=dim(benthos_contam)[1])))
normalised_HBCDD$sample_TAG = rep.int(benthos_contam$sample_TAG, times = length(HBCDD_selected_benthos))
normalised_HBCDD$season = rep.int(benthos_contam$season, times = length(HBCDD_selected_benthos))
normalised_HBCDD$zone = rep.int(benthos_contam$zone, times = length(HBCDD_selected_benthos))
normalised_HBCDD$species = rep.int(benthos_contam$species, times = length(HBCDD_selected_benthos))
normalised_HBCDD$labels = rep.int(benthos_contam$labels, times = length(HBCDD_selected_benthos))

normalised_HBCDD_juin = normalised_HBCDD[which(normalised_HBCDD$season=="Printemps"),]
normalised_HBCDD_oct = normalised_HBCDD[which(normalised_HBCDD$season=="Automne"),]



#----------------------------------------------
# graphique Stacked Bar Charts

png("Output/BENTHOS/Profils_normalised_benthos_.jpeg",width = 30, height = 25, units = "cm",res=720)


b1 = ggplot(normalised_PCB_juin, aes(x=sample_TAG, y=contamination, fill=contam)) + 
  geom_bar(stat="identity", width=0.5, color="black")+
  scale_color_viridis(discrete = TRUE, option = "D")+
  scale_fill_viridis(discrete = TRUE) +
  scale_x_discrete(limits=normalised_PCB_juin$sample_TAG[1:length(benthos_contam_juin$sample_TAG)][order(benthos_contam_juin$zone)[1:length(benthos_contam_juin$sample_TAG)]]) +
  #scale_x_discrete(labels=benthos_contam_juin$species)+
  annotate(size=3,"text", x=3.5, y=105, label="Embouchure")+
  geom_vline(xintercept=6.5) +
  annotate(size=3,"text", x=9.5, y=105, label="Fosse Nord")+
  geom_vline(xintercept=12.5) + 
  annotate(size=3,"text", x=14.5, y=105, label="Fosse Sud")+
  theme_bw() +
  theme(legend.position="none",title = element_text(size=12)) +
  theme(axis.text.x = element_blank())+
  #theme(axis.text.x=element_text(size=9, angle=45, hjust=1), legend.position="bottom") +
  #guides(col=guide_legend(nrow=2)) +
  labs(title="Juin 2017", x=NULL, y=expression(paste("% de la ",sum()," des PCB")))


b2 = ggplot(normalised_PCB_oct, aes(x=sample_TAG, y=contamination, fill=contam)) + 
  geom_bar(stat="identity", width=0.5, color="black")+
  scale_color_viridis(discrete = TRUE, option = "D")+
  scale_fill_viridis(discrete = TRUE, labels = PCB_benthos_lab) +
  scale_x_discrete(limits=normalised_PCB_oct$sample_TAG[1:length(benthos_contam_oct$sample_TAG)][order(benthos_contam_oct$zone)[1:length(benthos_contam_oct$sample_TAG)]]) +
  #scale_x_discrete(labels=benthos_contam_juin$species)+
  annotate(size=2,"text", x=1, y=105, label="CH")+
  geom_vline(xintercept=1.5) +
  annotate(size=3,"text", x=7.5, y=105, label="Embouchure")+
  geom_vline(xintercept=14.5) +
  annotate(size=3,"text", x=17.5, y=105, label="Fosse Nord")+
  geom_vline(xintercept=21.5) +
  annotate(size=3,"text", x=27.5, y=105, label="Fosse Sud")+
  theme_bw() +
  #theme(legend.position="none") +
  theme(axis.text.x = element_blank(),title = element_text(size=12))+
  #theme(axis.text.x=element_text(size=9, angle=45, hjust=1), legend.position="bottom") +
  guides(size=F, fill=guide_legend(ncol=1)) +
  theme(plot.margin = margin(0.2,0.8,0.2,0.2,"cm"), legend.text = element_text(size=12))+
  labs(title="Octobre 2017", x=NULL, y=NULL, fill="PCB")



b3 = ggplot(normalised_PFAS_juin, aes(x=sample_TAG, y=contamination, fill=contam)) + 
  geom_bar(stat="identity", width=0.5, color="black")+
  scale_color_manual(values = PALETTE_PFAS_benthos)+
  scale_fill_manual(values = PALETTE_PFAS_benthos)+
  # scale_color_viridis(discrete = TRUE, option = "D")+
  # scale_fill_viridis(discrete = TRUE) +
  scale_x_discrete(limits=normalised_PFAS_juin$sample_TAG[1:length(benthos_contam_juin$sample_TAG)][order(benthos_contam_juin$zone)[1:length(benthos_contam_juin$sample_TAG)]]) +
  #scale_x_discrete(labels=benthos_contam_juin$species)+
  geom_vline(xintercept=6.5) +
  geom_vline(xintercept=12.5) + 
  # annotate(size=3,"text", x=3.5, y=105, label="Embouchure")+
  # annotate(size=3,"text", x=9.5, y=105, label="Fosse Nord")+
  # annotate(size=3,"text", x=14.5, y=105, label="Fosse Sud")+
  theme_bw() +
  theme(legend.position="none") +
  theme(axis.text.x = element_blank())+
  #theme(axis.text.x=element_text(size=9, angle=45, hjust=1), legend.position="bottom") +
  #guides(col=guide_legend(nrow=2)) +
  labs(title=NULL, x=NULL, y=expression(paste("% de la ",sum()," des PFAS")))


b4 = ggplot(normalised_PFAS_oct, aes(x=sample_TAG, y=contamination, fill=contam)) + 
  geom_bar(stat="identity", width=0.5, color="black")+
  scale_color_manual(values = PALETTE_PFAS_benthos)+
  scale_fill_manual(values = PALETTE_PFAS_benthos, labels = PFAS_benthos_lab)+
  # scale_color_viridis(discrete = TRUE, option = "D")+
  # scale_fill_viridis(discrete = TRUE) +
  scale_x_discrete(limits=normalised_PFAS_oct$sample_TAG[1:length(benthos_contam_oct$sample_TAG)][order(benthos_contam_oct$zone)[1:length(benthos_contam_oct$sample_TAG)]]) +
  #scale_x_discrete(labels=benthos_contam_juin$species)+
  # annotate(size=2,"text", x=1, y=105, label="CH")+
  # annotate(size=3,"text", x=7.5, y=105, label="Embouchure")+
  # annotate(size=3,"text", x=17.5, y=105, label="Fosse Nord")+
  # annotate(size=3,"text", x=27.5, y=105, label="Fosse Sud")+
  geom_vline(xintercept=1.5) +
  geom_vline(xintercept=14.5) +
  geom_vline(xintercept=21.5) +
  theme_bw() +
  #theme(legend.position="none") +
  theme(axis.text.x = element_blank(),plot.margin = margin(0.2,0,0.2,0.2,"cm"), legend.text = element_text(size=12))+
  #theme(axis.text.x=element_text(size=9, angle=45, hjust=1), legend.position="bottom") +
  guides(fill=guide_legend(ncol=1)) +
  labs(title=NULL, x=NULL, y=NULL, fill="PFAS")



b5 = ggplot(normalised_HBCDD_juin, aes(x=sample_TAG, y=contamination, fill=contam)) + 
  geom_bar(stat="identity", width=0.5, color="black")+
  scale_color_viridis(discrete = TRUE, option = "D")+
  scale_fill_viridis(discrete = TRUE) +
  scale_x_discrete(limits=normalised_HBCDD_juin$sample_TAG[1:length(benthos_contam_juin$sample_TAG)][order(benthos_contam_juin$zone)[1:length(benthos_contam_juin$sample_TAG)]],
                   labels=normalised_HBCDD_juin$labels[1:length(benthos_contam_juin$sample_TAG)][order(benthos_contam_juin$zone)[1:length(benthos_contam_juin$sample_TAG)]]) +
  #scale_x_discrete(labels=benthos_contam_juin$species)+
  geom_vline(xintercept=6.5) +
  geom_vline(xintercept=12.5) + 
  # annotate(size=3,"text", x=3.5, y=105, label="Embouchure")+
  # annotate(size=3,"text", x=9.5, y=105, label="Fosse Nord")+
  # annotate(size=3,"text", x=14.5, y=105, label="Fosse Sud")+
  theme_bw() +
  theme(legend.position="none") +
  theme(axis.text.x = element_blank())+
  theme(axis.text.x=element_text(size=12, angle=90,hjust=1)) +
  #guides(col=guide_legend(nrow=2)) +
  labs(title=NULL, x=NULL, y=expression(paste("% de la ", sum()," des HBCDD")))


b6 = ggplot(normalised_HBCDD_oct, aes(x=sample_TAG, y=contamination, fill=contam)) + 
  geom_bar(stat="identity", width=0.5, color="black")+
  scale_color_viridis(discrete = TRUE, option = "D")+
  scale_fill_viridis(discrete = TRUE) +
  scale_x_discrete(limits=normalised_HBCDD_oct$sample_TAG[1:length(benthos_contam_oct$sample_TAG)][order(benthos_contam_oct$zone)[1:length(benthos_contam_oct$sample_TAG)]],
                   labels=normalised_HBCDD_oct$labels[1:length(benthos_contam_oct$sample_TAG)][order(benthos_contam_oct$zone)[1:length(benthos_contam_oct$sample_TAG)]]) +
  #scale_x_discrete(labels=benthos_contam_juin$species)+
  # annotate(size=2,"text", x=1, y=105, label="CH")+
  # annotate(size=3,"text", x=7.5, y=105, label="Embouchure")+
  # annotate(size=3,"text", x=17.5, y=105, label="Fosse Nord")+
  # annotate(size=3,"text", x=27.5, y=105, label="Fosse Sud")+
  geom_vline(xintercept=1.5) +
  geom_vline(xintercept=14.5) +
  geom_vline(xintercept=21.5) +
  theme_bw() +
  guides(size=F, fill=guide_legend(ncol=1)) +
  theme(plot.margin = margin(0.2,0.2,0.2,0.2,"cm"))+
  theme(axis.text.x=element_text(size=12, angle=90,vjust=0.4, hjust=1), 
        legend.position="right", legend.text = element_text(size=12)) +
  labs(title=NULL, x=NULL, y=NULL, fill="HBCDD")

a = 7
b = 7
c = 6
z=a+b+c

grid.arrange(b1,b2,b3,b4,b5,b6,
             ncol=3,nrow=z,
             layout_matrix = rbind(t(matrix(rep.int(c(1,2,2),times = a),ncol = a, nrow=3)),
                                   t(matrix(rep.int(c(3,4,4),times = b),ncol = b, nrow=3)),
                                   t(matrix(rep.int(c(5,6,6),times = c),ncol = c, nrow=3)))
)

dev.off()
