####################################
##         PROJET CHOPIN
## 
##          GRAPHIQUES
##     PROFILS PAR FAMILLE
##            SOLES
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

other_PCBs_soles = setdiff(PCB, PCB_selected_soles)
PCB_soles = c(PCB_selected_soles,"otherPCBs")
PCB_soles_lab = c(PCB_selected_soles,"Autres")

soles_contam$otherPCBs_normalised_sum_ng.gdw = apply(soles_contam[, paste(other_PCBs_soles,"_ng_g.1ps",sep="")], MARGIN = 1, FUN = sum, na.rm=T)/soles_contam$sommePCB_ng_gdw

normalised_PCB = data.frame("contamination"=unlist(soles_contam[,paste(PCB_soles,"_normalised_sum_ng.gdw",sep="")])*100, 
                            "contam"=unlist(lapply(PCB_soles, FUN = rep.int, times=dim(soles_contam)[1])))
normalised_PCB$sample_TAG = rep.int(soles_contam$sample_TAG, times = length(PCB_soles))
normalised_PCB$season = rep.int(soles_contam$season, times = length(PCB_soles))
normalised_PCB$zone = rep.int(soles_contam$zone, times = length(PCB_soles))
normalised_PCB$sexe = rep.int(soles_contam$sexe, times = length(PCB_soles))
normalised_PCB$grp = rep.int(soles_contam$grp, times = length(PCB_soles))
normalised_PCB$year = rep.int(soles_contam$year, times = length(PCB_soles))

# order labels
normalised_PCB$contam = factor(x = normalised_PCB$contam,levels = PCB_soles, labels = PCB_soles)

normalised_PCB_juin = normalised_PCB[which(normalised_PCB$season=="Printemps"),]
normalised_PCB_oct = normalised_PCB[which(normalised_PCB$season=="Automne"),]



#-------------------------------------------
# Mise en forme jeu de donnees PFAS

other_PFASs_soles = setdiff(PFAS_ALL, PFAS_selected_soles)

PFAS_soles = c(PFAS_selected_soles,"otherPFASs")
PFAS_soles_lab = c(PFAS_ALL_lab[which(PFAS_ALL %in% PFAS_selected_soles==T)],
                     "Autres")

soles_contam$otherPFASs_normalised_sum_ng.gdw = apply(soles_contam[, other_PFASs_soles], MARGIN = 1, FUN = sum, na.rm=T)/soles_contam$sommePFAS_ng_gdw

normalised_PFAS = data.frame("contamination"=unlist(soles_contam[,paste(PFAS_soles,"_normalised_sum_ng.gdw",sep="")])*100, 
                             "contam"=unlist(lapply(PFAS_soles, FUN = rep.int, times=dim(soles_contam)[1])))
normalised_PFAS$sample_TAG = rep.int(soles_contam$sample_TAG, times = length(PFAS_soles))
normalised_PFAS$season = rep.int(soles_contam$season, times = length(PFAS_soles))
normalised_PFAS$zone = rep.int(soles_contam$zone, times = length(PFAS_soles))
normalised_PFAS$sexe = rep.int(soles_contam$sexe, times = length(PFAS_soles))
normalised_PFAS$grp = rep.int(soles_contam$grp, times = length(PFAS_soles))
normalised_PFAS$year = rep.int(soles_contam$year, times = length(PFAS_soles))


# order PFAS par sous famille dans les labels
normalised_PFAS$contam = factor(x = normalised_PFAS$contam, levels = PFAS_soles, labels = PFAS_soles)

normalised_PFAS_juin = normalised_PFAS[which(normalised_PFAS$season=="Printemps"),]
normalised_PFAS_oct = normalised_PFAS[which(normalised_PFAS$season=="Automne"),]



#-------------------------------------------
# Mise en forme jeu de donnees HBCDD

normalised_HBCDD = data.frame("contamination"=unlist(soles_contam[,paste(HBCDD_selected_soles_abund,"_normalised_sum_ng.gdw",sep="")])*100, 
                              "contam"=unlist(lapply(HBCDD_selected_soles_abund, FUN = rep.int, times=dim(soles_contam)[1])))
normalised_HBCDD$sample_TAG = rep.int(soles_contam$sample_TAG, times = length(HBCDD_selected_soles_abund))
normalised_HBCDD$season = rep.int(soles_contam$season, times = length(HBCDD_selected_soles_abund))
normalised_HBCDD$zone = rep.int(soles_contam$zone, times = length(HBCDD_selected_soles_abund))
normalised_HBCDD$grp = rep.int(soles_contam$grp, times = length(HBCDD_selected_soles_abund))
normalised_HBCDD$year = rep.int(soles_contam$year, times = length(HBCDD_selected_soles_abund))

normalised_HBCDD_juin = normalised_HBCDD[which(normalised_HBCDD$season=="Printemps"),]
normalised_HBCDD_oct = normalised_HBCDD[which(normalised_HBCDD$season=="Automne"),]




#----------------------------------------------
# graphique Stacked Bar Charts


png("Output/SOLES/Profils_normalised_soles.jpeg",
    width = 32, height = 42, units = "cm",res=280)

summary(soles_contam[which(soles_contam$season=="Printemps"),]$grp)
summary(soles_contam[which(soles_contam$season=="Printemps"),]$zone)
# G0 G1 G2 
# 6  0  0
# CH EM FN  FS 
# 1          1          2          2

b1 = ggplot(normalised_PCB_juin, aes(x=sample_TAG, y=contamination, fill=contam)) + 
  scale_x_discrete(limits=normalised_PCB_juin$sample_TAG[1:length(soles_contam_juin$sample_TAG)][order(soles_contam_juin$zone)[1:length(soles_contam_juin$sample_TAG)]]) +
  geom_bar(stat="identity", width=0.5, color="black")+
  scale_color_viridis(discrete = TRUE, option = "D")+
  scale_fill_viridis(discrete = TRUE) +
  geom_vline(xintercept=1+.5) +
  annotate(size=3,"text", x=1/2+.5, y=105, label="CH")+
  geom_vline(xintercept=1+1+.5) +
  geom_vline(xintercept=1+1+2+.5)+
  annotate(size=3,"text", x=1+1/2+.5, y=105, label="EM")+
  annotate(size=3,"text", x=1+1+2/2+.5, y=105, label="FN")+
  annotate(size=3,"text", x=1+1+2+2/2+.5, y=105, label="FS")+
  #scale_x_discrete(limits=normalised_PCB_juin$sample_TAG[1:length(soles_contam_juin$sample_TAG)][order(soles_contam_juin$zone)[1:length(soles_contam_juin$sample_TAG)]]) +
  #scale_x_discrete(labels=soles_contam_juin$species)+
  theme_bw() +
  theme(legend.position="none",title = element_text(size=12)) +
  theme(axis.text.x = element_blank())+
  #theme(axis.text.x=element_text(size=9, angle=45, hjust=1), legend.position="bottom") +
  #guides(col=guide_legend(nrow=2)) +
  labs(title="Juin 2017 - G0", x=NULL,
       y=expression(paste("% de la ",sum()," des PCB")))

summary(soles_contam[which(soles_contam$season=="Automne"),]$grp)
# G0 G1 G2 
# 10 25 11
summary(as.factor(soles_contam[which(soles_contam$season=="Automne"&soles_contam$grp=="G1"),]$year))
# 2017 2018 
# 15   10 

b2 = ggplot(normalised_PCB_oct, aes(x=sample_TAG, y=contamination, fill=contam)) + 
  geom_bar(stat="identity", width=0.5, color="black")+
  scale_color_viridis(discrete = TRUE, option = "D")+
  scale_fill_viridis(discrete = TRUE, labels = PCB_soles_lab) +
  scale_x_discrete(limits=normalised_PCB_oct$sample_TAG[1:length(soles_contam_oct$sample_TAG)][order(soles_contam_oct$grp)[1:length(soles_contam_oct$sample_TAG)]]) +
  geom_vline(xintercept=10.5) +
  annotate(size=5,"text", x=10/2+.5, y=105, label="G0 - 2017")+
  geom_vline(xintercept=10+25.5) +
  annotate(size=5,"text", x=10+25+11/2+.5, y=105, label="G2 - 2018")+
  annotate(size=5,"text", x=10+15/2+.5, y=105, label="G1 - 2017")+
  geom_vline(xintercept=10+15+.5) +
  annotate(size=5,"text", x=10+15+10/2+.5, y=105, label="G1 - 2018")+
  theme_bw() +
  #theme(legend.position="none") +
  theme(axis.text.x = element_blank(),title = element_text(size=12))+
  #theme(axis.text.x=element_text(size=9, angle=45, hjust=1), legend.position="bottom") +
  guides(size=F, fill=guide_legend(ncol=1)) +
  theme(plot.margin = margin(0.2,0.8,0.2,0.2,"cm"), legend.text = element_text(size=12))+
  labs(title="Automne", x=NULL, y=NULL, fill="PCB")



b3 = 
  ggplot(normalised_PFAS_juin, aes(x=sample_TAG, y=contamination, fill=contam)) + 
  scale_x_discrete(limits=normalised_PFAS_juin$sample_TAG[1:length(soles_contam_juin$sample_TAG)][order(soles_contam_juin$zone)[1:length(soles_contam_juin$sample_TAG)]]) +
  geom_bar(stat="identity", width=0.5, color="black")+
  scale_color_manual(values = PALETTE_PFAS_soles)+
  scale_fill_manual(values = PALETTE_PFAS_soles)+
  # scale_color_viridis(discrete = TRUE, option = "D")+
  # scale_fill_viridis(discrete = TRUE) +
  #scale_x_discrete(limits=normalised_PFAS_juin$sample_TAG[1:length(soles_contam_juin$sample_TAG)][order(soles_contam_juin$zone)[1:length(soles_contam_juin$sample_TAG)]]) +
  #scale_x_discrete(labels=soles_contam_juin$species)+
  geom_vline(xintercept=1+.5) +
  geom_vline(xintercept=1+1+.5) +
  geom_vline(xintercept=1+1+2+.5)+
  # annotate(size=3,"text", x=1/2+.5, y=110, label="CH")+
  # annotate(size=3,"text", x=1+1/2+.5, y=110, label="EM")+
  # annotate(size=3,"text", x=1+1+2/2+.5, y=110, label="FN")+
  # annotate(size=3,"text", x=1+1+2+2/2+.5, y=110, label="FS")+
  theme_bw() +
  theme(axis.text.x = element_blank(), legend.position="none")+
  #theme(axis.text.x=element_text(size=9, angle=45, hjust=1), legend.position="bottom") +
  #guides(col=guide_legend(nrow=2)) +
  labs(title=NULL, x=NULL, y=expression(paste("% de la ",sum()," des PFAS")))


b4 = ggplot(normalised_PFAS_oct, aes(x=sample_TAG, y=contamination, fill=contam)) + 
  geom_bar(stat="identity", width=0.5, color="black")+
  scale_color_manual(values = PALETTE_PFAS_soles)+
  scale_fill_manual(values = PALETTE_PFAS_soles, labels = PFAS_soles_lab)+
  # scale_color_viridis(discrete = TRUE, option = "D")+
  # scale_fill_viridis(discrete = TRUE) +
  scale_x_discrete(limits=normalised_PFAS_oct$sample_TAG[1:length(soles_contam_oct$sample_TAG)][order(soles_contam_oct$grp)]) +
  #scale_x_discrete(labels=soles_contam_juin$species)+
  geom_vline(xintercept=10.5) +
  geom_vline(xintercept=10+25.5)+
  geom_vline(xintercept=10+15+.5) +
  # annotate(size=5,"text", x=10/2+.5, y=110, label="G0 - 2017")+
  # annotate(size=5,"text", x=10+25+11/2+.5, y=110, label="G2 - 2018")+
  # annotate(size=5,"text", x=10+15/2+.5, y=110, label="G1 - 2017")+
  # annotate(size=5,"text", x=10+15+10/2+.5, y=110, label="G1 - 2018")+
  theme_bw() +
  #theme(legend.position="none") +
  theme(axis.text.x = element_blank(),plot.margin = margin(0.2,0.5,0.2,0.2,"cm"),legend.text = element_text(size=12))+
  #theme(axis.text.x=element_text(size=9, angle=45, hjust=1), legend.position="bottom") +
  guides(fill=guide_legend(ncol=1)) +
  labs(title=NULL, x=NULL, y=NULL, fill="PFAS")



b5 = ggplot(normalised_HBCDD_juin, aes(x=sample_TAG, y=contamination, fill=contam)) + 
  scale_x_discrete(limits=normalised_HBCDD_juin$sample_TAG[1:length(soles_contam_juin$sample_TAG)][order(soles_contam_juin$zone)[1:length(soles_contam_juin$sample_TAG)]],
                   labels=normalised_HBCDD_juin$sample_TAG[1:length(soles_contam_juin$sample_TAG)][order(soles_contam_juin$zone)[1:length(soles_contam_juin$sample_TAG)]]) +
  scale_color_viridis(discrete = TRUE, option = "D")+
  scale_fill_viridis(discrete = TRUE) +
  geom_bar(stat="identity", width=0.5, color="black")+
  theme( legend.position="none")+
  geom_vline(xintercept=1+.5) +
  geom_vline(xintercept=1+1+.5) +
  geom_vline(xintercept=1+1+2+.5)+
  # annotate(size=3,"text", x=1/2+.5, y=105, label="CH")+
  # annotate(size=3,"text", x=1+1/2+.5, y=105, label="EM")+
  # annotate(size=3,"text", x=1+1+2/2+.5, y=105, label="FN")+
  # annotate(size=3,"text", x=1+1+2+2/2+.5, y=105, label="FS")+
  theme_bw() +
  theme(legend.position="none") +
  theme(axis.text.x=element_text(size=9, angle=90, hjust=1), legend.text = element_text(size=12))+
  #guides(col=guide_legend(nrow=2)) +
  labs(title=NULL, x=NULL, y=expression(paste("% de la ",sum()," des HBCDD")))


b6 = ggplot(normalised_HBCDD_oct, aes(x=sample_TAG, y=contamination, fill=contam)) + 
  scale_color_viridis(discrete = TRUE, option = "D")+
  scale_fill_viridis(discrete = TRUE) +
  geom_bar(stat="identity", width=0.5, color="black")+
  scale_x_discrete(limits=normalised_HBCDD_oct$sample_TAG[1:length(soles_contam_oct$sample_TAG)][order(soles_contam_oct$grp)[1:length(soles_contam_oct$sample_TAG)]],
                   labels=normalised_HBCDD_oct$sample_TAG[1:length(soles_contam_oct$sample_TAG)][order(soles_contam_oct$grp)[1:length(soles_contam_oct$sample_TAG)]]) +
  geom_vline(xintercept=10.5) +
  geom_vline(xintercept=10+25.5) +
  geom_vline(xintercept=10+15+.5) +
  # annotate(size=3,"text", x=10/2+.5, y=110, label="G0 - 2017")+
  # annotate(size=3,"text", x=10+25+11/2+.5, y=110, label="G2 - 2018")+
  # annotate(size=3,"text", x=10+15/2+.5, y=110, label="G1 - 2017")+
  # annotate(size=3,"text", x=10+15+10/2+.5, y=110, label="G1 - 2018")+
  theme_bw() +
  guides(size=F, fill=guide_legend(ncol=1)) +
  theme(plot.margin = margin(0.2,0.2,0.2,0.2,"cm"))+
  theme(axis.text.x=element_text(size=9, angle=90, hjust=1), 
        legend.position="right", legend.text = element_text(size=12)) +
  labs(title=NULL, x=NULL, y=NULL, fill="HBCDD")

a = 7
b = 7
c = 6
z=a+b+c

grid.arrange(b1,b2,b3,b4,b5,b6,
             ncol=6,nrow=z,
             layout_matrix = rbind(t(matrix(rep.int(c(1,2,2,2,2,2),times = a),ncol = a, nrow=6)),
                                   t(matrix(rep.int(c(3,4,4,4,4,4),times = b),ncol = b, nrow=6)),
                                   t(matrix(rep.int(c(5,6,6,6,6,6),times = c),ncol = c, nrow=6)))
)

dev.off()



