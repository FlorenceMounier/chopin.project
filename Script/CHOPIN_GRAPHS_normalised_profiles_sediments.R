####################################
##         PROJET CHOPIN
## 
##          GRAPHIQUES
##     PROFILS PAR FAMILLE
##          SEDIMENTS
##      
####################################


source('Script/CHOPIN_GRAPHS_colors.R')
source('Script/CHOPIN_DATA_FORMATTING_contaminants.R')

#-------------------------------------------
# Mise en forme jeu de donnees pour les PCB selectionnes

other_PCBs_sed = setdiff(PCB, PCB_selected_sed)
PCB_sed = c(PCB_selected_sed,"otherPCBs")
PCB_sed_lab = c(PCB_selected_sed,"Autres")

sed_contam$otherPCBs_normalised_sum_ng.gdw = apply(sed_contam[, other_PCBs_sed], MARGIN = 1, FUN = sum)/sed_contam$sommePCB_ng_gdw

normalised_PCB = data.frame("contamination"=unlist(sed_contam[,paste(PCB_sed,"_normalised_sum_ng.gdw",sep="")])*100, 
                            "contam"=unlist(lapply(PCB_sed, FUN = rep.int, times=dim(sed_contam)[1])))
normalised_PCB$sample_TAG = rep.int(sed_contam$echantillon, times = length(PCB_sed))
normalised_PCB$season = rep.int(sed_contam$season, times = length(PCB_sed))
normalised_PCB$zone = rep.int(sed_contam$zone, times = length(PCB_sed))
normalised_PCB$year = rep.int(sed_contam$year, times = length(PCB_sed))

# order labels
normalised_PCB$contam = factor(x = normalised_PCB$contam, levels = PCB_sed, labels = PCB_sed_lab)

normalised_PCB_juin = normalised_PCB[which(normalised_PCB$season=="Printemps"),]
normalised_PCB_oct = normalised_PCB[which(normalised_PCB$season=="Automne"),]



#-------------------------------------------
# Mise en forme jeu de donnees PFAS

other_PFASs_sed = setdiff(PFAS_ALL, PFAS_selected_sed)

PFAS_sed = c(PFAS_selected_sed,"otherPFASs")
PFAS_sed_lab = c(PFAS_ALL_lab[which(PFAS_ALL %in% PFAS_selected_sed==T)],
                 "Autres")

sed_contam$otherPFASs_normalised_sum_ng.gdw = apply(sed_contam[, other_PFASs_sed], MARGIN = 1, FUN = sum)/sed_contam$sommePFAS_ng_gdw

normalised_PFAS = data.frame("contamination"=unlist(sed_contam[,paste(PFAS_sed,"_normalised_sum_ng.gdw",sep="")])*100, 
                             "contam"=unlist(lapply(PFAS_sed, FUN = rep.int, times=dim(sed_contam)[1])))
normalised_PFAS$sample_TAG = rep.int(sed_contam$echantillon, times = length(PFAS_sed))
normalised_PFAS$season = rep.int(sed_contam$season, times = length(PFAS_sed))
normalised_PFAS$zone = rep.int(sed_contam$zone, times = length(PFAS_sed))
normalised_PFAS$year = rep.int(sed_contam$year, times = length(PFAS_sed))

# order PFAS par sous famille dans les labels
normalised_PFAS$contam = factor(x = normalised_PFAS$contam, levels = PFAS_sed, labels = PFAS_sed_lab)


normalised_PFAS_juin = normalised_PFAS[which(normalised_PFAS$season=="Printemps"),]
normalised_PFAS_oct = normalised_PFAS[which(normalised_PFAS$season=="Automne"),]


#-------------------------------------------
# Mise en forme jeu de donne?s HBCDD

labels = c("6EM29","6EM47","6EM52","6FNprox8","6FNproxR8","6FN14","6FS19",
           "6FS59","10EM29","10EMprox47","10EM52","10FN8","10FNproxR8",
           "10FN14","10FS19","10FS59","1810EM29","1810EMprox47","1810EM52","1810FNprox8",
           "1810FN14","1810FNproxR8","1810FS19" ,"1810FS59")

normalised_HBCDD = data.frame("contamination"=unlist(sed_contam[,paste(HBCDD_selected_sed_abund,"_normalised_sum_ng.gdw",sep="")])*100, 
                              "contam"=unlist(lapply(HBCDD_selected_sed_abund, FUN = rep.int, times=dim(sed_contam)[1])))
normalised_HBCDD$sample_TAG = rep.int(sed_contam$echantillon, times = length(HBCDD_selected_sed_abund))
normalised_HBCDD$season = rep.int(sed_contam$season, times = length(HBCDD_selected_sed_abund))
normalised_HBCDD$zone = rep.int(sed_contam$zone, times = length(HBCDD_selected_sed_abund))
normalised_HBCDD$year = rep.int(sed_contam$year, times = length(HBCDD_selected_sed_abund))
normalised_HBCDD$labels = rep.int(labels, times = length(HBCDD_selected_sed_abund))

normalised_HBCDD_juin = normalised_HBCDD[which(normalised_HBCDD$season=="Printemps"),]
normalised_HBCDD_oct = normalised_HBCDD[which(normalised_HBCDD$season=="Automne"),]

TAG_juin = c(order(sed_contam_juin$zone[sed_contam_juin$year=="2017"]),
             8+order(sed_contam_juin$zone[sed_contam_juin$year=="2018"]))

TAG_oct = c(order(sed_contam_oct$zone[sed_contam_oct$year=="2017"]),
            8+order(sed_contam_oct$zone[sed_contam_oct$year=="2018"]))


png("Output/SEDIMENTS/Profils_normalised_sed.jpeg",width = 30, height = 25, units = "cm",res=720)

summary(sed_contam_juin$zone)

b1 = ggplot(normalised_PCB_juin, aes(x=sample_TAG, y=contamination, fill=contam)) + 
  geom_bar(stat="identity", width=0.5, color="black")+
  scale_color_viridis(discrete = TRUE, option = "D")+
  scale_fill_viridis(discrete = TRUE) +
  scale_x_discrete(limits=normalised_PCB_juin$sample_TAG[1:length(sed_contam_juin$echantillon)][TAG_juin]) +
  #scale_x_discrete(labels=sed_contam_juin$species)+
  geom_vline(xintercept=3+.5) +
  geom_vline(xintercept=3+3+.5) + 
  annotate(size=3,"text", x=3/2+.5, y=105, label="EM")+
  annotate(size=3,"text", x=3+3/2+.5, y=105, label="FN")+
  annotate(size=3,"text", x=3+3+2/2+.5, y=105, label="FS")+
  theme_bw() +
  theme(legend.position="none",title = element_text(size=12)) +
  theme(axis.text.x = element_blank())+
  #theme(axis.text.x=element_text(size=9, angle=45, hjust=1), legend.position="bottom") +
  #guides(col=guide_legend(nrow=2)) +
  labs(title="Juin 2017", x=NULL, y=expression(paste("% de la ",sum()," des PCB")))

summary(sed_contam_oct$zone[which(sed_contam_oct$year=="2017")])
summary(sed_contam_oct$zone[which(sed_contam_oct$year=="2018")])

b2 = ggplot(normalised_PCB_oct[which(normalised_PCB_oct$year=="2017"),],
            aes(x=sample_TAG, y=contamination, fill=contam)) + 
  geom_bar(stat="identity", width=0.5, color="black")+
  scale_color_viridis(discrete = TRUE, option = "D")+
  scale_fill_viridis(discrete = TRUE, labels = PCB_sed_lab) +
  scale_x_discrete(limits=normalised_PCB_oct[which(normalised_PCB_oct$year=="2017"),]$sample_TAG[1:length(sed_contam_oct[which(sed_contam_oct$year=="2017"),]$echantillon)]) +
  theme_bw() +
  
  geom_vline(xintercept=3+.5) +
  geom_vline(xintercept=3+3+.5) + 
  annotate(size=3,"text", x=3/2+.5, y=105, label="EM")+
  annotate(size=3,"text", x=3+3/2+.5, y=105, label="FN")+
  annotate(size=3,"text", x=3+3+2/2+.5, y=105, label="FS")+
  
  theme(legend.position="none") +
  theme(axis.text.x = element_blank(),title = element_text(size=12))+
  guides(size=F) +
  theme(legend.text = element_text(size=12))+
  labs(title="Octobre 2017", x=NULL, y=NULL, fill="PCB")


b3=ggplot(normalised_PCB_oct[which(normalised_PCB_oct$year=="2018"),],
          aes(x=sample_TAG, y=contamination, fill=contam)) + 
  geom_bar(stat="identity", width=0.5, color="black")+
  scale_color_viridis(discrete = TRUE, option = "D")+
  scale_fill_viridis(discrete = TRUE) +
  scale_x_discrete(limits=normalised_PCB_oct[which(normalised_PCB_oct$year=="2018"),]$sample_TAG[1:length(sed_contam_oct[which(sed_contam_oct$year=="2018"),]$echantillon)]) +
  theme_bw() +
  
  geom_vline(xintercept=3+.5) +
  geom_vline(xintercept=3+3+.5) + 
  annotate(size=3,"text", x=3/2+.5, y=105, label="EM")+
  annotate(size=3,"text", x=3+3/2+.5, y=105, label="FN")+
  annotate(size=3,"text", x=3+3+2/2+.5, y=105, label="FS")+
  theme_bw() +
  #theme(legend.position="none") +
  theme(axis.text.x = element_blank(),title = element_text(size=12))+
  #theme(axis.text.x=element_text(size=9, angle=45, hjust=1), legend.position="bottom") +
  guides(size=F, fill=guide_legend(ncol=1)) +
  theme(plot.margin = margin(0.2,1.3,0.2,0.2,"cm"), legend.text = element_text(size=12))+
  labs(title="Octobre 2018", x=NULL, y=NULL, fill="PCB")



b4 = ggplot(normalised_PFAS_juin, aes(x=sample_TAG, y=contamination, fill=contam)) + 
  geom_bar(stat="identity", width=0.5, color="black")+
  scale_color_manual(values = PALETTE_PFAS_sed)+
  scale_fill_manual(values = PALETTE_PFAS_sed, labels = PFAS_sed_lab)+
  #scale_color_viridis(discrete = TRUE, option = "D")+
 # scale_fill_viridis(discrete = TRUE) +
  scale_x_discrete(limits=normalised_PFAS_juin$sample_TAG[1:length(sed_contam_juin$echantillon)][TAG_juin]) +
  #scale_x_discrete(labels=sed_contam_juin$species)+
  geom_vline(xintercept=3+.5) +
  geom_vline(xintercept=3+3+.5) + 
  annotate(size=3,"text", x=3/2+.5, y=100, label="")+
  # annotate(size=3,"text", x=3+3/2+.5, y=105, label="FN")+
  # annotate(size=3,"text", x=3+3+2/2+.5, y=105, label="FS")+
  theme_bw() +
  theme(legend.position="none") +
  theme(axis.text.x = element_blank())+
  #theme(axis.text.x=element_text(size=9, angle=45, hjust=1), legend.position="bottom") +
  #guides(col=guide_legend(nrow=2)) +
  labs(title=NULL, x=NULL, y=expression(paste("% de la ",sum()," des PFAS")))


b5 = ggplot(normalised_PFAS_oct[which(normalised_PFAS_oct$year=="2017"),],
            aes(x=sample_TAG, y=contamination, fill=contam)) + 
  geom_bar(stat="identity", width=0.5, color="black")+
  scale_color_manual(values = PALETTE_PFAS_sed)+
  scale_fill_manual(values = PALETTE_PFAS_sed)+
  #scale_color_viridis(discrete = TRUE, option = "D")+
  # scale_fill_viridis(discrete = TRUE) +
  scale_x_discrete(limits=normalised_PFAS_oct[which(normalised_PFAS_oct$year=="2017"),]$sample_TAG[1:length(sed_contam_oct[which(sed_contam_oct$year=="2017"),]$echantillon)]) +
  theme_bw() +
  
  geom_vline(xintercept=3+.5) +
  geom_vline(xintercept=3+3+.5) + 
  annotate(size=3,"text", x=3/2+.5, y=100, label="")+
  # annotate(size=3,"text", x=3+3/2+.5, y=100, label="FN")+
  # annotate(size=3,"text", x=3+3+2/2+.5, y=100, label="FS")+
  
  theme(legend.position="none") +
  theme(axis.text.x = element_blank(),title = element_text(size=12))+
  guides(size=F) +
  theme(legend.text = element_text(size=12))+
  labs(title=NULL, x=NULL, y=NULL, fill="PFAS")


b6=ggplot(normalised_PFAS_oct[which(normalised_PFAS_oct$year=="2018"),],
          aes(x=sample_TAG, y=contamination, fill=contam)) + 
  geom_bar(stat="identity", width=0.5, color="black")+
  scale_color_manual(values = PALETTE_PFAS_sed)+
  scale_fill_manual(values = PALETTE_PFAS_sed)+
  #scale_color_viridis(discrete = TRUE, option = "D")+
  # scale_fill_viridis(discrete = TRUE) +
  scale_x_discrete(limits=normalised_PFAS_oct[which(normalised_PFAS_oct$year=="2018"),]$sample_TAG[1:length(sed_contam_oct[which(sed_contam_oct$year=="2018"),]$echantillon)]) +
  theme_bw() +
  
  geom_vline(xintercept=3+.5) +
  geom_vline(xintercept=3+3+.5) + 
  annotate(size=3,"text", x=3/2+.5, y=100, label="")+
  # annotate(size=3,"text", x=3+3/2+.5, y=100, label="FN")+
  # annotate(size=3,"text", x=3+3+2/2+.5, y=100, label="FS")+
  theme_bw() +
  #theme(legend.position="none") +
  theme(axis.text.x = element_blank(),title = element_text(size=12))+
  #theme(axis.text.x=element_text(size=9, angle=45, hjust=1), legend.position="bottom") +
  guides(size=F, fill=guide_legend(ncol=1)) +
  theme(plot.margin = margin(0.2,0.2,0.2,0.2,"cm"), legend.text = element_text(size=12))+
  labs(title=NULL, x=NULL, y=NULL, fill="PFAS")



b7 = ggplot(normalised_HBCDD_juin, aes(x=sample_TAG, y=contamination, fill=contam)) + 
  geom_bar(stat="identity", width=0.5, color="black")+
  scale_color_viridis(discrete = TRUE, option = "D")+
  scale_fill_viridis(discrete = TRUE) +
  scale_x_discrete(limits=normalised_HBCDD_juin$sample_TAG[1:length(sed_contam_juin$echantillon)][TAG_juin],
                   labels=normalised_HBCDD_juin$labels[1:length(sed_contam_juin$echantillon)][TAG_juin]) +
  #scale_x_discrete(labels=sed_contam_juin$species)+
  geom_vline(xintercept=3+.5) +
  geom_vline(xintercept=3+3+.5) + 
  annotate(size=3,"text", x=3/2+.5, y=100, label="")+
  # annotate(size=3,"text", x=3+3/2+.5, y=105, label="FN")+
  # annotate(size=3,"text", x=3+3+2/2+.5, y=105, label="FS")+
  theme_bw() +
  theme(legend.position="none", plot.margin = margin(0.2,0.2,0.8,0.2,"cm")) +
  theme(axis.text.x = element_blank())+
  theme(axis.text.x=element_text(size=12, angle=90,hjust=1)) +
  #guides(col=guide_legend(nrow=2)) +
  labs(title=NULL, x=NULL, y=expression(paste("% de la ", sum()," des HBCDD")))


b8 = ggplot(normalised_HBCDD_oct[which(normalised_HBCDD_oct$year=="2017"),],
            aes(x=sample_TAG, y=contamination, fill=contam)) + 
  geom_bar(stat="identity", width=0.5, color="black")+
  scale_color_viridis(discrete = TRUE, option = "D")+
  scale_fill_viridis(discrete = TRUE) +
  scale_x_discrete(limits=normalised_HBCDD_oct[which(normalised_HBCDD_oct$year=="2017"),]$sample_TAG[1:length(sed_contam_oct[which(sed_contam_oct$year=="2017"),]$echantillon)],
                   labels=normalised_HBCDD_oct[which(normalised_HBCDD_oct$year=="2017"),]$labels[1:length(sed_contam_oct[which(sed_contam_oct$year=="2017"),]$echantillon)]) +
  theme_bw() +
  
  geom_vline(xintercept=3+.5) +
  geom_vline(xintercept=3+3+.5) + 
  annotate(size=3,"text", x=3/2+.5, y=100, label="")+
  # annotate(size=3,"text", x=3+3/2+.5, y=100, label="FN")+
  # annotate(size=3,"text", x=3+3+2/2+.5, y=100, label="FS")+
  
  theme(legend.position="none") +
  theme(title = element_text(size=12))+
  guides(size=F) +
  theme(axis.text.x=element_text(size=12, angle=90,hjust=1),
        plot.margin = margin(0.2,0.2,0.6,0.2,"cm"),
        legend.text = element_text(size=12))+
  labs(title=NULL, x=NULL, y=NULL, fill="HBCDD")


b9=ggplot(normalised_HBCDD_oct[which(normalised_HBCDD_oct$year=="2018"),],
          aes(x=sample_TAG, y=contamination, fill=contam)) + 
  geom_bar(stat="identity", width=0.5, color="black")+
  scale_color_viridis(discrete = TRUE, option = "D")+
  scale_fill_viridis(discrete = TRUE) +
  scale_x_discrete(limits=normalised_HBCDD_oct[which(normalised_HBCDD_oct$year=="2018"),]$sample_TAG[1:length(sed_contam_oct[which(sed_contam_oct$year=="2018"),]$echantillon)],
                   labels=normalised_HBCDD_oct[which(normalised_HBCDD_oct$year=="2018"),]$labels[1:length(sed_contam_oct[which(sed_contam_oct$year=="2018"),]$echantillon)]) +
  theme_bw() +
  
  geom_vline(xintercept=3+.5) +
  geom_vline(xintercept=3+3+.5) + 
  annotate(size=3,"text", x=3/2+.5, y=100, label="")+
  # annotate(size=3,"text", x=3+3/2+.5, y=100, label="FN")+
  # annotate(size=3,"text", x=3+3+2/2+.5, y=100, label="FS")+
  theme_bw() +
  #theme(legend.position="none") +
  theme(title = element_text(size=12))+
  #theme(axis.text.x=element_text(size=9, angle=45, hjust=1), legend.position="bottom") +
  guides(size=F, fill=guide_legend(ncol=1)) +
  theme(axis.text.x=element_text(size=12, angle=90,hjust=1),
        plot.margin = margin(0.2,0.7,0.2,0.2,"cm"), legend.text = element_text(size=12))+
  labs(title=NULL, x=NULL, y=NULL, fill="HBCDD")



a = 7
b = 7
c = 6
z=a+b+c

grid.arrange(b1,b2,b3,b4,b5,b6,b7,b8,b9,
             ncol=7,nrow=z,
             layout_matrix = rbind(t(matrix(rep.int(c(1,1,2,2,3,3,3),times = a),ncol = a, nrow=7)),
                                   t(matrix(rep.int(c(4,4,5,5,6,6,6),times = b),ncol = b, nrow=7)),
                                   t(matrix(rep.int(c(7,7,8,8,9,9,9),times = c),ncol = c, nrow=7)))
)

dev.off()



