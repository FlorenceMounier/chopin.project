####################################
##         PROJET CHOPIN
## 
##          GRAPHIQUES
##     ISOTOPES - BENTHOS
##      
####################################


benthos_isotopes = read.xlsx(file = "DATA/CHOPIN_BASE_DE_DONNEES_GENERALE.xlsx",
                             sheetName= "isotopes_benth_CAPES", h=T, dec=",")
benthos_isotopes$species = as.character(benthos_isotopes$espece)
benthos_isotopes$species[which(benthos_isotopes$espece=="Nephtys"|
                                 benthos_isotopes$espece=="Nephtys_assimilis"|
                                 benthos_isotopes$espece=="Nephtys_caeca"|
                                 benthos_isotopes$espece=="Nephtys_cirrosa"|
                                 benthos_isotopes$espece=="Nephtys_hombergii")] = "Nephtys_sp"
benthos_isotopes$species[which(benthos_isotopes$species=="Limecola_baltica")] = "Limecola_balthica"
benthos_isotopes$species = as.factor(benthos_isotopes$species)

benthos_isotopes$labels = rep.int(NA, times = dim(benthos_isotopes)[1])
benthos_isotopes$grp = rep.int(NA, times = dim(benthos_isotopes)[1])
for(l in 1:3){
  for(s in 1:length(taxons[[l]])){
    benthos_isotopes$labels[which(as.character(benthos_isotopes$species) == as.character(labels[[l]][s]))] = names(labels[[l]])[s]
    benthos_isotopes$grp[which(as.character(benthos_isotopes$species) == as.character(taxons[[l]][s]))] = names(taxons)[l]
  }
}


isotopes_CAPES = data.frame("species"=benthos_isotopes$species,
                            "d13C" = benthos_isotopes$d13C,
                            "d15N" = benthos_isotopes$d15N,
                            "sample_TAG" = benthos_isotopes$CODE_ECHANTILLON,
                            "zone" = benthos_isotopes$Secteur.zone,
                            "season" = factor(benthos_isotopes$Campagne,labels = c("Automne","Printemps")),
                            "grp" = benthos_isotopes$grp,
                            "labels" = benthos_isotopes$labels,
                            "source" = rep.int("CAPES", times = dim(benthos_isotopes)[1]))

isotopes_CHOPIN = data.frame("species"=benthos_contam$species[-which(is.na(benthos_contam$d.13C.12C.â...)==T)],
                             "d13C"= benthos_contam$d.13C.12C.â...[-which(is.na(benthos_contam$d.13C.12C.â...)==T)],
                             "d15N" = benthos_contam$d.15N.14N.â...[-which(is.na(benthos_contam$d.13C.12C.â...)==T)],
                             "sample_TAG" = benthos_contam$sample_TAG[-which(is.na(benthos_contam$d.13C.12C.â...)==T)],
                             "zone" = benthos_contam$zone[-which(is.na(benthos_contam$d.13C.12C.â...)==T)],
                             "season" = benthos_contam$season[-which(is.na(benthos_contam$d.13C.12C.â...)==T)],
                             "grp" = benthos_contam$grp[-which(is.na(benthos_contam$d.13C.12C.â...)==T)],
                             "labels" = benthos_contam$labels[-which(is.na(benthos_contam$d.13C.12C.â...)==T)],
                             "source" = rep.int("CHOPIN", times = length(benthos_contam$labels[-which(is.na(benthos_contam$d.13C.12C.â...)==T)])))

ISOTOPES = rbind(isotopes_CAPES,isotopes_CHOPIN)

#write.xlsx2(ISOTOPES, "Isotopes_benthos_CAPES_CHOPIN.xlsx", row.names = F)

ISOTOPES$md13C = -ISOTOPES$d13C

ISOTOPES_juin = ISOTOPES[which(ISOTOPES$season=="Printemps"),]
ISOTOPES_juin = ISOTOPES_juin[order(ISOTOPES_juin$zone),]
summary(ISOTOPES_juin$zone)
ISOTOPES_oct = ISOTOPES[which(ISOTOPES$season=="Automne"),]
ISOTOPES_oct = ISOTOPES_oct[order(ISOTOPES_oct$zone),]
summary(ISOTOPES_oct$zone)


res_isotope = data.frame("species"=rownames(tapply(ISOTOPES$d13C,INDEX = ISOTOPES$species, FUN = median)),
                         "med_d13C"=-tapply(ISOTOPES$d13C,INDEX = ISOTOPES$species, FUN = median),
                         "min_d13C"=-tapply(ISOTOPES$d13C,INDEX = ISOTOPES$species, FUN = min),
                         "max_d13C"=-tapply(ISOTOPES$d13C,INDEX = ISOTOPES$species, FUN = max),
                         "med_d15N"=tapply(ISOTOPES$d15N,INDEX = ISOTOPES$species, FUN = median),
                         "min_d15N"=tapply(ISOTOPES$d15N,INDEX = ISOTOPES$species, FUN = min),
                         "max_d15N"=tapply(ISOTOPES$d15N,INDEX = ISOTOPES$species, FUN = max))


res_isotope$labels = rep.int(NA, times = dim(res_isotope)[1])
res_isotope$grp = rep.int(NA, times = dim(res_isotope)[1])
for(l in 1:3){
  for(s in 1:length(taxons[[l]])){
    res_isotope$labels[which(as.character(res_isotope$species) == as.character(labels[[l]][s]))] = names(labels[[l]])[s]
    res_isotope$grp[which(as.character(res_isotope$species) == as.character(taxons[[l]][s]))] = names(taxons)[l]
  }
}

##################################################################################################


test_effect(dataset = benthos_isotopes, variable = "d13C", factor = "Secteur.zone")
test_effect(dataset = benthos_isotopes, variable = "d13C", factor = "grp")


##################################################################################################


png("Output/BENTHOS/isotopes_benthos_d13C_especes.jpeg",width = 500, height = 300, units = "px")
ggplot(res_isotope, aes(x=species, y=med_d13C, fill=grp))+ 
  geom_bar(stat="identity", position=position_dodge()) +
  theme_bw() +
  scale_x_discrete(limits=res_isotope$species[order(res_isotope$grp)],
                   labels = res_isotope$labels[order(res_isotope$grp)]) +
  theme(axis.text.x = element_blank())+
  theme(axis.text.x = element_text(size=10, angle=90,vjust=0.4, hjust=1))+
  geom_errorbar(aes(ymin=min_d13C, ymax=max_d13C), width=.2,
                position=position_dodge(.9))+
  geom_text(x=seq(1,16,by=1),
            label=c("EMB","FS","EMB","FS","FSN","EMB","FN","EMB","","ALL","FN","FN","FSEm","FS","FSEm","EMB"), 
            y=0, color="black", size=3.5)+
  labs(title="Delta 13C par esp?ce du benthos", x=NULL, y=expression(paste("-d13C")))
dev.off()

png("Output/BENTHOS/isotopes_benthos_d15N_especes.jpeg",width = 500, height = 300, units = "px")
ggplot(res_isotope, aes(x=species, y=med_d15N, fill=grp))+ 
  geom_bar(stat="identity", position=position_dodge()) +
  theme_bw() +
  scale_x_discrete(limits=res_isotope$species[order(res_isotope$grp)],
                   labels = res_isotope$labels[order(res_isotope$grp)]) +
  theme(axis.text.x = element_blank())+
  theme(axis.text.x = element_text(size=10, angle=90,vjust=0.4, hjust=1))+
  geom_errorbar(aes(ymin=min_d15N, ymax=max_d15N), width=.2,
                position=position_dodge(.9))+
  geom_text(x=seq(1,16,by=1),
            label=c("EMB","FS","EMB","FS","FSN","EMB","FN","EMB","","ALL","FN","FN","FSEm","FS","FSEm","EMB"), 
            y=0, color="black", size=3.5)+
  labs(title="Delta 15N par esp?ce du benthos", x=NULL, y=expression(paste("d15N")))
dev.off()



##################################################################################################


png("Output/BENTHOS/isotopes_benthos.jpeg",width = 1100, height = 600, units = "px")


b1 = ggplot(ISOTOPES_juin, aes(x=sample_TAG, y=md13C, color=grp, fill=grp)) + 
  geom_bar(stat="identity", width=0.5)+
  scale_x_discrete(limits=ISOTOPES_juin$sample_TAG[order(ISOTOPES_juin$zone)]) +
  #scale_x_discrete(labels=ISOTOPES$species)+
  geom_vline(xintercept=20.5) +
  annotate(size=3,"text", x=20/2+.5, y=23, label="Embouchure")+
  geom_vline(xintercept=20+12.5) +
  annotate(size=3,"text", x=20+(12/2)+.5, y=23, label="Fosse Nord")+
  geom_vline(xintercept=20+12+18.5) + 
  annotate(size=3,"text", x=(20+12)+(18/2)+.5, y=23, label="Fosse Sud")+
  theme_bw() +
  theme(legend.position="none") +
  theme(axis.text.x = element_blank())+
  #theme(axis.text.x=element_text(size=9, angle=45, hjust=1), legend.position="bottom") +
  #guides(col=guide_legend(nrow=2)) +
  labs(title="Juin 2017", x=NULL, y=expression(paste("-d13C")))


b2 = ggplot(ISOTOPES_oct, aes(x=sample_TAG, y=md13C, color=grp, fill=grp)) + 
  geom_bar(stat="identity", width=0.5)+
  scale_x_discrete(limits=ISOTOPES_oct$sample_TAG[order(ISOTOPES_oct$zone)]) +
  geom_vline(xintercept=28.5) +
  annotate(size=3,"text", x=28/2+.5, y=23, label="Embouchure")+
  geom_vline(xintercept=28+6.5) +
  annotate(size=3,"text", x=28+(6/2)+.5, y=23, label="Fosse Nord")+
  geom_vline(xintercept=28+6+25.5) + 
  annotate(size=3,"text", x=(28+6)+(25/2)+.5, y=23, label="Fosse Sud")+
  annotate(size=3,"text", x=(28+6+25)+0.5, y=23, label="Chenal", angle=90)+
  theme_bw() +
  theme(legend.position="none") +
  theme(axis.text.x = element_blank())+
  #scale_x_discrete(labels=ISOTOPES_oct$species)+
  #theme(axis.text.x=element_text(size=9, angle=45, hjust=1), legend.position="bottom") +
  #guides(col=guide_legend(nrow=2)) +
  labs(title=expression(paste("Octobre 2017")), x=NULL, y=NULL)


b3 = ggplot(ISOTOPES_juin, aes(x=sample_TAG, y=d15N, color=grp, fill=grp)) + 
  geom_bar(stat="identity", width=0.5)+
  scale_x_discrete(limits=ISOTOPES_juin$sample_TAG[order(ISOTOPES_juin$zone)],
                   labels = ISOTOPES_juin$labels[order(ISOTOPES_juin$zone)]) +
  geom_vline(xintercept=20.5) +
  annotate(size=3,"text", x=20/2+.5, y=17, label="Embouchure")+
  geom_vline(xintercept=20+12.5) +
  annotate(size=3,"text", x=20+(12/2)+.5, y=17, label="Fosse Nord")+
  geom_vline(xintercept=20+12+18.5) + 
  annotate(size=3,"text", x=(20+12)+(18/2)+.5, y=17, label="Fosse Sud")+
  theme_bw() +
  #scale_x_discrete(labels=ISOTOPES$species)+
  theme(legend.position="none") +
  theme(axis.text.x = element_text(size=10, angle=90,vjust=0.4, hjust=1))+
  #theme(axis.text.x=element_text(size=9, angle=45, hjust=1), legend.position="bottom") +
  #guides(col=guide_legend(nrow=2)) +
  labs(title=NULL, x=NULL, y=expression(paste("d15N")))

b4 = ggplot(ISOTOPES_oct, aes(x=sample_TAG, y=d15N, color=grp, fill=grp)) + 
  geom_bar(stat="identity", width=0.5)+
  scale_x_discrete(limits=ISOTOPES_oct$sample_TAG[order(ISOTOPES_oct$zone)],
                   labels = ISOTOPES_oct$labels[order(ISOTOPES_oct$zone)]) +
  geom_vline(xintercept=28.5) +
  annotate(size=3,"text", x=28/2+.5, y=17, label="Embouchure")+
  geom_vline(xintercept=28+6.5) +
  annotate(size=3,"text", x=28+(6/2)+.5, y=17, label="Fosse Nord")+
  geom_vline(xintercept=28+6+25.5) + 
  annotate(size=3,"text", x=(28+6)+(25/2)+.5, y=17, label="Fosse Sud")+
  annotate(size=3,"text", x=(28+6+25)+0.5, y=17, label="Chenal", angle=90)+
  theme_bw() +
  theme(legend.position="none") +
  theme(axis.text.x = element_text(size=10, angle=90,vjust=0.4, hjust=1))+
  #scale_x_discrete(labels=ISOTOPES_oct$species)+
  #theme(axis.text.x=element_text(size=9, angle=45, hjust=1), legend.position="bottom") +
  #guides(col=guide_legend(nrow=2)) +
  labs(title=NULL, x=NULL, y=NULL)

a = 6
b = 5
z=a+b

grid.arrange(b1,b2,b3,b4,
             ncol=2,nrow=z,
             layout_matrix = rbind(t(matrix(rep.int(c(1,2),times = a),ncol = a, nrow=2)),
                                   t(matrix(rep.int(c(3,4),times = b),ncol = b, nrow=2)))
)

dev.off()

##################################################################################################


p1 = ggplot(ISOTOPES, aes(x=grp, y=d13C, color=grp, fill=grp)) + 
  geom_dotplot(binaxis='y', stackdir='center')+
  theme_bw() +
  theme(legend.position="none") +
  theme(axis.title.y = element_text(size=6), title = element_text(size=8))+
  #theme(axis.text.x=element_text(size=9, angle=45, hjust=1), legend.position="bottom") +
  stat_summary(fun = mean, geom="crossbar", size=0.5, color="darkgrey", width=.5) +
  stat_summary(fun = median, geom="crossbar", size=0.5, color="black", width=.5) +
  #guides(col=guide_legend(nrow=2)) +
  labs(title=paste("d13C - TAXON"), x=NULL,y=NULL)

p2 = ggplot(ISOTOPES, aes(x=zone, y=d13C, color=zone, fill=zone))+
  geom_dotplot(binaxis='y', stackdir='center')+
  theme_bw() +
  theme(legend.position="none") +
  theme(axis.text.y = element_blank(), title = element_text(size=8))+
  #theme(axis.text.x=element_text(size=9, angle=45, hjust=1), legend.position="bottom") +
  stat_summary(fun = mean, geom="crossbar", size=0.5, color="darkgrey", width=.5) +
  stat_summary(fun = median, geom="crossbar", size=0.5, color="black", width=.5) +
  #guides(col=guide_legend(nrow=2)) +
  labs(title=paste("d13C - ZONE"), x=NULL, y=NULL)

p3 = ggplot(ISOTOPES, aes(x=season, y=d13C, color=season, fill=season)) + 
  geom_dotplot(binaxis='y', stackdir='center')+
  theme_bw() +
  theme(legend.position="none") +
  theme(axis.text.y = element_blank(), title = element_text(size=8))+
  #theme(axis.text.x=element_text(size=9, angle=45, hjust=1), legend.position="bottom") +
  stat_summary(fun = mean, geom="crossbar", size=0.5, color="darkgrey", width=.5) +
  stat_summary(fun = median, geom="crossbar", size=0.5, color="black", width=.5) +
  #guides(col=guide_legend(nrow=2)) +
  labs(title=paste("d13C - SAISON"), x=NULL, y=NULL)


plot = ggdraw()+
  draw_plot(p1, x=0, y=0, width=0.35, height=1) +
  draw_plot(p2, x=0.35,y=0, 0.45, 1) +
  draw_plot(p3, x=0.8,y=0, 0.2, 1)

ggsave(filename = "Output/BENTHOS/benthos_d13C.jpeg",
       plot = plot,device = "jpeg",width = 20,height = 7, units = "cm",dpi=300)



##################################################################################################



p1 = ggplot(ISOTOPES, aes(x=grp, y=d15N, color=grp, fill=grp)) + 
  geom_dotplot(binaxis='y', stackdir='center')+
  theme_bw() +
  theme(legend.position="none") +
  theme(axis.title.y = element_text(size=6), title = element_text(size=8))+
  #theme(axis.text.x=element_text(size=9, angle=45, hjust=1), legend.position="bottom") +
  stat_summary(fun = mean, geom="crossbar", size=0.5, color="darkgrey", width=.5) +
  stat_summary(fun = median, geom="crossbar", size=0.5, color="black", width=.5) +
  #guides(col=guide_legend(nrow=2)) +
  labs(title=paste("d15N - TAXON"), x=NULL,y=NULL)

p2 = ggplot(ISOTOPES, aes(x=zone, y=d15N, color=zone, fill=zone))+
  geom_dotplot(binaxis='y', stackdir='center')+
  theme_bw() +
  theme(legend.position="none") +
  theme(axis.text.y = element_blank(), title = element_text(size=8))+
  #theme(axis.text.x=element_text(size=9, angle=45, hjust=1), legend.position="bottom") +
  stat_summary(fun = mean, geom="crossbar", size=0.5, color="darkgrey", width=.5) +
  stat_summary(fun = median, geom="crossbar", size=0.5, color="black", width=.5) +
  #guides(col=guide_legend(nrow=2)) +
  labs(title=paste("d15N - ZONE"), x=NULL, y=NULL)

p3 = ggplot(ISOTOPES, aes(x=season, y=d15N, color=season, fill=season)) + 
  geom_dotplot(binaxis='y', stackdir='center')+
  theme_bw() +
  theme(legend.position="none") +
  theme(axis.text.y = element_blank(), title = element_text(size=8))+
  #theme(axis.text.x=element_text(size=9, angle=45, hjust=1), legend.position="bottom") +
  stat_summary(fun = mean, geom="crossbar", size=0.5, color="darkgrey", width=.5) +
  stat_summary(fun = median, geom="crossbar", size=0.5, color="black", width=.5) +
  #guides(col=guide_legend(nrow=2)) +
  labs(title=paste("d15N - SAISON"), x=NULL, y=NULL)


plot = ggdraw()+
  draw_plot(p1, x=0, y=0, width=0.35, height=1) +
  draw_plot(p2, x=0.35,y=0, 0.45, 1) +
  draw_plot(p3, x=0.8,y=0, 0.2, 1)

ggsave(filename = "Output/BENTHOS/benthos_d15N.jpeg",
       plot = plot,device = "jpeg",width = 20,height = 7, units = "cm",dpi=300)
