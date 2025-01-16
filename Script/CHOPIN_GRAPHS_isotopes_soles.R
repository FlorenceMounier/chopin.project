####################################
##         PROJET CHOPIN
## 
##           ANALYSES
##           SOLES
##          ISOTOPES
##      
####################################




# --------------------------------------------------------------
# Suppression des composâs non analysâs

PFAS_lab = PFAS_ALL_lab[-which(PFAS_ALL=="MeFOSA"|PFAS_ALL=="EtFOSA")]
ss_famille = ss_famille_ALL[-which(PFAS_ALL=="MeFOSA"|PFAS_ALL=="EtFOSA")]
n_C = n_C_ALL[-which(PFAS_ALL=="MeFOSA"|PFAS_ALL=="EtFOSA")]
PFAS = PFAS_ALL[-which(PFAS_ALL=="MeFOSA"|PFAS_ALL=="EtFOSA")]



##########################################################################


CAPES_fish_isotopes = read.xlsx(file = "DATA/CHOPIN_BASE_DE_DONNEES_GENERALE.xlsx",
                                sheetName= "isotopes_fish_CAPES", h=T, dec=",")

soles_isotopes = CAPES_fish_isotopes[CAPES_fish_isotopes$espece=="SOLESOL",]
soles_isotopes$Secteur.zone = factor(soles_isotopes$Secteur.zone,
                                     levels = levels(soles_isotopes$Secteur.zone),
                                     labels = c("Chenal","Embouchure","Fosse Nord","Fosse Sud"))


ISOTOPES = data.frame("grp"=c(as.character(soles_isotopes$stade),
                              as.character(soles_contam$grp)),
                      "season" = c(as.character(soles_isotopes$Campagne),
                                   as.character(soles_contam$season)),
                      "zone"=c(as.character(soles_isotopes$Secteur.zone),
                               as.character(soles_contam$zone)),
                      "d13C"=c(soles_isotopes$d13C,soles_contam$d.13C.12C.â...),
                      "d15N"=c(soles_isotopes$d15N,soles_contam$d.15N.14N.â...))

ISOTOPES$zone = factor(ISOTOPES$zone,levels = levels(ISOTOPES$zone),
                       labels = c("CH","EMB","FN","FS"))

ISOTOPES_G0 = ISOTOPES[which(ISOTOPES$grp=="G0"),]
ISOTOPES_G1_G2 = ISOTOPES[which(ISOTOPES$grp=="G1"|ISOTOPES$grp=="G2"),]


#-----------------------------
#   d13C

p1 = ggplot(ISOTOPES, aes(x=grp, y=d13C, color=grp, fill=grp)) + 
  geom_dotplot(binaxis='y', stackdir='center')+
  theme_bw() +
  theme(legend.position="none") +
  theme(axis.title.y = element_text(size=6), title = element_text(size=8))+
  #theme(axis.text.x=element_text(size=9, angle=45, hjust=1), legend.position="bottom") +
  stat_summary(fun = mean, geom="crossbar", size=0.5, color="darkgrey", width=.5) +
  stat_summary(fun = median, geom="crossbar", size=0.5, color="black", width=.5) +
  #guides(col=guide_legend(nrow=2)) +
  labs(title=paste("d13C - CLASSE D'AGE"), x=NULL,y=NULL)

p2 = ggplot(ISOTOPES_G0, aes(x=zone, y=d13C, color=zone, fill=zone))+
  geom_dotplot(binaxis='y', stackdir='center')+
  theme_bw() +
  theme(legend.position="none") +
  theme(axis.text.y = element_blank(), title = element_text(size=8))+
  #theme(axis.text.x=element_text(size=9, angle=45, hjust=1), legend.position="bottom") +
  stat_summary(fun = mean, geom="crossbar", size=0.5, color="darkgrey", width=.5) +
  stat_summary(fun = median, geom="crossbar", size=0.5, color="black", width=.5) +
  #guides(col=guide_legend(nrow=2)) +
  labs(title=paste("d13C - ZONE G0"), x=NULL, y=NULL)

p3 = ggplot(ISOTOPES_G0, aes(x=season, y=d13C, color=season, fill=season)) + 
  geom_dotplot(binaxis='y', stackdir='center')+
  theme_bw() +
  theme(legend.position="none") +
  theme(axis.text.y = element_blank(), title = element_text(size=8))+
  #theme(axis.text.x=element_text(size=9, angle=45, hjust=1), legend.position="bottom") +
  stat_summary(fun = mean, geom="crossbar", size=0.5, color="darkgrey", width=.5) +
  stat_summary(fun = median, geom="crossbar", size=0.5, color="black", width=.5) +
  #guides(col=guide_legend(nrow=2)) +
  labs(title=paste("d13C - SAISON G0"), x=NULL, y=NULL)


plot = ggdraw()+
  draw_plot(p1, x=0, y=0, width=0.45, height=1) +
  draw_plot(p2, x=0.45,y=0, 0.3, 1) +
  draw_plot(p3, x=0.75,y=0, 0.25, 1)

ggsave(filename = "Output/SOLES/soles_d13C.jpeg",
       plot = plot,device = "jpeg",width = 20,height = 7, units = "cm",dpi=300)



###################################################################

p1 = ggplot(ISOTOPES, aes(x=grp, y=d15N, color=grp, fill=grp)) + 
  geom_dotplot(binaxis='y', stackdir='center')+
  theme_bw() +
  theme(legend.position="none") +
  theme(axis.title.y = element_text(size=6), title = element_text(size=8))+
  #theme(axis.text.x=element_text(size=9, angle=45, hjust=1), legend.position="bottom") +
  stat_summary(fun = mean, geom="crossbar", size=0.5, color="darkgrey", width=.5) +
  stat_summary(fun = median, geom="crossbar", size=0.5, color="black", width=.5) +
  #guides(col=guide_legend(nrow=2)) +
  labs(title=paste("d15N - CLASSE D'AGE"), x=NULL,y=NULL)

p2 = ggplot(ISOTOPES_G0, aes(x=zone, y=d15N, color=zone, fill=zone))+
  geom_dotplot(binaxis='y', stackdir='center')+
  theme_bw() +
  theme(legend.position="none") +
  theme(axis.text.y = element_blank(), title = element_text(size=8))+
  #theme(axis.text.x=element_text(size=9, angle=45, hjust=1), legend.position="bottom") +
  stat_summary(fun = mean, geom="crossbar", size=0.5, color="darkgrey", width=.5) +
  stat_summary(fun = median, geom="crossbar", size=0.5, color="black", width=.5) +
  #guides(col=guide_legend(nrow=2)) +
  labs(title=paste("d15N - ZONE G0"), x=NULL, y=NULL)

p3 = ggplot(ISOTOPES_G0, aes(x=season, y=d15N, color=season, fill=season)) + 
  geom_dotplot(binaxis='y', stackdir='center')+
  theme_bw() +
  theme(legend.position="none") +
  theme(axis.text.y = element_blank(), title = element_text(size=8))+
  #theme(axis.text.x=element_text(size=9, angle=45, hjust=1), legend.position="bottom") +
  stat_summary(fun = mean, geom="crossbar", size=0.5, color="darkgrey", width=.5) +
  stat_summary(fun = median, geom="crossbar", size=0.5, color="black", width=.5) +
  #guides(col=guide_legend(nrow=2)) +
  labs(title=paste("d15N - SAISON G0"), x=NULL, y=NULL)


plot = ggdraw()+
  draw_plot(p1, x=0, y=0, width=0.45, height=1) +
  draw_plot(p2, x=0.45,y=0, 0.3, 1) +
  draw_plot(p3, x=0.75,y=0, 0.25, 1)

ggsave(filename = "Output/SOLES/soles_d15N.jpeg",
       plot = plot,device = "jpeg",width = 20,height = 7, units = "cm",dpi=300)

################################################################################

test_effect(dataset = ISOTOPES, variable = "d13C", factor = "grp")
test_effect(dataset = ISOTOPES, variable = "d15N", factor = "grp")

test_effect(dataset = ISOTOPES_G0, variable = "d13C", factor = "zone")
test_effect(dataset = ISOTOPES_G0, variable = "d15N", factor = "zone")

test_effect(dataset = ISOTOPES_G0, variable = "d13C", factor = "season")
test_effect(dataset = ISOTOPES_G0, variable = "d15N", factor = "season")



