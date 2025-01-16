####################################
##         PROJET CHOPIN
## 
##            GRAPH
##           SOLES
##    SOMMES par ZONE & CLASSE
##      
#################################### 

variable = 

temp = data.frame(c(c("Bivalves"=NA,tapply(X = soles_contam[soles_contam$zone=="Chenal",variable],
                                           INDEX = soles_contam[soles_contam$zone=="Chenal",]$grp, FUN=median, na.rm=T),
                      "Polychetes"=NA),
                    
                    tapply(X = soles_contam[soles_contam$zone=="Embouchure",variable],
                           INDEX = soles_contam[soles_contam$zone=="Embouchure",]$grp, FUN=median, na.rm=T),
                    
                    tapply(X = soles_contam[soles_contam$zone=="Fosse Nord",variable],
                           INDEX = soles_contam[soles_contam$zone=="Fosse Nord",]$grp, FUN=median, na.rm=T),
                    
                    tapply(X = soles_contam[soles_contam$zone=="Fosse Sud",variable],
                           INDEX = soles_contam[soles_contam$zone=="Fosse Sud",]$grp, FUN=median, na.rm=T)
))



soles_contam_chenal = soles_contam[soles_contam$zone=="Chenal",]
soles_contam_emb = soles_contam[soles_contam$zone=="Embouchure",]
soles_contam_FN = soles_contam[soles_contam$zone=="Fosse Nord",]
soles_contam_FS = soles_contam[soles_contam$zone=="Fosse Sud",]

GRAPH_soles_group_zone = function(variable,label_y){
  
  p1 = ggplot(soles_contam_chenal, aes(x=grp, y=soles_contam_chenal[,variable])) + 
    geom_dotplot(binaxis='y', stackdir='center', dotsize=0.7, color = "#01B837", fill = "#01B837")+
    theme_bw() +
    theme(legend.position="none") +
    ylim(min(soles_contam[,variable], na.rm=T), max(soles_contam[,variable], na.rm=T)) +
    #theme(axis.text.x=element_text(size=9, angle=45, hjust=1), legend.position="bottom") +
    stat_summary(fun = mean, geom="crossbar", size=0.5, color="darkgrey", width=.5) +
    stat_summary(fun = median, geom="crossbar", size=0.5, color="black", width=.5) +
    #guides(col=guide_legend(nrow=2)) +
    labs(title="CHENAL", x=NULL, y=label_y)
  
  p2 = ggplot(soles_contam_emb, aes(x=grp, y=soles_contam_emb[,variable], color=grp, fill=grp)) + 
    geom_dotplot(binaxis='y', stackdir='center', dotsize=0.7)+
    theme_bw() +
    theme(legend.position="none") +
    ylim(min(soles_contam[,variable], na.rm=T), max(soles_contam[,variable], na.rm=T)) +
    #theme(axis.text.x=element_text(size=9, angle=45, hjust=1), legend.position="bottom") +
    stat_summary(fun = mean, geom="crossbar", size=0.5, color="darkgrey", width=.5) +
    stat_summary(fun = median, geom="crossbar", size=0.5, color="black", width=.5) +
    #guides(col=guide_legend(nrow=2)) +
    labs(title="EMBOUCHURE", x=NULL, y=NULL)
  
  
  p3 = ggplot(soles_contam_FN, aes(x=grp, y=soles_contam_FN[,variable], color=grp, fill=grp)) + 
    geom_dotplot(binaxis='y', stackdir='center', dotsize=0.7)+
    theme_bw() +
    theme(legend.position="none") +
    ylim(min(soles_contam[,variable], na.rm=T), max(soles_contam[,variable], na.rm=T)) +
    #theme(axis.text.x=element_text(size=9, angle=45, hjust=1), legend.position="bottom") +
    stat_summary(fun = mean, geom="crossbar", size=0.5, color="darkgrey", width=.5) +
    stat_summary(fun = median, geom="crossbar", size=0.5, color="black", width=.5) +
    #guides(col=guide_legend(nrow=2)) +
    labs(title="FOSSE NORD", x=NULL, y=NULL)
  
  
  p4 = ggplot(soles_contam_FS, aes(x=grp, y=soles_contam_FS[,variable], color=grp, fill=grp)) + 
    geom_dotplot(binaxis='y', stackdir='center', dotsize=0.7)+
    theme_bw() +
    theme(legend.position="none") +
    ylim(min(soles_contam[,variable], na.rm=T), max(soles_contam[,variable], na.rm=T)) +
    #theme(axis.text.x=element_text(size=9, angle=45, hjust=1), legend.position="bottom") +
    stat_summary(fun = mean, geom="crossbar", size=0.5, color="darkgrey", width=.5) +
    stat_summary(fun = median, geom="crossbar", size=0.5, color="black", width=.5) +
    #guides(col=guide_legend(nrow=2)) +
    labs(title="FOSSE SUD", x=NULL, y=NULL)
  
  #list("plot_taxon"=p1,"plot_zone"=p2,"plot_saison"=p3)
  plot=ggdraw()+
    draw_plot(p1, x=0, y=0, width=0.22, height=1) +
    draw_plot(p2, x=0.22,y=0, 0.26, 1) +
    draw_plot(p3, x=0.48,y=0, 0.26, 1) +
    draw_plot(p4, x=0.74,y=0, 0.26, 1)
  
}

plot1 = GRAPH_soles_group_zone(variable = "sommePCB_ng_gdw",label_y = expression(paste(sum(),"PCB")))
plot2 = GRAPH_soles_group_zone(variable = "sommePFAS_ng_gdw",label_y = expression(paste(sum(),"PFAS")))
plot3 = GRAPH_soles_group_zone(variable = "sommeHBCDD_ng_gdw",label_y = expression(paste(sum(),"HBCDD")))


plot=ggdraw()+
  draw_plot(plot3, x=0,y=0.02, 1, 0.35) +
  draw_plot(plot2, x=0,y=0.33, 1, 0.35) + 
  draw_plot(plot1, x=0, y=0.64, width=1, height=0.35)

plot

ggsave(filename = "Output/SOLES/soles_group_zone.jpeg",
       plot = plot,
       device = "jpeg",width = 20,height = 15, units = "cm",dpi=300)




############################################################


FUN_soles_median_zone = function(variable){
  
  bCH = tapply(X = soles_contam_chenal[,variable],
               INDEX = soles_contam_chenal[,]$grp, FUN=median, na.rm=T)
  bEM = tapply(X = soles_contam_emb[,variable],
               INDEX = soles_contam_emb[,]$grp, FUN=median, na.rm=T)
  bFN = tapply(X = soles_contam_FN[,variable],
               INDEX = soles_contam_FN[,]$grp, FUN=median, na.rm=T)
  bFS = tapply(X = soles_contam_FS[,variable],
               INDEX = soles_contam_FS[,]$grp, FUN=median, na.rm=T)
  
  c(bCH,bEM,bFN,bFS)
}



soles_median_zone = data.frame("zone"=rep.int(levels(soles_contam$zone),times = 3),
                                 "sommePCB_ng_gdw"=FUN_soles_median_zone(variable = "sommePCB_ng_gdw"),
                                 "sommePFAS_ng_gdw"=FUN_soles_median_zone(variable = "sommePFAS_ng_gdw"),
                                 "sommeHBCDD_ng_gdw"=FUN_soles_median_zone(variable = "sommeHBCDD_ng_gdw"),
                                 "grp"=rep.int(c("G0","G1","G2"),times = 4))

p1 = ggplot(soles_median_zone, aes(x=zone, y=sommePCB_ng_gdw)) + 
  geom_bar(aes(fill=grp),colour="black",  stat="identity", width=0.5,
           position=position_dodge())+
  theme_bw() +
  theme(legend.position="none") +
  scale_x_discrete(labels=c("CH","EMB","FN","FS"))+
  labs(title="PCB", x=NULL, y=expression(paste(sum()," par famille (ng.",g^-1,"ps)")))


p2 = ggplot(soles_median_zone, aes(x=zone, y=sommePFAS_ng_gdw)) + 
  geom_bar(aes(fill=grp),colour="black",  stat="identity", width=0.5,
           position=position_dodge())+
  theme_bw() +
  theme(legend.position="none") + 
  scale_x_discrete(labels=c("CH","EMB","FN","FS"))+
  labs(title="PFAS", x=NULL, y=NULL)


p3 = ggplot(soles_median_zone, aes(x=zone, y=sommeHBCDD_ng_gdw)) + 
  geom_bar(aes(fill=grp),colour="black",  stat="identity", width=0.5,
           position=position_dodge())+
  theme_bw() +
  theme(legend.position="none") +
  scale_x_discrete(labels=c("CH","EMB","FN","FS"))+
  labs(title="HBCDD", x=NULL, y=NULL)


#list("plot_taxon"=p1,"plot_zone"=p2,"plot_saison"=p3)
plot=ggdraw()+
  draw_plot(p3, x=0.66,y=0, 0.33, 1)+
  draw_plot(p2, x=0.33,y=0, 0.33, 1) +
  draw_plot(p1, x=0, y=0, width=0.33, height=1)


plot

ggsave(filename = "Output/SOLES/soles_group_zone_barplot.jpeg",
       plot = plot,
       device = "jpeg",width = 20,height = 5, units = "cm",dpi=300)
