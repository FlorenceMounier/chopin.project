####################################
##         PROJET CHOPIN
## 
##       FONCTION GRAPHIQUE 
##       TAXON, ZONE, SAISON
####################################


GRAPH_group_zone_saison = function(data,label_data, variable,label_y, wd){
  
  p1 = ggplot(data, aes(x=grp, y=data[,variable], color=grp, fill=grp)) + 
    geom_dotplot(binaxis='y', stackdir='center')+
    theme_bw() +
    theme(legend.position="none") +
    #theme(axis.text.x=element_text(size=9, angle=45, hjust=1), legend.position="bottom") +
    stat_summary(fun = mean, geom="crossbar", size=0.5, color="darkgrey", width=.5) +
    stat_summary(fun = median, geom="crossbar", size=0.5, color="black", width=.5) +
    #guides(col=guide_legend(nrow=2)) +
    labs(title="GROUPE", x=NULL, y=label_y)
  
  p2 = ggplot(data, aes(x=zone, y=data[,variable], color=zone, fill=zone)) + 
    geom_dotplot(binaxis='y', stackdir='center')+
    theme_bw() +
    theme(legend.position="none") +
    #theme(axis.text.x=element_text(size=9, angle=45, hjust=1), legend.position="bottom") +
    stat_summary(fun = mean, geom="crossbar", size=0.5, color="darkgrey", width=.5) +
    stat_summary(fun = median, geom="crossbar", size=0.5, color="black", width=.5) +
    #guides(col=guide_legend(nrow=2)) +
    labs(title="ZONE", x=NULL, y=NULL)
  
  p3 = ggplot(data, aes(x=season, y=data[,variable], color=season, fill=season)) + 
    geom_dotplot(binaxis='y', stackdir='center')+
    theme_bw() +
    theme(legend.position="none") +
    #theme(axis.text.x=element_text(size=9, angle=45, hjust=1), legend.position="bottom") +
    stat_summary(fun = mean, geom="crossbar", size=0.5, color="darkgrey", width=.5) +
    stat_summary(fun = median, geom="crossbar", size=0.5, color="black", width=.5) +
    #guides(col=guide_legend(nrow=2)) +
    labs(title="SAISON", x=NULL, y=NULL)
  
  #list("plot_taxon"=p1,"plot_zone"=p2,"plot_saison"=p3)
  plot=ggdraw()+
    draw_plot(p1, x=0, y=0, width=0.35, height=1) +
    draw_plot(p2, x=0.35,y=0, 0.45, 1) +
    draw_plot(p3, x=0.8,y=0, 0.2, 1)
  
  ggsave(filename = paste(wd,label_data,"_",variable,"_group_zone_saison.jpeg", sep=""),
         plot = plot,device = "jpeg",width = 20,height = 5, units = "cm",dpi=300)
}