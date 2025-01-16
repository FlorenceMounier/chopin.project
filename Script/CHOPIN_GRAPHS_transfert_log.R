

sed_contam$zone = factor(sed_contam$zone,levels = levels(sed_contam$zone),
                         labels = c("EMB","FN","FS"))

sed_contam$sommePCB_log_ng_gdw = log(sommePCB_ng_gdw)
sed_contam$sommePFAS_log_ng_gdw = log(sommePFAS_ng_gdw)
sed_contam$sommeHBCDD_log_ng_gdw = log(sommeHBCDD_ng_gdw)

par(mar=c(4,4,3,2))
p1 = ggplot(sed_contam, aes(x=zone, y=sommePCB_log_ng_gdw, col=zone, fill=zone)) + 
  geom_dotplot(binaxis='y', stackdir='center')+
  theme_bw() +
  theme(legend.position="none") +
  theme(axis.title.y = element_text(size=8), title = element_text(size=8))+
  #theme(axis.text.x=element_text(size=9, angle=45, hjust=1), legend.position="bottom") +
  stat_summary(fun = mean, geom="crossbar", size=0.5, color="darkgrey", width=.5) +
  stat_summary(fun = median, geom="crossbar", size=0.5, color="black", width=.5) +
  expand_limits(y=c(-5, 8))+
  #guides(col=guide_legend(nrow=2)) +
  labs(title=paste("SEDIMENT"), x=NULL, y=expression(paste("LOG de la ",sum(),"PCB (log ng.",g^-1,"ps)")))

p2 = ggplot(benthos_contam, aes(x=grp, y=sommePCB_log_ng_gdw, color=grp, fill=grp))+
  geom_dotplot(binaxis='y', stackdir='center')+
  theme_bw() +
  theme(legend.position="none") +
  theme(axis.title.y = element_text(size=6),title = element_text(size=8))+
  #theme(axis.text.x=element_text(size=9, angle=45, hjust=1), legend.position="bottom") +
  stat_summary(fun = mean, geom="crossbar", size=0.5, color="darkgrey", width=.5) +
  stat_summary(fun = median, geom="crossbar", size=0.5, color="black", width=.5) +
  #guides(col=guide_legend(nrow=2)) +
  expand_limits(y=c(-5, 8))+
  labs(title=paste("BENTHOS"), x=NULL, y=NULL)

p3 = ggplot(soles_contam, aes(x=grp, y=sommePCB_log_ng_gdw, color=grp, fill=grp)) + 
  geom_dotplot(binaxis='y', stackdir='center')+
  theme_bw() +
  theme(legend.position="none") +
  theme(axis.title.y = element_text(size=6), title = element_text(size=8))+
  #theme(axis.text.x=element_text(size=9, angle=45, hjust=1), legend.position="bottom") +
  stat_summary(fun = mean, geom="crossbar", size=0.5, color="darkgrey", width=.5) +
  stat_summary(fun = median, geom="crossbar", size=0.5, color="black", width=.5) +
  expand_limits(y=c(-5, 8))+
  #guides(col=guide_legend(nrow=2)) +
  labs(title=paste("SOLES"), x=NULL,y=NULL)

plot = ggdraw()+
  draw_plot(p1, x=0, y=0, width=0.3, height=1) +
  draw_plot(p2, x=0.3,y=0, 0.4, 1) +
  draw_plot(p3, x=0.7,y=0, 0.3, 1)

ggsave(filename = "Output/transfert_PCB.jpeg",
       plot = plot,device = "jpeg",width = 20,height = 7, units = "cm",dpi=300)



par(mar=c(4,4,3,2))
p1 = ggplot(sed_contam, aes(x=zone, y=sommePFAS_log_ng_gdw, col=zone, fill=zone)) + 
  geom_dotplot(binaxis='y', stackdir='center')+
  theme_bw() +
  theme(legend.position="none") +
  theme(axis.title.y = element_text(size=8), title = element_text(size=8))+
  #theme(axis.text.x=element_text(size=9, angle=45, hjust=1), legend.position="bottom") +
  stat_summary(fun = mean, geom="crossbar", size=0.5, color="darkgrey", width=.5) +
  stat_summary(fun = median, geom="crossbar", size=0.5, color="black", width=.5) +
  #guides(col=guide_legend(nrow=2)) +
  expand_limits(y=c(-5, 8))+
  labs(title=paste("SEDIMENT"), x=NULL, y=expression(paste("LOG de la ",sum(),"PFAS (log ng.",g^-1,"ps)")))

p2 = ggplot(benthos_contam, aes(x=grp, y=sommePFAS_log_ng_gdw, color=grp, fill=grp))+
  geom_dotplot(binaxis='y', stackdir='center')+
  theme_bw() +
  theme(legend.position="none") +
  theme(axis.title.y = element_text(size=6),title = element_text(size=8))+
  #theme(axis.text.x=element_text(size=9, angle=45, hjust=1), legend.position="bottom") +
  stat_summary(fun = mean, geom="crossbar", size=0.5, color="darkgrey", width=.5) +
  stat_summary(fun = median, geom="crossbar", size=0.5, color="black", width=.5) +
  #guides(col=guide_legend(nrow=2)) +
  expand_limits(y=c(-5,8))+
  labs(title=paste("BENTHOS"), x=NULL, y=NULL)

p3 = ggplot(soles_contam, aes(x=grp, y=sommePFAS_log_ng_gdw, color=grp, fill=grp)) + 
  geom_dotplot(binaxis='y', stackdir='center')+
  theme_bw() +
  theme(legend.position="none") +
  theme(axis.title.y = element_text(size=6), title = element_text(size=8))+
  #theme(axis.text.x=element_text(size=9, angle=45, hjust=1), legend.position="bottom") +
  stat_summary(fun = mean, geom="crossbar", size=0.5, color="darkgrey", width=.5) +
  stat_summary(fun = median, geom="crossbar", size=0.5, color="black", width=.5) +
  #guides(col=guide_legend(nrow=2)) +
  expand_limits(y=c(-5,8))+
  labs(title=paste("SOLES"), x=NULL,y=NULL)

plot = ggdraw()+
  draw_plot(p1, x=0, y=0, width=0.3, height=1) +
  draw_plot(p2, x=0.3,y=0, 0.4, 1) +
  draw_plot(p3, x=0.7,y=0, 0.3, 1)

ggsave(filename = "Output/transfert_PFAS.jpeg",
       plot = plot,device = "jpeg",width = 20,height = 7, units = "cm",dpi=300)



par(mar=c(4,4,3,2))
p1 = ggplot(sed_contam, aes(x=zone, y=sommeHBCDD_log_ng_gdw, col=zone, fill=zone)) + 
  geom_dotplot(binaxis='y', stackdir='center')+
  theme_bw() +
  theme(legend.position="none") +
  theme(axis.title.y = element_text(size=8), title = element_text(size=8))+
  #theme(axis.text.x=element_text(size=9, angle=45, hjust=1), legend.position="bottom") +
  stat_summary(fun = mean, geom="crossbar", size=0.5, color="darkgrey", width=.5) +
  stat_summary(fun = median, geom="crossbar", size=0.5, color="black", width=.5) +
  #guides(col=guide_legend(nrow=2)) +
  expand_limits(y=c(-5, 8))+
  labs(title=paste("SEDIMENT"), x=NULL, y=expression(paste("LOG de la ",sum(),"HBCDD (log ng.",g^-1,"ps)")))

p2 = ggplot(benthos_contam, aes(x=grp, y=sommeHBCDD_log_ng_gdw, color=grp, fill=grp))+
  geom_dotplot(binaxis='y', stackdir='center')+
  theme_bw() +
  theme(legend.position="none") +
  theme(axis.title.y = element_text(size=6),title = element_text(size=8))+
  #theme(axis.text.x=element_text(size=9, angle=45, hjust=1), legend.position="bottom") +
  stat_summary(fun = mean, geom="crossbar", size=0.5, color="darkgrey", width=.5) +
  stat_summary(fun = median, geom="crossbar", size=0.5, color="black", width=.5) +
  #guides(col=guide_legend(nrow=2)) +
  expand_limits(y=c(-5, 8))+
  labs(title=paste("BENTHOS"), x=NULL, y=NULL)

p3 = ggplot(soles_contam, aes(x=grp, y=sommeHBCDD_log_ng_gdw, color=grp, fill=grp)) + 
  geom_dotplot(binaxis='y', stackdir='center')+
  theme_bw() +
  theme(legend.position="none") +
  theme(axis.title.y = element_text(size=6), title = element_text(size=8))+
  #theme(axis.text.x=element_text(size=9, angle=45, hjust=1), legend.position="bottom") +
  stat_summary(fun = mean, geom="crossbar", size=0.5, color="darkgrey", width=.5) +
  stat_summary(fun = median, geom="crossbar", size=0.5, color="black", width=.5) +
  #guides(col=guide_legend(nrow=2)) +
  expand_limits(y=c(-5, 8))+
  labs(title=paste("SOLES"), x=NULL,y=NULL)

plot = ggdraw()+
  draw_plot(p1, x=0, y=0, width=0.3, height=1) +
  draw_plot(p2, x=0.3,y=0, 0.4, 1) +
  draw_plot(p3, x=0.7,y=0, 0.3, 1)

ggsave(filename = "Output/transfert_HBCDD.jpeg",
       plot = plot,device = "jpeg",width = 20,height = 7, units = "cm",dpi=300)
