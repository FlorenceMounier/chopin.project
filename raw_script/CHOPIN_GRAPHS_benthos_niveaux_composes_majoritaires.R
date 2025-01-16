####################################
##         PROJET CHOPIN
## 
##          GRAPHIQUES
##   NIVEAU COMPOSES MAJORITAIRES
##           BENTHOS
##      
####################################



png("Output/BENTHOS/BENTHOS_CB153_L.PFOS_g.HBCDD_espece.jpeg",width = 900, height = 600, units = "px")


b1 = ggplot(benthos_contam_juin, aes(x=sample_TAG, y=CB153_ng_g.1ps, color=grp, fill=grp)) + 
  geom_bar(stat="identity", width=0.5)+
  scale_x_discrete(limits=benthos_contam_juin$sample_TAG[order(benthos_contam_juin$zone)]) +
  #scale_x_discrete(labels=benthos_contam_juin$species)+
  geom_vline(xintercept=6.5) +
  annotate(size=3,"text", x=3.5, y=300, label="Embouchure")+
  geom_vline(xintercept=6.5) +
  annotate(size=3,"text", x=9.5, y=300, label="Fosse Nord")+
  geom_vline(xintercept=12.5) + 
  annotate(size=3,"text", x=14.5, y=300, label="Fosse Sud")+
  theme_bw() +
  theme(legend.position="none") +
  theme(axis.text.x = element_blank())+
  #theme(axis.text.x=element_text(size=9, angle=45, hjust=1), legend.position="bottom") +
  #guides(col=guide_legend(nrow=2)) +
  labs(title="Juin 2017", x=NULL, y=expression(paste("CB153 (ng.", g^{-1},"ps)")))


b2 = ggplot(benthos_contam_oct, aes(x=sample_TAG, y=CB153_ng_g.1ps, color=grp, fill=grp)) + 
  geom_bar(stat="identity", width=0.5)+
  scale_x_discrete(limits=benthos_contam_oct$sample_TAG[order(benthos_contam_oct$zone)]) +
  annotate(size=3,"text", x=1, y=250, label="Chenal", angle=90)+
  geom_vline(xintercept=1.5) +
  annotate(size=3,"text", x=7.5, y=300, label="Embouchure")+
  geom_vline(xintercept=14.5) +
  annotate(size=3,"text", x=17.5, y=300, label="Fosse Nord")+
  geom_vline(xintercept=21.5) + 
  annotate(size=3,"text", x=27.5, y=300, label="Fosse Sud")+
  theme_bw() +
  theme(legend.position="none") +
  theme(axis.text.x = element_blank())+
  #scale_x_discrete(labels=benthos_contam_oct$species)+
  #theme(axis.text.x=element_text(size=9, angle=45, hjust=1), legend.position="bottom") +
  #guides(col=guide_legend(nrow=2)) +
  labs(title=expression(paste("Octobre 2017")), x=NULL, y=NULL)


b3 = ggplot(benthos_contam_juin, aes(x=sample_TAG, y=L.PFOS, color=grp, fill=grp)) + 
  geom_bar(stat="identity", width=0.5)+
  scale_x_discrete(limits=benthos_contam_juin$sample_TAG[order(benthos_contam_juin$zone)]) +
  geom_vline(xintercept=6.5) +
  annotate(size=3,"text", x=3.5, y=25, label="Embouchure")+
  geom_vline(xintercept=6.5) +
  annotate(size=3,"text", x=9.5, y=25, label="Fosse Nord")+
  geom_vline(xintercept=12.5) + 
  annotate(size=3,"text", x=14.5, y=25, label="Fosse Sud")+
  theme_bw() +
  #scale_x_discrete(labels=benthos_contam_juin$species)+
  theme(legend.position="none") +
  theme(axis.text.x = element_blank())+
  #theme(axis.text.x=element_text(size=9, angle=45, hjust=1), legend.position="bottom") +
  #guides(col=guide_legend(nrow=2)) +
  labs(title=NULL, x=NULL, y=expression(paste("L-PFOS (ng.", g^{-1},"ps)")))

b4 = ggplot(benthos_contam_oct, aes(x=sample_TAG, y=L.PFOS, color=grp, fill=grp)) + 
  geom_bar(stat="identity", width=0.5)+
  scale_x_discrete(limits=benthos_contam_oct$sample_TAG[order(benthos_contam_oct$zone)]) +
  annotate(size=3,"text", x=1, y=20, label="CH", angle=90)+
  geom_vline(xintercept=1.5) +
  annotate(size=3,"text", x=7.5, y=25, label="Embouchure")+
  geom_vline(xintercept=14.5) +
  annotate(size=3,"text", x=17.5, y=25, label="Fosse Nord")+
  geom_vline(xintercept=21.5) + 
  annotate(size=3,"text", x=27.5, y=25, label="Fosse Sud")+
  theme_bw() +
  theme(legend.position="none") +
  theme(axis.text.x = element_blank())+
  #scale_x_discrete(labels=benthos_contam_oct$species)+
  #theme(axis.text.x=element_text(size=9, angle=45, hjust=1), legend.position="bottom") +
  #guides(col=guide_legend(nrow=2)) +
  labs(title=NULL, x=NULL, y=NULL)

b5 = ggplot(benthos_contam_juin, aes(x=sample_TAG, y=g.HBCDD_ng_g.1ps_censored, color=grp, fill=grp)) + 
  geom_bar(stat="identity", width=0.5)+
  scale_x_discrete(limits=benthos_contam_juin$sample_TAG[order(benthos_contam_juin$zone)], 
                   labels=benthos_contam_juin$labels[order(benthos_contam_juin$zone)]) +
  geom_vline(xintercept=6.5) +
  annotate(size=3,"text", x=3.5, y=82, label="Embouchure")+
  geom_vline(xintercept=6.5) +
  annotate(size=3,"text", x=9.5, y=82, label="Fosse Nord")+
  geom_vline(xintercept=12.5) + 
  annotate(size=3,"text", x=14.5, y=82, label="Fosse Sud")+
  theme_bw() +
  theme(legend.position="none") +
  theme(axis.text.x=element_text(size=9, angle=90,vjust=0.4, hjust=1), legend.position="bottom") +
  #guides(col=guide_legend(nrow=2)) +
  labs(title=NULL, x=NULL, y=expression(paste(gamma,"-HBCDD (ng.", g^{-1},"ps)")))+
  theme(legend.position="none") 

b6 = ggplot(benthos_contam_oct, aes(x=sample_TAG, y=g.HBCDD_ng_g.1ps_censored, color=grp, fill=grp)) + 
  geom_bar(stat="identity", width=0.5)+
  scale_x_discrete(limits=benthos_contam_oct$sample_TAG[order(benthos_contam_oct$zone)], 
                   labels=benthos_contam_oct$labels[order(benthos_contam_oct$zone)]) +
  annotate(size=3,"text", x=1, y=72, label="Chenal",angle=90)+
  geom_vline(xintercept=1.5) +
  annotate(size=3,"text", x=7.5, y=82, label="Embouchure")+
  geom_vline(xintercept=14.5) +
  annotate(size=3,"text", x=17.5, y=82, label="Fosse Nord")+
  geom_vline(xintercept=21.5) +
  annotate(size=3,"text", x=27.5, y=82, label="Fosse Sud")+
  theme_bw() +
  theme(axis.text.x=element_text(size=9, angle=90,vjust=0.4, hjust=1), legend.position="bottom") +
  #guides(col=guide_legend(nrow=2)) +
  labs(title=NULL, x=NULL, y=NULL)+
  theme(legend.position="none") 

a = 6
b = 5
c = 8
z=a+b+c

grid.arrange(b1,b2,b3,b4,b5,b6,
             ncol=3,nrow=z,
             layout_matrix = rbind(t(matrix(rep.int(c(1,2,2),times = a),ncol = a, nrow=3)),
                                   t(matrix(rep.int(c(3,4,4),times = b),ncol = b, nrow=3)),
                                   t(matrix(rep.int(c(5,6,6),times = c),ncol = c, nrow=3)))
)

dev.off()


