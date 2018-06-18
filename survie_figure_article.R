### Figures pour les sous types ####
TABLEAU<-NULL

for (i in c("NOS","AITL","ALCL ALK+","NK/T nasal","HS","EATL","Autres ALCL")) {

trash<-greffe[greffe$anapath3 %in% i,]
summary(as.numeric(trash$delai_dc))
s<-Surv(event=trash$deces,time=as.numeric(trash$delai_dc))

# trash$delai_dc2<-trash$age_trash*30.25+trash$delai_dc
# sbis<-Surv(event=trash$deces,time2=trash$delai_dc2,time=trash$age_trash*30.25)
# abis <- survfit( sbis ~ 1)
# plot(abis)


a <- survfit( s ~ 1,conf.type = "log-log")
re<-summary(a,censored = TRUE)


censure<-as.data.frame(cbind(re$time[re$n.event==0],re$surv[re$n.event==0] ))
colnames(censure)<-c("time","ce")
evenement<-as.data.frame(cbind(re$time,re$surv ))
colnames(evenement)<-c("time","ev")
intervalle<-as.data.frame(cbind(re$time,re$upper
                                ,re$lower ))
colnames(intervalle)<-c("time","haut","bas")


cpfs<-Surv(event=trash$rechute_progressionc,time=trash$delai_pfs)
e <- survfit( cpfs ~ 1)
ree<-summary(e,censored = TRUE)
plot(e, xlab="Time in months",ylab="Probability")

censuree<-as.data.frame(cbind(ree$time[ree$n.event==0],ree$surv[ree$n.event==0] ))
colnames(censuree)<-c("time","ce")
evenemente<-as.data.frame(cbind(ree$time,ree$surv ))
colnames(evenemente)<-c("time","ev")
intervallee<-as.data.frame(cbind(ree$time,ree$upper
                                 ,ree$lower ))
colnames(intervallee)<-c("time","haut","bas")



pfs_kmt<-ggplot()+ geom_step(data=evenement,aes(x=time, y=ev),color="black", direction="hv")  +
  geom_step(data=evenemente,aes(x=time, y=ev),color="black", direction="hv",linetype = "dashed")  +
  #geom_ribbon(data=intervalle, aes(x=time, ymin=bas, ymax=haut),fill="grey",alpha="0.5")+
  #geom_step(data=intervalle,aes(x=time, y=haut),color="black" ,direction="hv")+
  #geom_step(data=intervalle,aes(x=time, y=bas),color="black", direction="hv")+
  scale_x_continuous(breaks=c(0,20,40,60),expand = c(0, 0),limits = c(0,70))+
  scale_size_manual(values=c(1.5,1.5))+
  
  geom_text(x=80, y=0.55, label="OS")+
  geom_text(x=80, y=0.45, label="EFS")+
  #geom_point(data=censure, aes(x=time, y=ce),shape=3,size=1 )+
  #ggtitle("Durée de vie des implants") +
  xlab("Time (Months)")+
  ylab("Probability")+
  #geom_step(data=gri,aes(x=time, y=ics), direction="hv",color="gray10",linetype="dashed" )+
  #  geom_step(data=gri,aes(x=time, y=ici), direction="hv",color="gray10",linetype="dashed" )+
  #  geom_step(data=gci,aes(x=time, y=ics), direction="hv",color="black",linetype="dashed" )+
  #  geom_step(data=gci,aes(x=time, y=ici), direction="hv",color="black",linetype="dashed" )+
  #
  #scale_colour_manual("",values = c("Rupture"="blue", "Autres causes"="black"))+annotate(geom="text", x=52, y=0.91, label="Tous les parcours",color="black", size=4)+coord_cartesian(ylim=c(0,1)) +
  scale_y_continuous(breaks=c(0,0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1),expand = c(0, 0))+
  coord_cartesian(ylim=c(0,1))+
  # annotate(geom="text", x=90, y=0.55, label="OS",
  #          color="black")+
  # annotate(geom="text", x=90, y=0.5, label="PFS",
  #          color="blue")+
  theme_classic()


trrm<-Surv(event=trash$cause_death_c3,time=as.numeric(trash$delai_dc))

# greffe$delai_dc2<-greffe$age_greffe*30.25+greffe$delai_dc
# sbis<-Surv(event=greffe$deces,time2=greffe$delai_dc2,time=greffe$age_greffe*30.25)
# abis <- survfit( sbis ~ 1)
# plot(abis)


atrm <- survfit( trrm ~ 1,conf.type = "log-log")
retrm<-summary(atrm,censored = TRUE)
plot(atrm, xlab="Time in months",ylab="Probability")




censuretrm<-as.data.frame(cbind(retrm$time[retrm$n.event==0],retrm$surv[retrm$n.event==0] ))
colnames(censuretrm)<-c("time","ce")
evenementtrm<-as.data.frame(cbind(retrm$time,retrm$surv ))
colnames(evenementtrm)<-c("time","ev")
intervalletrm<-as.data.frame(cbind(retrm$time,retrm$upper
                                   ,retrm$lower ))
colnames(intervalletrm)<-c("time","haut","bas")


km_trmt<-ggplot()+ geom_step(data=evenementtrm,aes(x=time, y=1-ev),color="black", direction="hv")  +
  #geom_ribbon(data=intervalletrm, aes(x=time, ymin=1-bas, ymax=1-haut),linetype="dashed",fill="grey",alpha="0.4")+
  #geom_step(data=intervalletrm,aes(x=time, y=1-haut),color="black" ,direction="hv")+
  #geom_step(data=intervalletrm,aes(x=time, y=1-bas),color="black", direction="hv")+
  scale_x_continuous(breaks=c(0,20,40,60),expand = c(0, 0),limits=c(0,70))+
  scale_size_manual(values=c(1.5,1.5))+
  
  #geom_point(data=censure, aes(x=time, y=ce),shape=3,size=1 )+
  #ggtitle("Durée de vie des implants") +
  xlab("Time (Months)")+
  ylab("Probability")+
  #geom_step(data=gri,aes(x=time, y=ics), direction="hv",color="gray10",linetype="dashed" )+
  #  geom_step(data=gri,aes(x=time, y=ici), direction="hv",color="gray10",linetype="dashed" )+
  #  geom_step(data=gci,aes(x=time, y=ics), direction="hv",color="black",linetype="dashed" )+
  #  geom_step(data=gci,aes(x=time, y=ici), direction="hv",color="black",linetype="dashed" )+
  #
  #scale_colour_manual("",values = c("Rupture"="blue", "Autres causes"="black"))+annotate(geom="text", x=52, y=0.91, label="Tous les parcours",color="black", size=4)+coord_cartesian(ylim=c(0,1)) +
  scale_y_continuous(breaks=c(0,0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1),expand = c(0, 0),limits = c(0, 1))+
  #coord_cartesian(ylim=c(0,1))+
  theme_classic()+
  theme(legend.position="bottom",
        legend.title=element_blank())+
  ggtitle("")

modelcompeteb<-cuminc(trash$delai_pfs,trash$rechute_progression_dc,cencode=0)

if (!i %in% c("NK/T nasal","HS","EATL")){
gri<-as.data.frame(cbind(modelcompeteb[[1]]$time,modelcompeteb[[1]]$est-1.96*sqrt(modelcompeteb[[1]]$var),modelcompeteb[[1]]$est+1.96*sqrt(modelcompeteb[[1]]$var)))
gci<-as.data.frame(cbind(modelcompeteb[[2]]$time,modelcompeteb[[2]]$est-1.96*sqrt(modelcompeteb[[2]]$var),modelcompeteb[[2]]$est+1.96*sqrt(modelcompeteb[[2]]$var)))
}
#plot(modelcompeteb,curvlab=c("Relapse/Progression","Death without relapse or progression"),xlab="Time (months)", ylab="Probability")


### tableau ### 
ost=c(round(re$surv[which.min((re$time-12)<0)-1],2),round(re$lower[which.min((re$time-12)<0)],2),
round(re$upper[which.min((re$time-12)<0)-1],2), round(re$surv[which.min((re$time-24)<0)-1],2),
round(re$lower[which.min((re$time-24)<0)-1],2),round(re$upper[which.min((re$time-24)<0)-1],2), 
round(re$surv[which.min((re$time-48)<0)-1],2),round(re$lower[which.min((re$time-48)<0)-1],2),
round(re$upper[which.min((re$time-48)<0)-1],2))

efst=c(round(ree$surv[which.min((ree$time-12)<0)-1],2),round(ree$lower[which.min((ree$time-12)<0)-1],2) ,
round(ree$upper[which.min((ree$time-12)<0)-1],2),round(ree$surv[which.min((ree$time-24)<0)-1],2) ,
round(ree$lower[which.min((ree$time-24)<0)-1],2),round(ree$upper[which.min((ree$time-24)<0)-1],2),
round(ree$surv[which.min((ree$time-48)<0)-1],2) ,round(ree$lower[which.min((ree$time-48)<0)-1],2),
round(ree$upper[which.min((ree$time-48)<0)-1],2))

trmt=c(1-round(retrm$surv[which.min((retrm$time-12)<0)-1],2),1-round(retrm$lower[which.min((retrm$time-12)<0)-1],2),
1-round(retrm$upper[which.min((retrm$time-12)<0)-1],2),1-round(retrm$surv[which.min((retrm$time-24)<0)-1],2),
1-round(retrm$lower[which.min((retrm$time-24)<0)-1],2),1-round(retrm$upper[which.min((retrm$time-24)<0)-1],2),
1-round(retrm$surv[which.min((retrm$time-48)<0)-1],2),1-round(retrm$lower[which.min((retrm$time-48)<0)-1],2),
1-round(retrm$upper[which.min((retrm$time-48)<0)-1],2))

if (!i %in% c("NK/T nasal","HS","EATL")){
      
rechutet=c(round(modelcompeteb[[1]]$est[which.min((modelcompeteb[[1]]$time-12)<0)-1],2),
          round(gri$V2[which.min((gri$V1-12)<0)-1],2),
          round(gri$V3[which.min((gri$V1-12)<0)-1],2),
          round(modelcompeteb[[1]]$est[which.min((modelcompeteb[[1]]$time-24)<0)-1],2),
          round(gri$V2[which.min((gri$V1-24)<0)-1],2),
          round(gri$V3[which.min((gri$V1-24)<0)-1],2),
          round(modelcompeteb[[1]]$est[which.min((modelcompeteb[[1]]$time-48)<0)-1],2),
          round(gri$V2[which.min((gri$V1-48)<0)-1],2),
          round(gri$V3[which.min((gri$V1-48)<0)-1],2)
)  
}

else {rechutet=c(NA,NA,NA,NA,NA,NA,NA,NA,NA)}



tableau<-rbind(ost,efst,trmt,rechutet)
tableau<-cbind(rep(i,4),tableau)

TABLEAU<-rbind(TABLEAU,tableau)


assign(paste("pfs_km",substr(i,1,2),sep="_"),pfs_kmt)
assign(paste("trm_km",substr(i,1,2),sep="_"),km_trmt)

# png(filename=paste0("Z:/projetstlouis/scripts/figure/","cuminc",substr(i,1,2),".png"))
# plot(modelcompeteb,curvlab=c("Relapse/Progression","Death without relapse or progression"),xlab="Time (months)", ylab="Probability")
# dev.off()



}



colnames(TABLEAU)<-c("histo","1 an","BI 1 an","BS 1 an",
                     "2 ans","BI 2 an","BS 2 an","4 an","BI 4 an","BS 4 an")
