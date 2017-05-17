
### Survie ###
par(mfrow=c(3,2))
### OS 
summary(as.numeric(greffe$delai_dc))
s<-Surv(event=greffe$deces,time=greffe$delai_dc)

a <- survfit( s ~ 1)
re<-summary(a,censored = TRUE)
plot(a, xlab="Time in months",ylab="Probability")

censure<-as.data.frame(cbind(re$time[re$n.event==0],re$surv[re$n.event==0] ))
colnames(censure)<-c("time","ce")
evenement<-as.data.frame(cbind(re$time,re$surv ))
colnames(evenement)<-c("time","ev")
intervalle<-as.data.frame(cbind(re$time,re$upper
                                ,re$lower ))
colnames(intervalle)<-c("time","haut","bas")
ggplot()+ geom_step(data=evenement,aes(x=time, y=ev),color="black", direction="hv")  +
  geom_ribbon(data=intervalle, aes(x=time, ymin=bas, ymax=haut),fill="grey",alpha="0.5")+
  geom_step(data=intervalle,aes(x=time, y=haut),color="black" ,direction="hv")+
  geom_step(data=intervalle,aes(x=time, y=bas),color="black", direction="hv")+
  scale_x_continuous(breaks=c(0,20,40,60,80,100),expand = c(0, 0))+
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
  scale_y_continuous(breaks=c(0,0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1),expand = c(0, 0))+
  coord_cartesian(ylim=c(0,1))










anapath<-summary(coxph( s ~ anapathc2,data=greffe))
anapath
anapath_t<-cox.zph(coxph( s ~ anapathc2,data=greffe))
plot(anapath_t[1:nlevels(greffe$anapathc2)-1,])


sex<-summary(coxph( s ~ sex_patient,data=greffe))
sex_t<-cox.zph(coxph( s ~ sex_patient,data=greffe))
plot(sex_t)


summary(coxph( s ~ greffe[,"disease_status_at_transplantc"],data=greffe))

for (i in c("delai_dia_alloc","stade_diac","disease_status_at_transplantc","disease_status_at_transplant",
            "karnofsky_greffec", "previous_autoc","programme_autoalloc","rechute_prem_greffec",
            "nbr_lignes_avt_alloc",
            "donnor","hla_matchc","hla_match",
            "sex_dp2","cmv_dp2","stem_cell_source","tbi","intensite_condi","condit_details","manipu_cells","nbr_donneurc","agvhd","agvhd_grade","cgvhd",
            "cgvhd_grade","cause_death_c","best_response_after_allo","relapse_progression_transplant_c")){
  
a<-summary(coxph( s ~ greffe[,i],data=greffe))
assign(paste(i, "m", sep="_"),a)
capture.output(a, file=paste("C:/Users/adupont/Documents/projetstlouis/resultats/",i,"m.txt",sep=""))
ss<-cox.zph( coxph( s ~ greffe[,i],data=greffe))
pdf(paste("C:/Users/adupont/Documents/projetstlouis/resultats/",i,"m.pdf",sep=""), width=4, height=4,onefile = TRUE)
#par(mfrow=c(2,2))
plot(ss[1:nlevels(greffe[,i])-1,])
dev.off()

    
}








### EFS (rechute, progression, deces)
efs<-Surv(event=greffe$rechute_progressionc,time=greffe$delai_efs)

e <- survfit( efs ~ 1)


ree<-summary(e,censored = TRUE)
plot(e, xlab="Time in months",ylab="Probability")

censuree<-as.data.frame(cbind(ree$time[ree$n.event==0],ree$surv[ree$n.event==0] ))
colnames(censuree)<-c("time","ce")
evenemente<-as.data.frame(cbind(ree$time,ree$surv ))
colnames(evenemente)<-c("time","ev")
intervallee<-as.data.frame(cbind(ree$time,ree$upper
                                ,ree$lower ))
colnames(intervallee)<-c("time","haut","bas")


ggplot()+ geom_step(data=evenement,aes(x=time, y=ev),color="black", direction="hv")  +
  geom_step(data=evenemente,aes(x=time, y=ev),color="blue", direction="hv",linetype = "dashed")  +
  #geom_ribbon(data=intervalle, aes(x=time, ymin=bas, ymax=haut),fill="grey",alpha="0.5")+
  #geom_step(data=intervalle,aes(x=time, y=haut),color="black" ,direction="hv")+
  #geom_step(data=intervalle,aes(x=time, y=bas),color="black", direction="hv")+
  scale_x_continuous(breaks=c(0,20,40,60,80,100),expand = c(0, 0))+
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
  annotate(geom="text", x=90, y=0.55, label="OS",
           color="black")+
  annotate(geom="text", x=90, y=0.45, label="EFS",
           color="blue")









### GVHD

gvhd<-Surv(event=greffe$gvhd,time=greffe$delai_gvhd)

e <- survfit( gvhd ~ 1)
ree<-summary(e,censored = TRUE)
plot(e, xlab="Time in months",ylab="Probability")

censuree<-as.data.frame(cbind(ree$time[ree$n.event==0],ree$surv[ree$n.event==0] ))
colnames(censuree)<-c("time","ce")
evenemente<-as.data.frame(cbind(ree$time,ree$surv ))
colnames(evenemente)<-c("time","ev")
intervallee<-as.data.frame(cbind(ree$time,ree$upper
                                 ,ree$lower ))
colnames(intervallee)<-c("time","haut","bas")


ggplot()+ 
  geom_step(data=evenemente,aes(x=time, y=ev),color="black", direction="hv")  +
  #geom_step(data=evenemente,aes(x=time, y=ev),color="blue", direction="hv",linetype = "dashed")  +
  #geom_ribbon(data=intervalle, aes(x=time, ymin=bas, ymax=haut),fill="grey",alpha="0.5")+
  #geom_step(data=intervalle,aes(x=time, y=haut),color="black" ,direction="hv")+
  #geom_step(data=intervalle,aes(x=time, y=bas),color="black", direction="hv")+
  scale_x_continuous(breaks=c(0,20,40,60,80,100),expand = c(0, 0))+
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
  annotate(geom="text", x=70, y=0.55, label="GVHD", color="black")
 # annotate(geom="text", x=90, y=0.45, label="EFS", color="blue")


### Risques compétitifs###

# pour rechute/ deces : pb d'un patient qui a une première greffe échec car la greffe n'a pas prise
# on la supprime et on ne garde que la seconde

modelcompete<-cuminc(greffe$delai_rechute,greffe$rechute_dc,cencode=0,subset=greffe$best_response_after_allo %in%
                       c("cr","PR")& !greffe$j0=="2012-08-28")
plot(modelcompete,curvlab=c("Relapse","Death"),xlab="Time (months)", ylab="Probability")


# pour rechute/p deces : pb d'un patient qui a une première greffe échec car la greffe n'a pas prise
# on la supprime et on ne garde que la seconde

modelcompete<-cuminc(greffe$delai_efs,greffe$rechute_progression_dc,cencode=0,subset= !greffe$j0=="2012-08-28")
plot(modelcompete,curvlab=c("Relapse/Progression","Death"),xlab="Time (months)", ylab="Probability")

modelcompete<-cuminc(greffe$delai_rechutepg,greffe$rechute_progression_ghvd_dc,cencode=0,subset= !greffe$j0=="2012-08-28")
plot(modelcompete,xlab="Time (months)", ylab="Probability")
