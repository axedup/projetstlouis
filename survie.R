
### Survie ###
par(mfrow=c(2,2))
### OS 
summary(as.numeric(greffe$delai_dc))
s<-Surv(event=greffe$deces,time=as.numeric(greffe$delai_dc))

greffe$delai_dc2<-greffe$age_greffe*30.25+greffe$delai_dc
sbis<-Surv(event=greffe$deces,time2=greffe$delai_dc2,time=greffe$age_greffe*30.25)
abis <- survfit( sbis ~ 1)
plot(abis)


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
  coord_cartesian(ylim=c(0,1))+
  theme_classic()


anapath<-summary(coxph( s ~ anapathc2,data=greffe))
anapath
anapath_t<-cox.zph(coxph( s ~ anapathc2,data=greffe))
plot(anapath_t[1:nlevels(greffe$anapathc2)-1,])


sex<-summary(coxph( s ~ sex_patient,data=greffe))
sex_t<-cox.zph(coxph( s ~ sex_patient,data=greffe))
plot(sex_t)
result.cox(coxph( s ~ sex_patient,data=greffe))

summary(coxph( s ~ greffe[,"disease_status_at_transplantc"],data=greffe))

for (i in c("sex_donor","sex_patient",
            "delai_dia_alloc","stade_dia","stade_diac",
            "disease_status_at_transplantc2",
            "disease_status_at_transplantc",
            "disease_status_at_transplant",
            "karnofsky_greffec","karnofsky_greffec2", "previous_autoc",
            "programme_autoalloc","rechute_prem_greffec",
            "nbr_lignes_avt_alloc","nbr_lignes_avt_alloc2",
            "donnor","hla_matchc","hla_match",
            "sex_dp2","cmv_dp2","stem_cell_source","tbi","intensite_condi","condit_details",
            "manipu_cells","nbr_donneurc",
            "agvhd","agvhd_grade","cgvhd",
            "cgvhd_grade","prise_greffe","cause_death_c","best_response_after_allo",
            "relapse_progression_transplant_c","anapathc2")){
  
a<-summary(coxph( s ~ greffe[,i],data=greffe))
assign(paste(i, "m", sep="_"),a)
capture.output(a, file=paste("C:/Users/adupont/Documents/projetstlouis/resultats/",i,"m.txt",sep=""))
ss<-cox.zph( coxph( s ~ greffe[,i],data=greffe),transform = "log")
capture.output(ss, file=paste("C:/Users/adupont/Documents/projetstlouis/resultats/",i,"ss.txt",sep=""))

ssu<-survdiff( s ~ greffe[,i],data=greffe, rho=1)
pw<-(1-pchisq(ssu$chisq, 1))
pdf(paste("C:/Users/adupont/Documents/projetstlouis/resultats/",i,"m.pdf",sep=""), width=4, height=4,onefile = TRUE)
#par(mfrow=c(2,2))
plot(ss[1:nlevels(greffe[,i])-1,])
dev.off()
pdf(paste("C:/Users/adupont/Documents/projetstlouis/resultats/",i,"m2.pdf",sep=""), width=4, height=4,onefile = TRUE)

plot(survfit( s ~greffe[,i],data=greffe),fun="log" ,lty=1:4, col=2:5)
text(0, 0.7, paste("Test du logrank pondéré: p=", format(round(pw, 5), scien=F)), cex=1, adj=0)

dev.off()    

pdf(paste("C:/Users/adupont/Documents/projetstlouis/resultats/",i,"m3.pdf",sep=""), width=4, height=4,onefile = TRUE)

plot(survfit( s ~greffe[,i],data=greffe),fun="cloglog" ,lty=1:4, col=2:5)
text(0, 0.7, paste("Test du logrank pondéré: p=", format(round(pw, 5), scien=F)), cex=1, adj=0)

dev.off() 

}


anapath<-summary(coxph( s ~ sex_dp2,data=greffe))
anapath
plot(survfit( s ~ strata(sex_dp2),data=greffe),fun="log" ,lty=1:4, col=2:5)
e<-cox.zph(coxph( s ~ sex_dp2,data=greffe), transform="identity")
plot(e, main="Identité")
survdiff (s ~ sex_dp2, data=greffe, rho=1)

anapath_t<-cox.zph(,transform = "log")
plot(anapath_t[1:nlevels(greffe$sex_dp2)-1,])









### Rechute/decès uniquement chez ceux qui ont un rémission complèteou partielle

rec<-Surv(event=greffe$rechute[greffe$best_response_after_allo %in%
                                 c("cr","PR")& !greffe$j0=="2012-08-28"],
          time=greffe$delai_rechute[greffe$best_response_after_allo %in%
                                      c("cr","PR")& !greffe$j0=="2012-08-28"])

er <- survfit( rec ~ 1)


rer<-summary(er,censored = TRUE)
plot(er, xlab="Time in months",ylab="Probability")

censure<-as.data.frame(cbind(rer$time[rer$n.event==0],rer$surv[rer$n.event==0] ))
colnames(censure)<-c("time","ce")
evenement<-as.data.frame(cbind(rer$time,rer$surv ))
colnames(evenement)<-c("time","ev")
intervalle<-as.data.frame(cbind(rer$time,rer$upper
                                ,rer$lower ))
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
  coord_cartesian(ylim=c(0,1))+
  theme_classic()



### PFS (rechute, progression, deces)
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
           color="blue")+
  theme_classic()









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



data(psych)
