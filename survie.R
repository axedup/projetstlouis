
### Survie ###
par(mfrow=c(2,2))
### OS 
summary(as.numeric(greffe$delai_dc))
s<-Surv(event=greffe$deces,time=as.numeric(greffe$delai_dc))

# greffe$delai_dc2<-greffe$age_greffe*30.25+greffe$delai_dc
# sbis<-Surv(event=greffe$deces,time2=greffe$delai_dc2,time=greffe$age_greffe*30.25)
# abis <- survfit( sbis ~ 1)
# plot(abis)


a <- survfit( s ~ 1,conf.type = "log-log")
re<-summary(a,censored = TRUE)
plot(a, xlab="Time in months",ylab="Probability")
#axis(2, at = seq(from=0.1, to=1, by=0.1), tick = TRUE, col = "black", lwd = 1, lty = 1, las = 1, cex.axis = 0.8)
#mtext("Nbre d'implants à risque", side=1, at=-8.5, adj=0, cex=1, font=1,
      #outer=F, line=4)
coul=c("black")
for( k in 1:1){
  mtext(20, side=1,  at=0 ,
        adj=0.5, cex=1, font=1, line=5, outer=FALSE, col=coul[k])
}





censure<-as.data.frame(cbind(re$time[re$n.event==0],re$surv[re$n.event==0] ))
colnames(censure)<-c("time","ce")
evenement<-as.data.frame(cbind(re$time,re$surv ))
colnames(evenement)<-c("time","ev")
intervalle<-as.data.frame(cbind(re$time,re$upper
                                ,re$lower ))
colnames(intervalle)<-c("time","haut","bas")
km_os<-ggplot()+ geom_step(data=evenement,aes(x=time, y=ev),color="black", direction="hv")  +
  geom_ribbon(data=intervalle, aes(x=time, ymin=bas, ymax=haut),linetype="dashed",fill="grey",alpha="0.4")+
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


ff <- cph(Surv(event=greffe$deces,time=as.numeric(greffe$delai_dc)) ~1,data =greffe, surv=TRUE,x=TRUE,Y=TRUE)

survplot(ff)
survplot(ff,1, label.curves = F, n.risk = T)

### Limite à 48 mois ###

greffe$deces_48<-ifelse(greffe$deces==1 & greffe$delai_dc>48,0,greffe$deces)
greffe$delai_dc_48<-ifelse( greffe$delai_dc>48,48,greffe$delai_dc)

summary(as.numeric(greffe$delai_dc_48))
s_48<-Surv(event=greffe$deces_48,time=as.numeric(greffe$delai_dc_48))




a_48 <- survfit( s_48 ~ 1)
re_48<-summary(a_48,censored = TRUE)
plot(a_48, xlab="Time in months",ylab="Probability")

censure_48<-as.data.frame(cbind(re_48$time[re_48$n.event==0],re_48$surv[re_48$n.event==0] ))
colnames(censure_48)<-c("time","ce")
evenement_48<-as.data.frame(cbind(re_48$time,re_48$surv ))
colnames(evenement_48)<-c("time","ev")
intervalle_48<-as.data.frame(cbind(re_48$time,re_48$upper
                                ,re_48$lower ))
colnames(intervalle_48)<-c("time","haut","bas")
km_48<-ggplot()+ geom_step(data=evenement_48,aes(x=time, y=ev),color="black", direction="hv")  +
  geom_ribbon(data=intervalle_48, aes(x=time, ymin=bas, ymax=haut),fill="grey",alpha="0.5")+
  geom_step(data=intervalle_48,aes(x=time, y=haut),color="black" ,direction="hv")+
  geom_step(data=intervalle_48,aes(x=time, y=bas),color="black", direction="hv")+
  scale_x_continuous(breaks=c(0,10,20,30,40,50),expand = c(0, 0))+
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

# ,"rechute_post_allo","karnofsky_greffec2", "previous_autoc",
# "programme_autoalloc","rechute_prem_greffec",
# "nbr_lignes_avt_alloc","nbr_lignes_avt_alloc2",
# "donnor","hla_matchc","hla_match","sex_dp3",
# "sex_dp2","cmv_dp2","stem_cell_source","tbi","intensite_condi","condit_details",
# "manipu_cells","nbr_donneurc","manipu_cells",
# "agvhd","agvhd_grade","cgvhd",
# "cgvhd_grade","prise_greffe","cause_death_c","best_response_after_allo",
# "relapse_progression_transplant_c","anapathc2")


# for (i in c("sex_donor","sex_patient","age_greffec",
#             "delai_dia_alloc","stade_dia","stade_diac",
#             "disease_status_at_transplantc2",
#             "disease_status_at_transplantc",
#             "disease_status_at_transplant","rechute_post_allo","karnofsky_greffec2","karnofsky_greffec3" ,
#             "previous_autoc",
#             "programme_autoalloc","rechute_prem_greffec",
#             "nbr_lignes_avt_alloc","nbr_lignes_avt_alloc2",
#             "donnor","hla_matchc","hla_match","sex_dp3",
#             "sex_dp2","cmv_dp2","stem_cell_source","tbi","intensite_condi","condit_details",
#             "manipu_cells","nbr_donneurc","manipu_cells",
#             "agvhd","agvhd3","agvhd_grade","cgvhd",
#             "cgvhd_grade","prise_greffe","cause_death_c","best_response_after_allo",
#             "relapse_progression_transplant_c","anapathc2")
# ){
#   
# a<-summary(coxph( s_48 ~ greffe[,i],data=greffe))
# assign(paste(i, "m_48", sep="_"),a)
# capture.output(a, file=paste("C:/Users/adupont/Documents/projetstlouis/resultats48/",i,"m.txt",sep=""))
# ss<-cox.zph( coxph( s_48 ~ greffe[,i],data=greffe),transform = "log")
# capture.output(ss, file=paste("C:/Users/adupont/Documents/projetstlouis/resultats48/",i,"ss.txt",sep=""))
# ssi<-cox.zph( coxph( s_48 ~ greffe[,i],data=greffe),transform = "identity")
# ssk<-cox.zph( coxph( s_48 ~ greffe[,i],data=greffe),transform = "km")
# capture.output(ssi, file=paste("C:/Users/adupont/Documents/projetstlouis/resultats48/",i,"identity.txt",sep=""))
# 
# ssu<-survdiff( s_48 ~ greffe[,i],data=greffe, rho=1)
# pw<-(1-pchisq(ssu$chisq, 1))
# pdf(paste("C:/Users/adupont/Documents/projetstlouis/resultats48/",i,"m.pdf",sep=""), width=4, height=4,onefile = TRUE)
# #par(mfrow=c(2,2))
# plot(ss[1:nlevels(greffe[,i])-1,])
# dev.off()
# pdf(paste("C:/Users/adupont/Documents/projetstlouis/resultats48/",i,"m2.pdf",sep=""), width=4, height=4,onefile = TRUE)
# 
# plot(survfit( s_48 ~greffe[,i],data=greffe),fun="log" ,lty=1:4, col=2:5)
# text(0, 0.7, paste("Test du logrank pondéré: p=", format(round(pw, 5), scien=F)), cex=1, adj=0)
# 
# dev.off()    
# 
# pdf(paste("C:/Users/adupont/Documents/projetstlouis/resultats48/",i,"m3.pdf",sep=""), width=4, height=4,onefile = TRUE)
# 
# plot(survfit( s_48 ~greffe[,i],data=greffe),fun="cloglog" ,lty=1:4, col=2:5)
# text(0, 0.7, paste("Test du logrank pondéré: p=", format(round(pw, 5), scien=F)), cex=1, adj=0)
# 
# dev.off() 
# pdf(paste("C:/Users/adupont/Documents/projetstlouis/resultats48/",i,"mi.pdf",sep=""), width=4, height=4,onefile = TRUE)
# 
# 
# plot(ssi[1:nlevels(greffe[,i])-1,])
# dev.off() 
# }
# 
# anapath<-summary(coxph( s ~ sex_dp2,data=greffe))
# anapath
# plot(survfit( s ~ strata(sex_dp2),data=greffe),fun="log" ,lty=1:4, col=2:5)
# e<-cox.zph(coxph( s ~ sex_dp2,data=greffe), transform="identity")
# plot(e, main="Identité")
# survdiff (s ~ sex_dp2, data=greffe, rho=1)
# 
# anapath_t<-cox.zph(,transform = "log")
# plot(anapath_t[1:nlevels(greffe$sex_dp2)-1,])
# 








### Rechute/decès uniquement chez ceux qui ont un rémission complète ou partielle

rec<-Surv(event=greffe$rechute[greffe$best_response_after_allo %in% c("CR")],
          time=greffe$delai_rechute[greffe$best_response_after_allo %in% c("CR")])

er <- survfit( rec ~ 1)


rer<-summary(er,censored = TRUE)
plot(er, xlab="Time in months",ylab="Probability")

censurem<-as.data.frame(cbind(rer$time[rer$n.event==0],rer$surv[rer$n.event==0] ))
colnames(censurem)<-c("time","ce")
evenementm<-as.data.frame(cbind(rer$time,rer$surv ))
colnames(evenementm)<-c("time","ev")
intervallem<-as.data.frame(cbind(rer$time,rer$upper
                                ,rer$lower ))
colnames(intervallem)<-c("time","haut","bas")
km_rechute<-ggplot()+ geom_step(data=evenementm,aes(x=time, y=ev),color="black", direction="hv")  +
  geom_ribbon(data=intervallem, aes(x=time, ymin=bas, ymax=haut),fill="grey",alpha="0.5")+
  geom_step(data=intervallem,aes(x=time, y=haut),color="black" ,direction="hv")+
  geom_step(data=intervallem,aes(x=time, y=bas),color="black", direction="hv")+
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

### pfs REMISSION COMPLETE


cpfscr<-Surv(event=greffe$rechute_progressionc[greffe$best_response_after_allo %in% c("CR")],
           time=greffe$delai_pfs[greffe$best_response_after_allo %in% c("CR")])

ecr <- survfit( cpfscr ~ 1)


reecr<-summary(ecr,censored = TRUE)
plot(ecr, xlab="Time in months",ylab="Probability")

censureecr<-as.data.frame(cbind(reecr$time[reecr$n.event==0],reecr$surv[reecr$n.event==0] ))
colnames(censureecr)<-c("time","ce")
evenementecr<-as.data.frame(cbind(reecr$time,reecr$surv ))
colnames(evenementecr)<-c("time","ev")
intervalleecr<-as.data.frame(cbind(reecr$time,reecr$upper
                                 ,reecr$lower ))
colnames(intervalleecr)<-c("time","haut","bas")


pfs_km_cr<-ggplot()+ #geom_step(data=evenement,aes(x=time, y=ev),color="black", direction="hv")  +
  geom_step(data=evenementecr,aes(x=time, y=ev),color="black", direction="hv")  +
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
  annotate(geom="text", x=90, y=0.60, label="PFS",
           color="black")+
  #annotate(geom="text", x=90, y=0.5, label="PFS",
           #color="blue")+
  theme_classic()

###♠ EFS chez remission complet 
efscr<-Surv(event=greffe$efs_statut[greffe$best_response_after_allo %in% c("CR")],
          time=as.numeric(greffe$delai_rechutepg[greffe$best_response_after_allo %in% c("CR")]))

efcr <- survfit( efscr ~ 1)


reefscr<-summary(efcr,censored = TRUE)
plot(efcr, xlab="Time in months",ylab="Probability")

censureefscr<-as.data.frame(cbind(reefscr$time[reefscr$n.event==0],reefscr$surv[reefscr$n.event==0] ))
colnames(censureefscr)<-c("time","ce")
evenementeefscr<-as.data.frame(cbind(reefscr$time,reefscr$surv ))
colnames(evenementeefscr)<-c("time","ev")
intervalleefscr<-as.data.frame(cbind(reefscr$time,reefscr$upper
                                   ,reefscr$lower ))
colnames(intervalleefscr)<-c("time","haut","bas")

efs_km_cr<-ggplot()+ #geom_step(data=evenement,aes(x=time, y=ev),color="black", direction="hv")  +
  geom_step(data=evenementeefscr,aes(x=time, y=ev),color="black", direction="hv")  +
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
  annotate(geom="text", x=90, y=0.50, label="EFS",
           color="black")+
  #annotate(geom="text", x=90, y=0.5, label="PFS",
  #color="blue")+
  theme_classic()









### PFS (rechute, progression, deces)
cpfs<-Surv(event=greffe$rechute_progressionc,time=greffe$delai_pfs)

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


pfs_km<-ggplot()+ geom_step(data=evenement,aes(x=time, y=ev),color="black", direction="hv")  +
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
  annotate(geom="text", x=90, y=0.5, label="PFS",
           color="blue")+
  theme_classic()

### EFS

efs<-Surv(event=greffe$efs_statut,time=as.numeric(greffe$delai_rechutepg))

ef <- survfit( efs ~ 1)


reefs<-summary(ef,censored = TRUE)
plot(ef, xlab="Time in months",ylab="Probability")

censureefs<-as.data.frame(cbind(reefs$time[reefs$n.event==0],reefs$surv[reefs$n.event==0] ))
colnames(censureefs)<-c("time","ce")
evenementeefs<-as.data.frame(cbind(reefs$time,reefs$surv ))
colnames(evenementeefs)<-c("time","ev")
intervalleefs<-as.data.frame(cbind(reefs$time,reefs$upper
                                 ,reefs$lower ))
colnames(intervalleefs)<-c("time","haut","bas")


efs_km<-ggplot()+ geom_step(data=evenement,aes(x=time, y=ev),color="black", direction="hv")  +
  geom_step(data=evenementeefs,aes(x=time, y=ev),color="blue", direction="hv",linetype = "dashed")  +
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

### Limite à 48 mois EFS ###

greffe$evenefs_48<-ifelse(greffe$efs_statut==1 & greffe$delai_rechutepg>48,0,greffe$efs_statut)
greffe$delai_rechutepg_48<-ifelse( greffe$delai_rechutepg>48,48,greffe$delai_rechutepg)

summary(as.numeric(greffe$delai_dc_48))
efs_48<-Surv(event=greffe$evenefs_48,time=as.numeric(greffe$delai_rechutepg_48))




afs_48 <- survfit( efs_48 ~ 1)
refs_48<-summary(afs_48,censored = TRUE)
plot(afs_48, xlab="Time in months",ylab="Probability")

censure_48e<-as.data.frame(cbind(refs_48$time[refs_48$n.event==0],refs_48$surv[refs_48$n.event==0] ))
colnames(censure_48e)<-c("time","ce")
evenement_48e<-as.data.frame(cbind(refs_48$time,refs_48$surv ))
colnames(evenement_48e)<-c("time","ev")
intervalle_48e<-as.data.frame(cbind(refs_48$time,refs_48$upper
                                   ,refs_48$lower ))
colnames(intervalle_48)<-c("time","haut","bas")

ggplot()+ geom_step(data=evenement_48,aes(x=time, y=ev),color="black", direction="hv")  +
  geom_step(data=evenement_48e,aes(x=time, y=ev),color="blue", direction="hv",linetype = "dashed")  +
  #geom_ribbon(data=intervalle, aes(x=time, ymin=bas, ymax=haut),fill="grey",alpha="0.5")+
  #geom_step(data=intervalle,aes(x=time, y=haut),color="black" ,direction="hv")+
  #geom_step(data=intervalle,aes(x=time, y=bas),color="black", direction="hv")+
  scale_x_continuous(breaks=c(0,10,20,30,40,50),expand = c(0, 0))+
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
  annotate(geom="text", x=40, y=0.7, label="OS",
           color="black")+
  annotate(geom="text", x=40, y=0.50, label="EFS",
           color="blue")+
  theme_classic()

# for (i in c("sex_donor","sex_patient","age_greffec",
#             "delai_dia_alloc","stade_dia","stade_diac",
#             "disease_status_at_transplantc2",
#             "disease_status_at_transplantc",
#             "disease_status_at_transplant","rechute_post_allo","karnofsky_greffec2","karnofsky_greffec3" ,
#             "previous_autoc",
#             "programme_autoalloc","rechute_prem_greffec",
#             "nbr_lignes_avt_alloc","nbr_lignes_avt_alloc2",
#             "donnor","hla_matchc","hla_match","sex_dp3",
#             "sex_dp2","cmv_dp2","stem_cell_source","tbi","intensite_condi","condit_details",
#             "manipu_cells","nbr_donneurc","manipu_cells",
#             "agvhd","agvhd3","agvhd_grade","cgvhd",
#             "cgvhd_grade","prise_greffe","cause_death_c","best_response_after_allo",
#             "relapse_progression_transplant_c","anapathc2")
# ){
#   
#   a<-summary(coxph( efs_48 ~ greffe[,i],data=greffe))
#   assign(paste(i, "efs_48", sep="_"),a)
#   capture.output(a, file=paste("C:/Users/adupont/Documents/projetstlouis/resultatsefs48/",i,"m.txt",sep=""))
#   ss<-cox.zph( coxph( s_48 ~ greffe[,i],data=greffe),transform = "log")
#   capture.output(ss, file=paste("C:/Users/adupont/Documents/projetstlouis/resultatsefs48/",i,"ss.txt",sep=""))
#   ssi<-cox.zph( coxph( s_48 ~ greffe[,i],data=greffe),transform = "identity")
#   ssk<-cox.zph( coxph( s_48 ~ greffe[,i],data=greffe),transform = "km")
#   capture.output(ssi, file=paste("C:/Users/adupont/Documents/projetstlouis/resultatsefs48/",i,"identity.txt",sep=""))
#   
#   ssu<-survdiff( s_48 ~ greffe[,i],data=greffe, rho=1)
#   pw<-(1-pchisq(ssu$chisq, 1))
#   pdf(paste("C:/Users/adupont/Documents/projetstlouis/resultatsefs48/",i,"m.pdf",sep=""), width=4, height=4,onefile = TRUE)
#   #par(mfrow=c(2,2))
#   plot(ss[1:nlevels(greffe[,i])-1,])
#   dev.off()
#   pdf(paste("C:/Users/adupont/Documents/projetstlouis/resultatsefs48/",i,"m2.pdf",sep=""), width=4, height=4,onefile = TRUE)
#   
#   plot(survfit( s_48 ~greffe[,i],data=greffe),fun="log" ,lty=1:4, col=2:5)
#   text(0, 0.7, paste("Test du logrank pondéré: p=", format(round(pw, 5), scien=F)), cex=1, adj=0)
#   
#   dev.off()    
#   
#   pdf(paste("C:/Users/adupont/Documents/projetstlouis/resultatsefs48/",i,"m3.pdf",sep=""), width=4, height=4,onefile = TRUE)
#   
#   plot(survfit( s_48 ~greffe[,i],data=greffe),fun="cloglog" ,lty=1:4, col=2:5)
#   text(0, 0.7, paste("Test du logrank pondéré: p=", format(round(pw, 5), scien=F)), cex=1, adj=0)
#   
#   dev.off() 
#   pdf(paste("C:/Users/adupont/Documents/projetstlouis/resultatsefs48/",i,"mi.pdf",sep=""), width=4, height=4,onefile = TRUE)
#   
#   
#   plot(ssi[1:nlevels(greffe[,i])-1,])
#   dev.off() 
# }












###TRM

trm<-Surv(event=greffe$cause_death_c2,time=as.numeric(greffe$delai_dc))

tr <- survfit( trm ~ 1)


trf<-summary(tr,censored = TRUE)
plot(tr, xlab="Time in months",ylab="Probability")

censuretrf<-as.data.frame(cbind(trf$time[trf$n.event==0],trf$surv[trf$n.event==0] ))
colnames(censuretrf)<-c("time","ce")
evenementtrf<-as.data.frame(cbind(trf$time,trf$surv ))
colnames(evenementtrf)<-c("time","ev")
intervalletrf<-as.data.frame(cbind(trf$time,trf$upper
                                   ,trf$lower ))
colnames(intervalletrf)<-c("time","haut","bas")


ggplot()+ geom_step(data=evenement,aes(x=time, y=ev),color="black", direction="hv")  +
  geom_step(data=evenementtrf,aes(x=time, y=ev),color="blue", direction="hv",linetype = "dashed")  +
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
  annotate(geom="text", x=90, y=0.75, label="TRM",
           color="blue")+
  theme_classic()

### Limite à 48 mois relm ###

greffe$eventrm_48<-ifelse(greffe$cause_death_c2==1 & greffe$delai_dc>48,0,greffe$cause_death_c2)
#greffe$delai_rechutepg_48<-ifelse( greffe$delai_rechutepg>48,48,greffe$delai_rechutepg)

summary(as.numeric(greffe$delai_dc_48))
trm_48<-Surv(event=greffe$eventrm_48,time=as.numeric(greffe$delai_dc_48))




rm_48 <- survfit(trm_48~ 1,data=greffe)
rrm_48<-summary(rm_48,censored = TRUE)
plot(rm_48, xlab="Time in months",ylab="Probability")

censure_48trm<-as.data.frame(cbind(rrm_48$time[rrm_48$n.event==0],rrm_48$surv[rrm_48$n.event==0] ))
colnames(censure_48trm)<-c("time","ce")
evenement_48trm<-as.data.frame(cbind(rrm_48$time,rrm_48$surv ))
colnames(evenement_48trm)<-c("time","ev")
intervalle_48trm<-as.data.frame(cbind(rrm_48$time,rrm_48$upper
                                    ,rrm_48$lower ))
colnames(intervalle_48trm)<-c("time","haut","bas")

ggplot()+ geom_step(data=evenement_48,aes(x=time, y=ev),color="black", direction="hv")  +
  geom_step(data=evenement_48trm,aes(x=time, y=ev),color="blue", direction="hv",linetype = "dashed")  +
  #geom_ribbon(data=intervalle, aes(x=time, ymin=bas, ymax=haut),fill="grey",alpha="0.5")+
  #geom_step(data=intervalle,aes(x=time, y=haut),color="black" ,direction="hv")+
  #geom_step(data=intervalle,aes(x=time, y=bas),color="black", direction="hv")+
  scale_x_continuous(breaks=c(0,10,20,30,40,50),expand = c(0, 0))+
  scale_size_manual(values=c(1.5,1.5))+
  
  geom_text(x=80, y=0.55, label="OS")+
  geom_text(x=80, y=0.45, label="TRM")+
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
  annotate(geom="text", x=40, y=0.65, label="OS",
           color="black")+
  annotate(geom="text", x=40, y=0.80, label="TRM",
           color="blue")+
  theme_classic()

# for (i in c("sex_donor","sex_patient","age_greffec",
#             "delai_dia_alloc","stade_dia","stade_diac",
#             "disease_status_at_transplantc2",
#             "disease_status_at_transplantc",
#             "disease_status_at_transplant","rechute_post_allo","karnofsky_greffec2","karnofsky_greffec3" ,
#             "previous_autoc",
#             "programme_autoalloc","rechute_prem_greffec",
#             "nbr_lignes_avt_alloc","nbr_lignes_avt_alloc2",
#             "donnor","hla_matchc","hla_match","sex_dp3",
#             "sex_dp2","cmv_dp2","stem_cell_source","tbi","intensite_condi","condit_details",
#             "manipu_cells","nbr_donneurc","manipu_cells",
#             "agvhd","agvhd3","agvhd_grade","cgvhd",
#             "cgvhd_grade","prise_greffe","cause_death_c","best_response_after_allo",
#             "relapse_progression_transplant_c","anapathc2")
# ){
#   
#   a<-summary(coxph( trm_48 ~ greffe[,i],data=greffe))
#   assign(paste(i, "rm_48", sep="_"),a)
#   capture.output(a, file=paste("F:/projetlo/resultatstrm48/",i,"m.txt",sep=""))
#   ss<-cox.zph( coxph( s_48 ~ greffe[,i],data=greffe),transform = "log")
#   capture.output(ss, file=paste("F:/projetlo/resultatstrm48/",i,"ss.txt",sep=""))
#   ssi<-cox.zph( coxph( s_48 ~ greffe[,i],data=greffe),transform = "identity")
#   ssk<-cox.zph( coxph( s_48 ~ greffe[,i],data=greffe),transform = "km")
#   capture.output(ssi, file=paste("F:/projetlo/resultatstrm48/",i,"identity.txt",sep=""))
#   
#   ssu<-survdiff( s_48 ~ greffe[,i],data=greffe, rho=1)
#   pw<-(1-pchisq(ssu$chisq, 1))
#   pdf(paste("F:/projetlo/resultatstrm48/",i,"m.pdf",sep=""), width=4, height=4,onefile = TRUE)
#   #par(mfrow=c(2,2))
#   plot(ss[1:nlevels(greffe[,i])-1,])
#   dev.off()
#   pdf(paste("F:/projetlo/resultatstrm48/",i,"m2.pdf",sep=""), width=4, height=4,onefile = TRUE)
#   
#   plot(survfit( s_48 ~greffe[,i],data=greffe),fun="log" ,lty=1:4, col=2:5)
#   text(0, 0.7, paste("Test du logrank pondéré: p=", format(round(pw, 5), scien=F)), cex=1, adj=0)
#   
#   dev.off()    
#   
#   pdf(paste("F:/projetlo/resultatstrm48/",i,"m3.pdf",sep=""), width=4, height=4,onefile = TRUE)
#   
#   plot(survfit( s_48 ~greffe[,i],data=greffe),fun="cloglog" ,lty=1:4, col=2:5)
#   text(0, 0.7, paste("Test du logrank pondéré: p=", format(round(pw, 5), scien=F)), cex=1, adj=0)
#   
#   dev.off() 
#   pdf(paste("F:/projetlo/resultatstrm48/",i,"mi.pdf",sep=""), width=4, height=4,onefile = TRUE)
#   
#   
#   plot(ssi[1:nlevels(greffe[,i])-1,])
#   dev.off() 
# }








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

modelcompetea<-cuminc(greffe$delai_rechute,greffe$rechute_dc,cencode=0,subset=greffe$best_response_after_allo %in%
                       c("CR"))
plot(modelcompetea,curvlab=c("Relapse","Death"),xlab="Time (months)", ylab="Probability")

gria<-as.data.frame(cbind(modelcompetea[[1]]$time,modelcompetea[[1]]$est-1.96*sqrt(modelcompetea[[1]]$var),modelcompetea[[1]]$est+1.96*sqrt(modelcompetea[[1]]$var)))
gcia<-as.data.frame(cbind(modelcompetea[[2]]$time,modelcompetea[[2]]$est-1.96*sqrt(modelcompetea[[2]]$var),modelcompetea[[2]]$est+1.96*sqrt(modelcompetea[[2]]$var)))


# pour rechute/p deces : pb d'un patient qui a une première greffe échec car la greffe n'a pas prise
# on la supprime et on ne garde que la seconde

modelcompeteb<-cuminc(greffe$delai_pfs,greffe$rechute_progression_dc,cencode=0)
plot(modelcompeteb,curvlab=c("Relapse/Progression","Death without relapse or progression"),xlab="Time (months)", ylab="Probability")

gri<-as.data.frame(cbind(modelcompeteb[[1]]$time,modelcompeteb[[1]]$est-1.96*sqrt(modelcompeteb[[1]]$var),modelcompeteb[[1]]$est+1.96*sqrt(modelcompeteb[[1]]$var)))
gci<-as.data.frame(cbind(modelcompeteb[[2]]$time,modelcompeteb[[2]]$est-1.96*sqrt(modelcompeteb[[2]]$var),modelcompeteb[[2]]$est+1.96*sqrt(modelcompeteb[[2]]$var)))




### compétions cause de deces 

modelcompetec<-cuminc(greffe$delai_dc,greffe$cause_death_c2,cencode=0)
plot(modelcompetec,curvlab=c("Related HSCT Death","Non-related HSCT Death"),xlab="Time (months)", ylab="Probability")


### GVHD , relapse, deces
modelcompete<-cuminc(greffe$delai_rechutepg,greffe$rechute_progression_ghvd_dc,cencode=0)
plot(modelcompete,curvlab=c("GVHD","Relapse/Progression","Death"),xlab="Time (months)", ylab="Probability")



