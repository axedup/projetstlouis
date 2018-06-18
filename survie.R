
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



plot(survfit(s~intensite_condi2,data=greffe))
### 
survfit( s ~ greffe$disease_status_at_transplant,conf.type = "log-log")



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
  scale_y_continuous(breaks=c(0,0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1),expand = c(0, 0),limits = c(0, 1))+
  #coord_cartesian(ylim=c(0,1))+
  theme_classic()+
  theme(legend.position="bottom",
        legend.title=element_blank())+
  ggtitle("")


test= data.frame(
  
  x = c(3.5,20,40,60,80,100 ),
  y = c(33,25,27,36,23,25),
  n=c(284,re$n.risk[which.min((re$time-20)<0)],re$n.risk[which.min((re$time-40)<0)],
      re$n.risk[which.min((re$time-60)<0)],re$n.risk[which.min((re$time-80)<0)],
      re$n.risk[which.min((re$time-100)<0)]),
  ypos=c(0.05,0.05,0.05,0.05,0.05,0.05)
  
)


for (ii in 1:nrow(test))
{
  #display numbers at each visit
  km_os=km_os+ annotation_custom(grob = textGrob(test$n[ii]),  
                                 xmin = test$x[ii], 
                                 xmax = test$x[ii], 
                                 ymin = test$ypos[ii], 
                                 ymax = test$ypos[ii])
  
}


km_os=km_os+annotation_custom(grob = textGrob("N at risk"),  
                              xmin = 6.5, 
                              xmax = 6.5, 
                              ymin = 0.1, 
                              ymax = 0.1)



gt <- ggplot_gtable(ggplot_build(km_os))
gt$layout$clip[gt$layout$name=="panel"] <- "off"

#grid.draw(gt)

# grid.text((paste("Largest x-value is","", sep = " ")),
#           x = unit(.2, "npc"), y = unit(.8, "npc"), just = c("left", "bottom"),
#           gp = gpar(fontface = "bold", fontsize = 18, col = "blue"))

# ff <- cph(Surv(event=greffe$deces,time=as.numeric(greffe$delai_dc)) ~1,data =greffe, surv=TRUE,x=TRUE,Y=TRUE)
# 
# survplot(ff)
# survplot(ff,1, label.curves = F, n.risk = T)


### OS PD###

spd<-Surv(event=greffe$deces[greffe$disease_status_at_transplantc=="PD"],time=as.numeric(greffe$delai_dc[greffe$disease_status_at_transplantc=="PD"]))

# greffe$delai_dc2<-greffe$age_greffe*30.25+greffe$delai_dc
# sbis<-Surv(event=greffe$deces,time2=greffe$delai_dc2,time=greffe$age_greffe*30.25)
# abis <- survfit( sbis ~ 1)
# plot(abis)


apd <- survfit( spd ~ 1,conf.type = "log-log")
repd<-summary(apd,censored = TRUE)

evenementpd<-as.data.frame(cbind(repd$time,repd$surv ))
colnames(evenementpd)<-c("time","ev")
intervallepd<-as.data.frame(cbind(repd$time,repd$upper
                                  ,repd$lower ))
colnames(intervallepd)<-c("time","haut","bas")

km_ospd<-ggplot()+ geom_step(data=evenementpd,aes(x=time, y=ev),color="black", direction="hv")  +
  #geom_ribbon(data=intervallepd, aes(x=time, ymin=bas, ymax=haut),linetype="dashed",fill="grey",alpha="0.4")+
  geom_step(data=intervallepd,aes(x=time, y=haut),color="black" ,direction="hv",linetype="dashed")+
  geom_step(data=intervallepd,aes(x=time, y=bas),color="black", direction="hv",linetype="dashed")+
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
  scale_y_continuous(breaks=c(0,0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1),expand = c(0, 0),limits = c(0, 1))+
  #coord_cartesian(ylim=c(0,1))+
  theme_classic()+
  theme(legend.position="bottom",
        legend.title=element_blank())+
  ggtitle("")


test= data.frame(

  x = c(3.5,20,40,60,80,100 ),
  y = c(33,25,27,36,23,25),
  n=c(32,repd$n.risk[which.min((repd$time-20)<0)],repd$n.risk[which.min((repd$time-40)<0)],
      repd$n.risk[which.min((repd$time-60)<0)],repd$n.risk[which.min((repd$time-80)<0)],
      repd$n.risk[which.min((repd$time-100)<0)]),
  ypos=c(0.05,0.05,0.05,0.05,0.05,0.05)

)


for (ii in 1:nrow(test))
{
  #display numbers at each visit
  km_ospd=km_ospd+ annotation_custom(grob = textGrob(test$n[ii]),
                                 xmin = test$x[ii],
                                 xmax = test$x[ii],
                                 ymin = test$ypos[ii],
                                 ymax = test$ypos[ii])

}


km_ospd=km_ospd+annotation_custom(grob = textGrob("N at risk"),
                              xmin = 6.5,
                              xmax = 6.5,
                              ymin = 0.1,
                              ymax = 0.1)



km_ospd <- ggplot_gtable(ggplot_build(km_ospd))
km_ospd$layout$clip[km_ospd$layout$name=="panel"] <- "off"





### OS cr1###

cros1<-Surv(event=greffe$deces[greffe$disease_status_at_transplant=="CR1"],time=as.numeric(greffe$delai_dc[greffe$disease_status_at_transplant=="CR1"]))

# greffe$delai_dc2<-greffe$age_greffe*30.25+greffe$delai_dc
# sbis<-Surv(event=greffe$deces,time2=greffe$delai_dc2,time=greffe$age_greffe*30.25)
# abis <- survfit( sbis ~ 1)
# plot(abis)


acr1 <- survfit( cros1 ~ 1,conf.type = "log-log")
recr1<-summary(acr1,censored = TRUE)

evenementcr1<-as.data.frame(cbind(recr1$time,recr1$surv ))
colnames(evenementcr1)<-c("time","ev")
intervallecr1<-as.data.frame(cbind(recr1$time,recr1$upper
                                  ,recr1$lower ))
colnames(intervallecr1)<-c("time","haut","bas")

km_oscr1<-ggplot()+ geom_step(data=evenementcr1,aes(x=time, y=ev),color="black", direction="hv")  +
  #geom_ribbon(data=intervallecr1, aes(x=time, ymin=bas, ymax=haut),linetype="dashed",fill="grey",alpha="0.4")+
  geom_step(data=intervallecr1,aes(x=time, y=haut),color="black" ,direction="hv",linetype="dashed")+
  geom_step(data=intervallecr1,aes(x=time, y=bas),color="black", direction="hv",linetype="dashed")+
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
  scale_y_continuous(breaks=c(0,0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1),expand = c(0, 0),limits = c(0, 1))+
  #coord_cartesian(ylim=c(0,1))+
  theme_classic()+
  theme(legend.position="bottom",
        legend.title=element_blank())+
  ggtitle("")


test= data.frame(
  
  x = c(3.5,20,40,60,80,100 ),
  y = c(33,25,27,36,23,25),
  n=c(94,recr1$n.risk[which.min((recr1$time-20)<0)],recr1$n.risk[which.min((recr1$time-40)<0)],
      recr1$n.risk[which.min((recr1$time-60)<0)],recr1$n.risk[which.min((recr1$time-80)<0)],
      recr1$n.risk[which.min((recr1$time-100)<0)]),
  ypos=c(0.05,0.05,0.05,0.05,0.05,0.05)
  
)


for (ii in 1:nrow(test))
{
  #display numbers at each visit
  km_oscr1=km_oscr1+ annotation_custom(grob = textGrob(test$n[ii]),
                                     xmin = test$x[ii],
                                     xmax = test$x[ii],
                                     ymin = test$ypos[ii],
                                     ymax = test$ypos[ii])
  
}


km_oscr1=km_oscr1+annotation_custom(grob = textGrob("N at risk"),
                                  xmin = 6.5,
                                  xmax = 6.5,
                                  ymin = 0.1,
                                  ymax = 0.1)



km_oscr1 <- ggplot_gtable(ggplot_build(km_oscr1))
km_oscr1$layout$clip[km_oscr1$layout$name=="panel"] <- "off"












### Limite à 60 mois ###

greffe$deces_60<-ifelse(greffe$deces==1 & greffe$delai_dc>60,0,greffe$deces)
greffe$delai_dc_60<-ifelse( greffe$delai_dc>60,60,greffe$delai_dc)

summary(as.numeric(greffe$delai_dc_60))
s_60<-Surv(event=greffe$deces_60,time=as.numeric(greffe$delai_dc_60))




a_60 <- survfit( s_60 ~ 1)
re_60<-summary(a_60,censored = TRUE)
plot(a_60, xlab="Time in months",ylab="Probability")

censure_60<-as.data.frame(cbind(re_60$time[re_60$n.event==0],re_60$surv[re_60$n.event==0] ))
colnames(censure_60)<-c("time","ce")
evenement_60<-as.data.frame(cbind(re_60$time,re_60$surv ))
colnames(evenement_60)<-c("time","ev")
intervalle_60<-as.data.frame(cbind(re_60$time,re_60$upper
                                ,re_60$lower ))
colnames(intervalle_60)<-c("time","haut","bas")
km_60<-ggplot()+ geom_step(data=evenement_60,aes(x=time, y=ev),color="black", direction="hv")  +
  geom_ribbon(data=intervalle_60, aes(x=time, ymin=bas, ymax=haut),fill="grey",alpha="0.5")+
  geom_step(data=intervalle_60,aes(x=time, y=haut),color="black" ,direction="hv")+
  geom_step(data=intervalle_60,aes(x=time, y=bas),color="black", direction="hv")+
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
cox<-coxph( s_60 ~ age_greffe,data=greffe)

res.m=residuals(cox, type="martingale")
X = as.matrix(greffe[,c("age_greffe")])
for (j in 1 ) {
  plot(X[,j], res.m, xlab=c("age", "prio")[j], ylab="
       residuals")
  abline(h=0, lty=2)
  lines(lowess(X[,j], res.m, iter=0))}



# cox<-coxph( csp_60 ~ age_greffe,data=greffe)
# 
# res.m=residuals(cox, type="martingale")
# X = as.matrix(greffe[,c("age_greffe")])
# for (j in 1 ) {
#   plot(X[,j][!is.na(greffe$cause_death_c360)], res.m, xlab=c("age", "prio")[j], ylab="
#        residuals")
#   abline(h=0, lty=2)
#   lines(lowess(X[,j][!is.na(greffe$cause_death_c360)], res.m, iter=0))}




### decès uniquement chez ceux qui ont un rémission complète ou partielle

rec<-Surv(event=greffe$deces[greffe$best_response_after_allo %in% c("CR")],
          time=greffe$delai_dc[greffe$best_response_after_allo %in% c("CR")])

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

###EFS chez remission complet 
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
  geom_step(data=evenementecr,aes(x=time, y=ev),color="black", direction="hv",linetype="dotdash")  +
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
  annotate(geom="text", x=90, y=0.6, label="RFS",
  color="black")+
  theme_classic()









### PFS (rechute, progression, deces)
cpfs<-Surv(event=greffe$rechute_progressionc,time=greffe$delai_pfs) # les 2 NA sur la rechute sont en NA au niveau du temps ##

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


### PFS selon l'intensite condi ###

econdi <- survfit( cpfs ~ intensite_condi2,data=greffe)


reecondi<-summary(econdi,censored = TRUE)



evenement1gv1s<-as.data.frame(cbind(reecondi$time[reecondi$strata=="intensite_condi2=MAC"],
                                    reecondi$surv[reecondi$strata=="intensite_condi2=MAC"] ))
colnames(evenement1gv1s)<-c("time","ev")

##evenement1gv1[order(evenement1gv1$time),]

debutev<-data.frame(time=0,ev=1)
evenement1gv1s<-rbind(debutev,evenement1gv1s)


evenement1gv2s<-as.data.frame(cbind(reecondi$time[reecondi$strata=="intensite_condi2=RIC/NMA"],
                                    reecondi$surv[reecondi$strata=="intensite_condi2=RIC/NMA"] ))
colnames(evenement1gv2s)<-c("time","ev")
debutev<-data.frame(time=0,ev=1)
evenement1gv2s<-rbind(debutev,evenement1gv2s)




# intervalle1evnt<-as.data.frame(cbind(repostevtabac$time[repostevtabac$strata=="bergerbasegv$tab1ev=0"],
#                                      repostevtabac$upper[repostevtabac$strata=="bergerbasegv$tab1ev=0"]
#                                      ,repostevtabac$lower[repostevtabac$strata=="bergerbasegv$tab1ev=0"] ))
# colnames(intervalle1evnt)<-c("time","haut","bas")




km_pfs_condi<-ggplot()+ geom_step(data=evenement1gv1s,aes(x=time, y=ev),color="black", direction="hv")  +
  #geom_ribbon(data=intervalle1evt, aes(x=time, ymin=bas, ymax=haut),linetype="dashed",fill="grey",alpha="0.4")+
  #geom_step(data=intervalle1evt,aes(x=time, y=haut),color="black" ,direction="hv",linetype="dashed")+
  #geom_step(data=intervalle1evt,aes(x=time, y=bas),color="black", direction="hv",linetype="dashed")+
  scale_x_continuous(breaks=c(0,20,40,60,80,100),expand = c(0, 0))+
  
  
  #geom_point(data=censure, aes(x=time, y=ce),shape=3,size=1 )+
  #ggtitle("DurÃ©e de vie des implants") +
  xlab("Time (Months)")+
  ylab("Probability")+
  
  geom_step(data=evenement1gv2s,aes(x=time, y=ev),color="#99CCFF", direction="hv")  +
  
  #geom_ribbon(data=intervalle1evnt, aes(x=time, ymin=bas, ymax=haut),linetype="dashed",fill="#99CCFF",alpha="0.4")+
  #geom_step(data=intervalle1evnt,aes(x=time, y=haut),color="#99CCFF" ,direction="hv",linetype="dashed")+
  #geom_step(data=intervalle1evnt,aes(x=time, y=bas),color="#99CCFF", direction="hv",linetype="dashed")+
  
  
  #scale_colour_manual("",values = c("Rupture"="blue", "Autres causes"="black"))+
  
  annotate(geom="text", x=12, y=0.5, label="RIC/NMA",color="#99CCFF", size=4)+
  annotate(geom="text", x=12, y=0.8, label="MAC",color="black", size=4) +
  coord_cartesian(ylim=c(0,1)) +
  scale_y_continuous(breaks=c(0,0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1),expand = c(0, 0),limits = c(0, 1))+
  #coord_cartesian(ylim=c(0,1))+
  theme_classic()+
  theme(legend.position="bottom",
        legend.title=element_blank())+
  ggtitle("")



# tests3= data.frame(
#   
#   x = c(0.4,1.1,3,5,10,15),
#   y = c(33,25,27,36,23,25),
#   n=c(repostgvtypes$n.risk[repostgvtypes$strata=="bergerbasegv$ref1GV=3"][1],
#       repostgvtypes$n.risk[repostgvtypes$strata=="bergerbasegv$ref1GV=3"][which.min((repostgvtypes$time[repostgvtypes$strata=="bergerbasegv$ref1GV=3"]-1)<0)],
#       repostgvtypes$n.risk[repostgvtypes$strata=="bergerbasegv$ref1GV=3"][which.min((repostgvtypes$time[repostgvtypes$strata=="bergerbasegv$ref1GV=3"]-3)<0)],
#       repostgvtypes$n.risk[repostgvtypes$strata=="bergerbasegv$ref1GV=3"][which.min((repostgvtypes$time[repostgvtypes$strata=="bergerbasegv$ref1GV=3"]-5)<0)],
#       repostgvtypes$n.risk[repostgvtypes$strata=="bergerbasegv$ref1GV=3"][which.min((repostgvtypes$time[repostgvtypes$strata=="bergerbasegv$ref1GV=3"]-10)<0)],
#       repostgvtypes$n.risk[repostgvtypes$strata=="bergerbasegv$ref1GV=3"][which.min((repostgvtypes$time[repostgvtypes$strata=="bergerbasegv$ref1GV=3"]-15)<0)]),
#   ypos=c(1,1,1,1,1,1)
#   
# )
# 
# 
# tests2= data.frame(
#   
#   x = c(0.4,1.1,3,5,10,15),
#   y = c(33,25,27,36,23,25),
#   n=c(repostgvtypes$n.risk[repostgvtypes$strata=="bergerbasegv$ref1GV=2"][1],
#       repostgvtypes$n.risk[repostgvtypes$strata=="bergerbasegv$ref1GV=2"][which.min((repostgvtypes$time[repostgvtypes$strata=="bergerbasegv$ref1GV=2"]-1)<0)],
#       repostgvtypes$n.risk[repostgvtypes$strata=="bergerbasegv$ref1GV=2"][which.min((repostgvtypes$time[repostgvtypes$strata=="bergerbasegv$ref1GV=2"]-3)<0)],
#       repostgvtypes$n.risk[repostgvtypes$strata=="bergerbasegv$ref1GV=2"][which.min((repostgvtypes$time[repostgvtypes$strata=="bergerbasegv$ref1GV=2"]-5)<0)],
#       0,0),
#   ypos=c(2,2,2,2,2,2)
#   
# )
# 
# 
# 
# tests1= data.frame(
#   
#   x = c(0.4,1.1,3,5,10,15),
#   y = c(33,25,27,36,23,25),
#   n=c(repostgvtypes$n.risk[repostgvtypes$strata=="bergerbasegv$ref1GV=1"][1],
#       repostgvtypes$n.risk[repostgvtypes$strata=="bergerbasegv$ref1GV=1"][which.min((repostgvtypes$time[repostgvtypes$strata=="bergerbasegv$ref1GV=1"]-1)<0)],
#       repostgvtypes$n.risk[repostgvtypes$strata=="bergerbasegv$ref1GV=1"][which.min((repostgvtypes$time[repostgvtypes$strata=="bergerbasegv$ref1GV=1"]-3)<0)],
#       repostgvtypes$n.risk[repostgvtypes$strata=="bergerbasegv$ref1GV=1"][which.min((repostgvtypes$time[repostgvtypes$strata=="bergerbasegv$ref1GV=1"]-5)<0)],
#       0,0),
#   ypos=c(3,3,3,3,3,3)
#   
# )
# 
# 
# df <- data.frame()
# f<-ggplot(df) + geom_point() + scale_y_continuous(limits=c(0, 3.5),expand=c(0,0),breaks=c(1,2,3),
#                                                   labels=c("Sympatectomie", "Pontage", "Endov"))+
#   scale_x_continuous(limits=c(0, 16),expand=c(0,0),breaks=c(0,5,10,15,20,25,30))+
#   theme_classic()+ theme(legend.position="bottom",legend.title=element_blank(),
#                          axis.ticks.y=element_line(colour="black"),
#                          axis.text.y = element_text(colour = "black"),
#                          axis.line.y = element_line(colour = "white")
#                          
#   )
# 
# 
# test<-rbind(tests1,tests2,tests3)
# 
# #f$theme$axis.line$colour<-"white"
# 
# for (ii in 1:nrow(test))
# {
#   #display numbers at each visit
#   f=f+ annotation_custom(grob = text_grob(test$n[ii],size=10),  
#                          xmin = test$x[ii], 
#                          xmax = test$x[ii], 
#                          ymin = test$ypos[ii], 
#                          ymax = test$ypos[ii])
#   
# }
# 
# 
# km_ev_gvs<-ggarrange(km_ev_gvs,f, ncol = 1, nrow = 2,heights = c(2,0.7),align = c("v"))
# km_ev_gvs
# 
# 
# 











### PFS 60 jours

greffe$pfs_60<-ifelse(greffe$rechute_progressionc==1 & greffe$delai_pfs>60,0,greffe$rechute_progressionc)
greffe$delai_pfs_60<-ifelse( greffe$delai_pfs>60,60,greffe$delai_pfs)

summary(as.numeric(greffe$delai_pfs_60))
pfss_60<-Surv(event=greffe$pfs_60,time=as.numeric(greffe$delai_pfs_60))

cox<-coxph( pfss_60 ~ age_greffe,data=greffe)

res.m=residuals(cox, type="martingale")
X = as.matrix(greffe[,c("age_greffe")])
for (j in 1 ) {
  plot(X[,j][!is.na(greffe$pfs_60)], res.m, xlab=c("age", "prio")[j], ylab="
       residuals")
  abline(h=0, lty=2)
  lines(lowess(X[,j][!is.na(greffe$pfs_60)], res.m, iter=0))}


# a_60 <- survfit( s_60 ~ 1)
# re_60<-summary(a_60,censored = TRUE)
# plot(a_60, xlab="Time in months",ylab="Probability")




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
  geom_step(data=evenementeefs,aes(x=time, y=ev),color="black", direction="hv",linetype = "dashed")  +
  geom_step(data=evenemente,aes(x=time, y=ev),color="black", direction="hv",linetype = "dotdash")  +
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
  annotate(geom="text", x=90, y=0.45, label="GRFS",
           color="black")+
  annotate(geom="text", x=90, y=0.51, label="EFS",
           color="black")+
  theme_classic()

### Limite à 60 mois EFS ###

greffe$evenefs_60<-ifelse(greffe$efs_statut==1 & greffe$delai_rechutepg>60,0,greffe$efs_statut)
greffe$delai_rechutepg_60<-ifelse( greffe$delai_rechutepg>60,60,greffe$delai_rechutepg)

summary(as.numeric(greffe$delai_dc_60))
efs_60<-Surv(event=greffe$evenefs_60,time=as.numeric(greffe$delai_rechutepg_60))




afs_60 <- survfit( efs_60 ~ 1)
refs_60<-summary(afs_60,censored = TRUE)
plot(afs_60, xlab="Time in months",ylab="Probability")

censure_60e<-as.data.frame(cbind(refs_60$time[refs_60$n.event==0],refs_60$surv[refs_60$n.event==0] ))
colnames(censure_60e)<-c("time","ce")
evenement_60e<-as.data.frame(cbind(refs_60$time,refs_60$surv ))
colnames(evenement_60e)<-c("time","ev")
intervalle_60e<-as.data.frame(cbind(refs_60$time,refs_60$upper
                                   ,refs_60$lower ))
colnames(intervalle_60)<-c("time","haut","bas")

# ggplot()+ geom_step(data=evenement_48,aes(x=time, y=ev),color="black", direction="hv")  +
#   geom_step(data=evenement_48e,aes(x=time, y=ev),color="blue", direction="hv",linetype = "dashed")  +
#   #geom_ribbon(data=intervalle, aes(x=time, ymin=bas, ymax=haut),fill="grey",alpha="0.5")+
#   #geom_step(data=intervalle,aes(x=time, y=haut),color="black" ,direction="hv")+
#   #geom_step(data=intervalle,aes(x=time, y=bas),color="black", direction="hv")+
#   scale_x_continuous(breaks=c(0,10,20,30,40,50),expand = c(0, 0))+
#   scale_size_manual(values=c(1.5,1.5))+
#   
#   geom_text(x=80, y=0.55, label="OS")+
#   geom_text(x=80, y=0.45, label="EFS")+
#   #geom_point(data=censure, aes(x=time, y=ce),shape=3,size=1 )+
#   #ggtitle("Durée de vie des implants") +
#   xlab("Time (Months)")+
#   ylab("Probability")+
#   #geom_step(data=gri,aes(x=time, y=ics), direction="hv",color="gray10",linetype="dashed" )+
#   #  geom_step(data=gri,aes(x=time, y=ici), direction="hv",color="gray10",linetype="dashed" )+
#   #  geom_step(data=gci,aes(x=time, y=ics), direction="hv",color="black",linetype="dashed" )+
#   #  geom_step(data=gci,aes(x=time, y=ici), direction="hv",color="black",linetype="dashed" )+
#   #
#   #scale_colour_manual("",values = c("Rupture"="blue", "Autres causes"="black"))+annotate(geom="text", x=52, y=0.91, label="Tous les parcours",color="black", size=4)+coord_cartesian(ylim=c(0,1)) +
#   scale_y_continuous(breaks=c(0,0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1),expand = c(0, 0))+
#   coord_cartesian(ylim=c(0,1))+
#   annotate(geom="text", x=40, y=0.7, label="OS",
#            color="black")+
#   annotate(geom="text", x=40, y=0.50, label="EFS",
#            color="blue")+
#   theme_classic()

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












###TRM : FAUX 
# 
# trm<-Surv(event=greffe$cause_death_c2,time=as.numeric(greffe$delai_dc))
# 
# tr <- survfit( trm ~ 1)
# 
# 
# trf<-summary(tr,censored = TRUE)
# plot(tr, xlab="Time in months",ylab="Probability")
# 
# censuretrf<-as.data.frame(cbind(trf$time[trf$n.event==0],trf$surv[trf$n.event==0] ))
# colnames(censuretrf)<-c("time","ce")
# evenementtrf<-as.data.frame(cbind(trf$time,trf$surv ))
# colnames(evenementtrf)<-c("time","ev")
# intervalletrf<-as.data.frame(cbind(trf$time,trf$upper
#                                    ,trf$lower ))
# colnames(intervalletrf)<-c("time","haut","bas")
# 
# 
# ggplot()+ geom_step(data=evenement,aes(x=time, y=ev),color="black", direction="hv")  +
#   geom_step(data=evenementtrf,aes(x=time, y=ev),color="blue", direction="hv",linetype = "dashed")  +
#   #geom_ribbon(data=intervalle, aes(x=time, ymin=bas, ymax=haut),fill="grey",alpha="0.5")+
#   #geom_step(data=intervalle,aes(x=time, y=haut),color="black" ,direction="hv")+
#   #geom_step(data=intervalle,aes(x=time, y=bas),color="black", direction="hv")+
#   scale_x_continuous(breaks=c(0,20,40,60,80,100),expand = c(0, 0))+
#   scale_size_manual(values=c(1.5,1.5))+
#   
#   geom_text(x=80, y=0.55, label="OS")+
#   geom_text(x=80, y=0.45, label="EFS")+
#   #geom_point(data=censure, aes(x=time, y=ce),shape=3,size=1 )+
#   #ggtitle("Durée de vie des implants") +
#   xlab("Time (Months)")+
#   ylab("Probability")+
#   #geom_step(data=gri,aes(x=time, y=ics), direction="hv",color="gray10",linetype="dashed" )+
#   #  geom_step(data=gri,aes(x=time, y=ici), direction="hv",color="gray10",linetype="dashed" )+
#   #  geom_step(data=gci,aes(x=time, y=ics), direction="hv",color="black",linetype="dashed" )+
#   #  geom_step(data=gci,aes(x=time, y=ici), direction="hv",color="black",linetype="dashed" )+
#   #
#   #scale_colour_manual("",values = c("Rupture"="blue", "Autres causes"="black"))+annotate(geom="text", x=52, y=0.91, label="Tous les parcours",color="black", size=4)+coord_cartesian(ylim=c(0,1)) +
#   scale_y_continuous(breaks=c(0,0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1),expand = c(0, 0))+
#   coord_cartesian(ylim=c(0,1))+
#   annotate(geom="text", x=90, y=0.55, label="OS",
#            color="black")+
#   annotate(geom="text", x=90, y=0.75, label="TRM",
#            color="blue")+
#   theme_classic()
# 
# ### Limite à 48 mois relm ###
# 
# greffe$eventrm_48<-ifelse(greffe$cause_death_c2==1 & greffe$delai_dc>48,0,greffe$cause_death_c2)
# #greffe$delai_rechutepg_48<-ifelse( greffe$delai_rechutepg>48,48,greffe$delai_rechutepg)
# 
# summary(as.numeric(greffe$delai_dc_48))
# trm_48<-Surv(event=greffe$eventrm_48,time=as.numeric(greffe$delai_dc_48))
# 
# 
# 
# 
# rm_48 <- survfit(trm_48~ 1,data=greffe)
# rrm_48<-summary(rm_48,censored = TRUE)
# plot(rm_48, xlab="Time in months",ylab="Probability")
# 
# censure_48trm<-as.data.frame(cbind(rrm_48$time[rrm_48$n.event==0],rrm_48$surv[rrm_48$n.event==0] ))
# colnames(censure_48trm)<-c("time","ce")
# evenement_48trm<-as.data.frame(cbind(rrm_48$time,rrm_48$surv ))
# colnames(evenement_48trm)<-c("time","ev")
# intervalle_48trm<-as.data.frame(cbind(rrm_48$time,rrm_48$upper
#                                     ,rrm_48$lower ))
# colnames(intervalle_48trm)<-c("time","haut","bas")
# 
# ggplot()+ geom_step(data=evenement_48,aes(x=time, y=ev),color="black", direction="hv")  +
#   geom_step(data=evenement_48trm,aes(x=time, y=ev),color="blue", direction="hv",linetype = "dashed")  +
#   #geom_ribbon(data=intervalle, aes(x=time, ymin=bas, ymax=haut),fill="grey",alpha="0.5")+
#   #geom_step(data=intervalle,aes(x=time, y=haut),color="black" ,direction="hv")+
#   #geom_step(data=intervalle,aes(x=time, y=bas),color="black", direction="hv")+
#   scale_x_continuous(breaks=c(0,10,20,30,40,50),expand = c(0, 0))+
#   scale_size_manual(values=c(1.5,1.5))+
#   
#   geom_text(x=80, y=0.55, label="OS")+
#   geom_text(x=80, y=0.45, label="TRM")+
#   #geom_point(data=censure, aes(x=time, y=ce),shape=3,size=1 )+
#   #ggtitle("Durée de vie des implants") +
#   xlab("Time (Months)")+
#   ylab("Probability")+
#   #geom_step(data=gri,aes(x=time, y=ics), direction="hv",color="gray10",linetype="dashed" )+
#   #  geom_step(data=gri,aes(x=time, y=ici), direction="hv",color="gray10",linetype="dashed" )+
#   #  geom_step(data=gci,aes(x=time, y=ics), direction="hv",color="black",linetype="dashed" )+
#   #  geom_step(data=gci,aes(x=time, y=ici), direction="hv",color="black",linetype="dashed" )+
#   #
#   #scale_colour_manual("",values = c("Rupture"="blue", "Autres causes"="black"))+annotate(geom="text", x=52, y=0.91, label="Tous les parcours",color="black", size=4)+coord_cartesian(ylim=c(0,1)) +
#   scale_y_continuous(breaks=c(0,0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1),expand = c(0, 0))+
#   coord_cartesian(ylim=c(0,1))+
#   annotate(geom="text", x=40, y=0.65, label="OS",
#            color="black")+
#   annotate(geom="text", x=40, y=0.80, label="TRM",
#            color="blue")+
#   theme_classic()

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

# gvhd<-Surv(event=greffe$gvhd,time=greffe$delai_gvhd)
# 
# e <- survfit( gvhd ~ 1)
# ree<-summary(e,censored = TRUE)
# plot(e, xlab="Time in months",ylab="Probability")
# 
# censuree<-as.data.frame(cbind(ree$time[ree$n.event==0],ree$surv[ree$n.event==0] ))
# colnames(censuree)<-c("time","ce")
# evenemente<-as.data.frame(cbind(ree$time,ree$surv ))
# colnames(evenemente)<-c("time","ev")
# intervallee<-as.data.frame(cbind(ree$time,ree$upper
#                                  ,ree$lower ))
# colnames(intervallee)<-c("time","haut","bas")
# 
# 
# ggplot()+ 
#   geom_step(data=evenemente,aes(x=time, y=ev),color="black", direction="hv")  +
#   #geom_step(data=evenemente,aes(x=time, y=ev),color="blue", direction="hv",linetype = "dashed")  +
#   #geom_ribbon(data=intervalle, aes(x=time, ymin=bas, ymax=haut),fill="grey",alpha="0.5")+
#   #geom_step(data=intervalle,aes(x=time, y=haut),color="black" ,direction="hv")+
#   #geom_step(data=intervalle,aes(x=time, y=bas),color="black", direction="hv")+
#   scale_x_continuous(breaks=c(0,20,40,60,80,100),expand = c(0, 0))+
#   scale_size_manual(values=c(1.5,1.5))+
#   
#   geom_text(x=80, y=0.55, label="OS")+
#   geom_text(x=80, y=0.45, label="EFS")+
#   #geom_point(data=censure, aes(x=time, y=ce),shape=3,size=1 )+
#   #ggtitle("Durée de vie des implants") +
#   xlab("Time (Months)")+
#   ylab("Probability")+
#   #geom_step(data=gri,aes(x=time, y=ics), direction="hv",color="gray10",linetype="dashed" )+
#   #  geom_step(data=gri,aes(x=time, y=ici), direction="hv",color="gray10",linetype="dashed" )+
#   #  geom_step(data=gci,aes(x=time, y=ics), direction="hv",color="black",linetype="dashed" )+
#   #  geom_step(data=gci,aes(x=time, y=ici), direction="hv",color="black",linetype="dashed" )+
#   #
#   #scale_colour_manual("",values = c("Rupture"="blue", "Autres causes"="black"))+annotate(geom="text", x=52, y=0.91, label="Tous les parcours",color="black", size=4)+coord_cartesian(ylim=c(0,1)) +
#   scale_y_continuous(breaks=c(0,0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1),expand = c(0, 0))+
#   coord_cartesian(ylim=c(0,1))+
#   annotate(geom="text", x=70, y=0.55, label="GVHD", color="black")
#  # annotate(geom="text", x=90, y=0.45, label="EFS", color="blue")

### TRM#


trrm<-Surv(event=greffe$cause_death_c3,time=as.numeric(greffe$delai_dc))

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


km_trm<-ggplot()+ geom_step(data=evenementtrm,aes(x=time, y=1-ev),color="black", direction="hv")  +
  geom_ribbon(data=intervalletrm, aes(x=time, ymin=1-bas, ymax=1-haut),linetype="dashed",fill="grey",alpha="0.4")+
  geom_step(data=intervalletrm,aes(x=time, y=1-haut),color="black" ,direction="hv")+
  geom_step(data=intervalletrm,aes(x=time, y=1-bas),color="black", direction="hv")+
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
  scale_y_continuous(breaks=c(0,0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1),expand = c(0, 0),limits = c(0, 1))+
  #coord_cartesian(ylim=c(0,1))+
  theme_classic()+
  theme(legend.position="bottom",
        legend.title=element_blank())+
  ggtitle("")


### TRM pd ###

trrmpd<-Surv(event=greffe$cause_death_c3[greffe$disease_status_at_transplantc=="PD"],
             time=as.numeric(greffe$delai_dc[greffe$disease_status_at_transplantc=="PD"]))

# greffe$delai_dc2<-greffe$age_greffe*30.25+greffe$delai_dc
# sbis<-Surv(event=greffe$deces,time2=greffe$delai_dc2,time=greffe$age_greffe*30.25)
# abis <- survfit( sbis ~ 1)
# plot(abis)


atrmpd <- survfit( trrmpd ~ 1,conf.type = "log-log")
retrmpd<-summary(atrmpd,censored = TRUE)
plot(atrmpd, xlab="Time in months",ylab="Probability",conf.int = FALSE)


### TRM CR1 ###

trrmcr1<-Surv(event=greffe$cause_death_c3[greffe$disease_status_at_transplant=="CR1"],
             time=as.numeric(greffe$delai_dc[greffe$disease_status_at_transplant=="CR1"]))

# greffe$delai_dc2<-greffe$age_greffe*30.25+greffe$delai_dc
# sbis<-Surv(event=greffe$deces,time2=greffe$delai_dc2,time=greffe$age_greffe*30.25)
# abis <- survfit( sbis ~ 1)
# plot(abis)


atrmcr1 <- survfit( trrmcr1 ~ 1,conf.type = "log-log")
retrmcr1<-summary(atrmcr1,censored = TRUE)
plot(atrmcr1, xlab="Time in months",ylab="Probability",conf.int=FALSE)





#### TRM selon condi###


trmcondi <- survfit( trrm ~ intensite_condi2,data=greffe)
survdiff(trrm ~ intensite_condi3,data=greffe)

reeconditrm<-summary(trmcondi,censored = TRUE)



evenement1gv1strm<-as.data.frame(cbind(reeconditrm$time[reeconditrm$strata=="intensite_condi2=MAC"],
                                    reeconditrm$surv[reeconditrm$strata=="intensite_condi2=MAC"] ))
colnames(evenement1gv1strm)<-c("time","ev")

##evenement1gv1[order(evenement1gv1$time),]

debutev<-data.frame(time=0,ev=1)
evenement1gv1strm<-rbind(debutev,evenement1gv1strm)


evenement1gv2strm<-as.data.frame(cbind(reeconditrm$time[reeconditrm$strata=="intensite_condi2=RIC/NMA"],
                                    reeconditrm$surv[reeconditrm$strata=="intensite_condi2=RIC/NMA"] ))
colnames(evenement1gv2strm)<-c("time","ev")
debutev<-data.frame(time=0,ev=1)
evenement1gv2strm<-rbind(debutev,evenement1gv2strm)




# intervalle1evnt<-as.data.frame(cbind(repostevtabac$time[repostevtabac$strata=="bergerbasegv$tab1ev=0"],
#                                      repostevtabac$upper[repostevtabac$strata=="bergerbasegv$tab1ev=0"]
#                                      ,repostevtabac$lower[repostevtabac$strata=="bergerbasegv$tab1ev=0"] ))
# colnames(intervalle1evnt)<-c("time","haut","bas")




km_trm_condi<-ggplot()+ geom_step(data=evenement1gv1strm,aes(x=time, y=1-ev),color="black", direction="hv")  +
  #geom_ribbon(data=intervalle1evt, aes(x=time, ymin=bas, ymax=haut),linetype="dashed",fill="grey",alpha="0.4")+
  #geom_step(data=intervalle1evt,aes(x=time, y=haut),color="black" ,direction="hv",linetype="dashed")+
  #geom_step(data=intervalle1evt,aes(x=time, y=bas),color="black", direction="hv",linetype="dashed")+
  scale_x_continuous(breaks=c(0,10,20,30),expand = c(0, 0))+
  
  
  #geom_point(data=censure, aes(x=time, y=ce),shape=3,size=1 )+
  #ggtitle("DurÃ©e de vie des implants") +
  xlab("Time (Months)")+
  ylab("Probability")+
  
  geom_step(data=evenement1gv2strm,aes(x=time, y=1-ev),color="#99CCFF", direction="hv")  +
  
  #geom_ribbon(data=intervalle1evnt, aes(x=time, ymin=bas, ymax=haut),linetype="dashed",fill="#99CCFF",alpha="0.4")+
  #geom_step(data=intervalle1evnt,aes(x=time, y=haut),color="#99CCFF" ,direction="hv",linetype="dashed")+
  #geom_step(data=intervalle1evnt,aes(x=time, y=bas),color="#99CCFF", direction="hv",linetype="dashed")+
  
  
  #scale_colour_manual("",values = c("Rupture"="blue", "Autres causes"="black"))+
  
  annotate(geom="text", x=20, y=0.4, label="RIC/NMA",color="#99CCFF", size=4)+
  annotate(geom="text", x=20, y=0.10, label="MAC",color="black", size=4) +
  coord_cartesian(ylim=c(0,1),xlim=c(0,35)) +
  scale_y_continuous(breaks=c(0,0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1),expand = c(0, 0),limits = c(0, 1))+
  #coord_cartesian(ylim=c(0,1))+
  theme_classic()+
  theme(legend.position="bottom",
        legend.title=element_blank())+
  ggtitle("")



tests3= data.frame(
  
  x = c(1,10,20,30),
  y = c(33,25,27,36),
  n=c(reeconditrm$n.risk[reeconditrm$strata=="intensite_condi2=MAC"][1],

      reeconditrm$n.risk[reeconditrm$strata=="intensite_condi2=MAC"][which.min((reeconditrm$time[reeconditrm$strata=="intensite_condi2=MAC"]-10)<0)],
      reeconditrm$n.risk[reeconditrm$strata=="intensite_condi2=MAC"][which.min((reeconditrm$time[reeconditrm$strata=="intensite_condi2=MAC"]-20)<0)],
      reeconditrm$n.risk[reeconditrm$strata=="intensite_condi2=MAC"][which.min((reeconditrm$time[reeconditrm$strata=="intensite_condi2=MAC"]-30)<0)]
     ),
  ypos=c(1,1,1,1)
  
)






tests2= data.frame(
  
  x = c(1,10,20,30),
  y = c(33,25,27,36),
  n=c(reeconditrm$n.risk[reeconditrm$strata=="intensite_condi2=RIC/NMA"][1],
      
      reeconditrm$n.risk[reeconditrm$strata=="intensite_condi2=RIC/NMA"][which.min((reeconditrm$time[reeconditrm$strata=="intensite_condi2=RIC/NMA"]-10)<0)],
      reeconditrm$n.risk[reeconditrm$strata=="intensite_condi2=RIC/NMA"][which.min((reeconditrm$time[reeconditrm$strata=="intensite_condi2=RIC/NMA"]-20)<0)],
      reeconditrm$n.risk[reeconditrm$strata=="intensite_condi2=RIC/NMA"][which.min((reeconditrm$time[reeconditrm$strata=="intensite_condi2=RIC/NMA"]-30)<0)]
  ),
  ypos=c(2,2,2,2)
  
)


df <- data.frame()
f<-ggplot(df) + geom_point() + scale_y_continuous(limits=c(0, 3.5),expand=c(0,0),breaks=c(1,2),
                                                  labels=c("MAC","RIC/MNA"))+
  scale_x_continuous(limits=c(0, 35),expand=c(0,0),breaks=c(0,10,20,30))+
  theme_classic()+ theme(legend.position="bottom",legend.title=element_blank(),
                         axis.ticks.y=element_line(colour="black"),
                         axis.text.y = element_text(colour = "black"),
                         axis.line.y = element_line(colour = "white")
                         
  )


test<-rbind(tests2,tests3)

#f$theme$axis.line$colour<-"white"

for (ii in 1:nrow(test))
{
  #display numbers at each visit
  f=f+ annotation_custom(grob = text_grob(test$n[ii],size=10),  
                         xmin = test$x[ii], 
                         xmax = test$x[ii], 
                         ymin = test$ypos[ii], 
                         ymax = test$ypos[ii])
  
}


km_trm_condi<-ggarrange(km_trm_condi,f, ncol = 1, nrow = 2,heights = c(2,0.7),align = c("v"))
km_trm_condi

### Sur vie post gvhd chronique


surviecgvhd<-Surv(event=greffe$deces,time=as.numeric(greffe$delai_gvhdc_deces))
surviecgvhdsurv<-survfit(surviecgvhd~1)
surviecgvhdsurvsummary<-summary(surviecgvhdsurv,censored=TRUE)


pfscgvhd<-Surv(event=greffe$rechute_progressionc,time=as.numeric(greffe$delai_gvhdc_rechutep))
pfscgvhdsurv<-survfit(pfscgvhd~1)
pfscgvhdsurvsummary<-summary(pfscgvhdsurv,censored=TRUE)


evenementoscg<-as.data.frame(cbind(surviecgvhdsurvsummary$time,surviecgvhdsurvsummary$surv ))
colnames(evenementoscg)<-c("time","ev")
intervalleoscg<-as.data.frame(cbind(surviecgvhdsurvsummary$time,surviecgvhdsurvsummary$upper
                                    ,surviecgvhdsurvsummary$lower ))
colnames(intervalleoscg)<-c("time","haut","bas")


km_cgvhd<-ggplot()+ geom_step(data=evenementoscg,aes(x=time, y=ev),color="black", direction="hv")  +
  geom_ribbon(data=intervalleoscg ,aes(x=time, ymin=bas, ymax=haut),linetype="dashed",fill="grey",alpha="0.4")+
  geom_step(data=intervalleoscg,aes(x=time, y=haut),color="black" ,direction="hv")+
  geom_step(data=intervalleoscg,aes(x=time, y=bas),color="black", direction="hv")+
  scale_x_continuous(breaks=c(0,20,40,60,80),expand = c(0,0),limits = c(0,85))+
  scale_size_manual(values=c(1.5,1.5))+
  
  #geom_point(data=censure, aes(x=time, y=ce),shape=3,size=1 )+
  #ggtitle("Durée de vie des implants") +
  xlab("Time from cgvhd (Months)")+
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


test= data.frame(
  
  x = c(3.5,20,40,60,80,100 ),
  y = c(33,25,27,36,23,25),
  n=c(89,surviecgvhdsurvsummary$n.risk[which.min((surviecgvhdsurvsummary$time-20)<0)],surviecgvhdsurvsummary$n.risk[which.min((surviecgvhdsurvsummary$time-40)<0)],
      surviecgvhdsurvsummary$n.risk[which.min((surviecgvhdsurvsummary$time-60)<0)],surviecgvhdsurvsummary$n.risk[which.min((surviecgvhdsurvsummary$time-80)<0)],
      surviecgvhdsurvsummary$n.risk[which.min((surviecgvhdsurvsummary$time-100)<0)]),
  ypos=c(0.05,0.05,0.05,0.05,0.05,0.05)
  
)


for (ii in 1:nrow(test))
{
  #display numbers at each visit
  km_cgvhd= km_cgvhd+ annotation_custom(grob = textGrob(test$n[ii]),
                                       xmin = test$x[ii],
                                       xmax = test$x[ii],
                                       ymin = test$ypos[ii],
                                       ymax = test$ypos[ii])
  
}


km_cgvhd=km_cgvhd+annotation_custom(grob = textGrob("N at risk"),
                                    xmin = 6.5,
                                    xmax = 6.5,
                                    ymin = 0.1,
                                    ymax = 0.1)



km_cgvhd <- ggplot_gtable(ggplot_build(km_cgvhd))
km_cgvhd$layout$clip[km_cgvhd$layout$name=="panel"] <- "off"



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



# rechute/p décès chez les progressive disease 

modelcompetebpd<-cuminc(greffe$delai_pfs[greffe$disease_status_at_transplantc=="PD"],greffe$rechute_progression_dc[greffe$disease_status_at_transplantc=="PD"],cencode=0)
plot(modelcompetebpd,curvlab=c("Relapse/Progression","Death without relapse or progression"),xlab="Time (months)", ylab="Probability")

gripd<-as.data.frame(cbind(modelcompetebpd[[1]]$time,modelcompetebpd[[1]]$est-1.96*sqrt(modelcompetebpd[[1]]$var),modelcompetebpd[[1]]$est+1.96*sqrt(modelcompetebpd[[1]]$var)))
gcipd<-as.data.frame(cbind(modelcompetebpd[[2]]$time,modelcompetebpd[[2]]$est-1.96*sqrt(modelcompetebpd[[2]]$var),modelcompetebpd[[2]]$est+1.96*sqrt(modelcompetebpd[[2]]$var)))


# rechute/p décès chez les CR1

modelcompetecr1<-cuminc(greffe$delai_pfs[greffe$disease_status_at_transplant=="CR1"],greffe$rechute_progression_dc[greffe$disease_status_at_transplant=="CR1"],cencode=0)
plot(modelcompetecr1,curvlab=c("Relapse/Progression","Death without relapse or progression"),xlab="Time (months)", ylab="Probability")

gricr1<-as.data.frame(cbind(modelcompetecr1[[1]]$time,modelcompetecr1[[1]]$est-1.96*sqrt(modelcompetecr1[[1]]$var),modelcompetecr1[[1]]$est+1.96*sqrt(modelcompetecr1[[1]]$var)))
gcicr1<-as.data.frame(cbind(modelcompetecr1[[2]]$time,modelcompetecr1[[2]]$est-1.96*sqrt(modelcompetecr1[[2]]$var),modelcompetecr1[[2]]$est+1.96*sqrt(modelcompetecr1[[2]]$var)))






### rechute/p et décès selon le conditionnement

modelefscondi<-cuminc(greffe$delai_pfs,greffe$rechute_progression_dc,cencode=0, group=greffe$intensite_condi2)

macre<-data.frame(time=modelefscondi[[1]]$time,ev=modelefscondi[[1]]$est)
ricre<-data.frame(time=modelefscondi[[2]]$time,ev=modelefscondi[[2]]$est)

macd<-data.frame(time=modelefscondi[[3]]$time,ev=modelefscondi[[3]]$est)
ricd<-data.frame(time=modelefscondi[[4]]$time,ev=modelefscondi[[4]]$est)

cif_red_condi<-ggplot()+ geom_step(data=macre,aes(x=time, y=ev,colour="MAC:relapse or progression",linetype="MAC:relapse or progression"), direction="hv")  +
  #+geom_ribbon(data=intervalletrm, aes(x=time, ymin=1-bas, ymax=1-haut),linetype="dashed",fill="grey",alpha="0.4")+
  geom_step(data=ricre,aes(x=time, y=ev,colour="RIC or NMA:relapse or progression",linetype="RIC or NMA:relapse or progression") ,direction="hv")+
  #geom_step(data=macd,aes(x=time, y=ev,colour="MAC:death without relapse or progression",linetype="MAC:death without relapse or progression"), direction="hv")+
  #geom_step(data=ricd,aes(x=time, y=ev,colour="RIC or NMA:death without relapse or progression",linetype="RIC or NMA:death without relapse or progression"), direction="hv")+
  
  
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
  scale_y_continuous(breaks=c(0,0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1),expand = c(0, 0),limits = c(0, 1))+
  #coord_cartesian(ylim=c(0,1))+
  theme_classic()+
  theme(legend.position="right",
        legend.title=element_blank())+
  
  scale_colour_manual(name="Line Color",
                      values=c("MAC:relapse or progression"="black", 
                               
                               "RIC or NMA:relapse or progression"="#0000CC"
                              , "MAC:death without relapse or progression"="black", 
                              "RIC or NMA:death without relapse or progression"="#0000CC"
                               
                               ))+
  
  
  scale_linetype_manual(name="Line Color",values=c("MAC:relapse or progression"="longdash", 
                                 
                                 "RIC or NMA:relapse or progression"="longdash"
                                 , "MAC:death without relapse or progression"="solid", 
                                 "RIC or NMA:death without relapse or progression"="solid"
                                 
  ))+
  ggtitle("")










### compétions cause de deces 

modelcompetec<-cuminc(greffe$delai_dc,greffe$cause_death_c2,cencode=0)
plot(modelcompetec,curvlab=c("Related HSCT Death","Non-related HSCT Death"),xlab="Time (months)", ylab="Probability")


modelcompeteccr<-cuminc(greffe$delai_dc,greffe$cause_death_c2,cencode=0,subset=greffe$best_response_after_allo %in%
                         c("CR"))
plot(modelcompeteccr,curvlab=c("Related HSCT Death","Non-related HSCT Death"),xlab="Time (months)", ylab="Probability")


plot(cuminc(greffe$delai_dc,greffe$cause_death_c2,cencode=0, group=greffe$intensite_condi2))





### GVHD , deces
modelcompete<-cuminc(greffe$delai_gvhd,greffe$gvhd_dc,cencode=0)
plot(modelcompete,curvlab=c("GVHD","Death without GVHD"),xlab="Time (months)", ylab="Probability")

modelcompetecr<-cuminc(greffe$delai_gvhd,greffe$gvhd_dc,cencode=0,subset=greffe$best_response_after_allo %in%
                         c("CR"))
plot(modelcompetecr,curvlab=c("GVHD","Death without GVHD"),xlab="Time (months)", ylab="Probability")



