


OS<-NULL
for (i in c("sex_donor","sex_patient","age_greffec",
            "delai_dia_alloc","stade_dia","stade_diac",
            "disease_status_at_transplantc2",
            "disease_status_at_transplantc",
            "disease_status_at_transplant","rechute_post_allo","karnofsky_greffec2","karnofsky_greffec3" ,
            "previous_autoc",
            "programme_autoalloc","rechute_prem_greffec",
            "nbr_lignes_avt_alloc","nbr_lignes_avt_alloc2",
            "donnor","hla_matchc","hla_match","sex_dp3",
            "sex_dp2","cmv_dp2","stem_cell_source","tbi","intensite_condi","condit_details",
            "manipu_cells","nbr_donneurc","manipu_cells",
            "agvhd","agvhd3","agvhd_grade","cgvhd",
            "cgvhd_grade","prise_greffe","cause_death_c","best_response_after_allo",
            "relapse_progression_transplant_c","anapathc2")
){
  
a<-summary(coxph( s_48 ~ greffe[,i],data=greffe))
assign(paste(i, "m_48", sep="_"),a)
capture.output(a, file=paste("C:/Users/adupont/Documents/projetstlouis/resultats48/",i,"m.txt",sep=""))
ss<-cox.zph( coxph( s_48 ~ greffe[,i],data=greffe),transform = "log")
capture.output(ss, file=paste("C:/Users/adupont/Documents/projetstlouis/resultats48/",i,"ss.txt",sep=""))
ssi<-cox.zph( coxph( s_48 ~ greffe[,i],data=greffe),transform = "identity")
ssk<-cox.zph( coxph( s_48 ~ greffe[,i],data=greffe),transform = "km")
capture.output(ssi, file=paste("C:/Users/adupont/Documents/projetstlouis/resultats48/",i,"identity.txt",sep=""))

ssu<-survdiff( s_48 ~ greffe[,i],data=greffe, rho=1)
pw<-(1-pchisq(ssu$chisq, 1))
pdf(paste("C:/Users/adupont/Documents/projetstlouis/resultats48/",i,"m.pdf",sep=""), width=4, height=4,onefile = TRUE)
#par(mfrow=c(2,2))
plot(ss[1:nlevels(greffe[,i])-1,])
dev.off()
pdf(paste("C:/Users/adupont/Documents/projetstlouis/resultats48/",i,"m2.pdf",sep=""), width=4, height=4,onefile = TRUE)

plot(survfit( s_48 ~greffe[,i],data=greffe),fun="log" ,lty=1:4, col=2:5)
text(0, 0.7, paste("Test du logrank pondéré: p=", format(round(pw, 5), scien=F)), cex=1, adj=0)

dev.off()    

pdf(paste("C:/Users/adupont/Documents/projetstlouis/resultats48/",i,"m3.pdf",sep=""), width=4, height=4,onefile = TRUE)

plot(survfit( s_48 ~greffe[,i],data=greffe),fun="cloglog" ,lty=1:4, col=2:5)
text(0, 0.7, paste("Test du logrank pondéré: p=", format(round(pw, 5), scien=F)), cex=1, adj=0)

dev.off() 
pdf(paste("C:/Users/adupont/Documents/projetstlouis/resultats48/",i,"mi.pdf",sep=""), width=4, height=4,onefile = TRUE)


plot(ssi[1:nlevels(greffe[,i])-1,])
dev.off() 
qq<-result.cox(mo)
OS<-rbind(OS,qq)
}