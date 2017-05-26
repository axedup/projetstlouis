


# OS<-NULL
# for (i in c("sex_donor","sex_patient","age_greffec",
#             "delai_dia_alloc","stade_dia","stade_diac",
#             "disease_status_at_transplantc2",
#             "disease_status_at_transplantc",
#             "disease_status_at_transplant","rechute_post_allo","karnofsky_greffec2","karnofsky_greffec3" ,
#             "previous_autoc",
#             "programme_autoalloc","rechute_post_allo",
#             "nbr_lignes_avt_alloc","nbr_lignes_avt_alloc2",
#             "donnor","hla_matchc","hla_match","sex_dp3",
#             "sex_dp2","cmv_dp2","stem_cell_source","tbi","intensite_condi","condit_details",
#             "manipu_cells","nbr_donneurc","manipu_cells",
#             "agvhd","agvhd3","agvhd_grade","cgvhd",
#             "cgvhd_grade","prise_greffe","cause_death_c","best_response_after_allo",
#             "relapse_progression_transplant_c","anapathc2")
# ){
# mo<-coxph( s ~ greffe[,i],data=greffe)  
# a<-summary(coxph( s ~ greffe[,i],data=greffe))
# assign(paste(i, "m", sep="_"),a)
# capture.output(a, file=paste("C:/Users/adupont/Documents/projetstlouis/resultats/",i,"m.txt",sep=""))
# ss<-cox.zph( coxph( s ~ greffe[,i],data=greffe),transform = "log")
# capture.output(ss, file=paste("C:/Users/adupont/Documents/projetstlouis/resultats/",i,"ss.txt",sep=""))
# ssi<-cox.zph( coxph( s ~ greffe[,i],data=greffe),transform = "identity")
# ssk<-cox.zph( coxph( s ~ greffe[,i],data=greffe),transform = "km")
# capture.output(ssi, file=paste("C:/Users/adupont/Documents/projetstlouis/resultats/",i,"identity.txt",sep=""))
# 
# ssu<-survdiff( s ~ greffe[,i],data=greffe, rho=1)
# pw<-(1-pchisq(ssu$chisq, 1))
# pdf(paste("C:/Users/adupont/Documents/projetstlouis/resultats/",i,"m.pdf",sep=""), width=4, height=4,onefile = TRUE)
# #par(mfrow=c(2,2))
# plot(ss[1:nlevels(greffe[,i])-1,])
# dev.off()
# pdf(paste("C:/Users/adupont/Documents/projetstlouis/resultats/",i,"m2.pdf",sep=""), width=4, height=4,onefile = TRUE)
# 
# plot(survfit( s ~greffe[,i],data=greffe),fun="log" ,lty=1:4, col=2:5)
# text(0, 0.7, paste("Test du logrank pondéré: p=", format(round(pw, 5), scien=F)), cex=1, adj=0)
# 
# dev.off()    
# 
# pdf(paste("C:/Users/adupont/Documents/projetstlouis/resultats/",i,"m3.pdf",sep=""), width=4, height=4,onefile = TRUE)
# 
# plot(survfit( s ~greffe[,i],data=greffe),fun="cloglog" ,lty=1:4, col=2:5)
# text(0, 0.7, paste("Test du logrank pondéré: p=", format(round(pw, 5), scien=F)), cex=1, adj=0)
# 
# dev.off() 
# pdf(paste("C:/Users/adupont/Documents/projetstlouis/resultats/",i,"mi.pdf",sep=""), width=4, height=4,onefile = TRUE)
# 
# 
# plot(ssi[1:nlevels(greffe[,i])-1,])
# dev.off() 
# qq<-result.cox(mo)
# OS<-rbind(OS,qq)
# }

OS<-NULL
for (i in c("sex_donor","sex_patient","age_greffec",
            "delai_dia_alloc","stade_dia","stade_diac",
            "disease_status_at_transplantc2",
            "disease_status_at_transplantc",
            "rechute_post_allo","karnofsky_greffec2","karnofsky_greffec3" ,
            "previous_autoc",
            "programme_autoalloc","rechute_post_allo",
            "nbr_lignes_avt_alloc","nbr_lignes_avt_alloc2",
            "donnor","hla_matchc","hla_match","sex_dp3",
            "sex_dp2","cmv_dp2","stem_cell_source","tbi","intensite_condi",
            "manipu_cells","nbr_donneurc","manipu_cells",
            "agvhd","agvhd3","agvhd_grade","cgvhd",
            "anapathc2")
){
  mo<-coxph( s ~ greffe[,i],data=greffe)  
  # a<-summary(coxph( s ~ greffe[,i],data=greffe))
  # assign(paste(i, "m", sep="_"),a)
  # capture.output(a, file=paste("C:/Users/adupont/Documents/projetstlouis/resultats/",i,"m.txt",sep=""))
  # ss<-cox.zph( coxph( s ~ greffe[,i],data=greffe),transform = "log")
  # capture.output(ss, file=paste("C:/Users/adupont/Documents/projetstlouis/resultats/",i,"ss.txt",sep=""))
  # ssi<-cox.zph( coxph( s ~ greffe[,i],data=greffe),transform = "identity")
  # ssk<-cox.zph( coxph( s ~ greffe[,i],data=greffe),transform = "km")
  # capture.output(ssi, file=paste("C:/Users/adupont/Documents/projetstlouis/resultats/",i,"identity.txt",sep=""))
  # 
  # ssu<-survdiff( s ~ greffe[,i],data=greffe, rho=1)
  # pw<-(1-pchisq(ssu$chisq, 1))
  # pdf(paste("C:/Users/adupont/Documents/projetstlouis/resultats/",i,"m.pdf",sep=""), width=4, height=4,onefile = TRUE)
  # #par(mfrow=c(2,2))
  # plot(ss[1:nlevels(greffe[,i])-1,])
  # dev.off()
  # pdf(paste("C:/Users/adupont/Documents/projetstlouis/resultats/",i,"m2.pdf",sep=""), width=4, height=4,onefile = TRUE)
  # 
  # plot(survfit( s ~greffe[,i],data=greffe),fun="log" ,lty=1:4, col=2:5)
  # text(0, 0.7, paste("Test du logrank pondéré: p=", format(round(pw, 5), scien=F)), cex=1, adj=0)
  # 
  # dev.off()    
  # 
  # pdf(paste("C:/Users/adupont/Documents/projetstlouis/resultats/",i,"m3.pdf",sep=""), width=4, height=4,onefile = TRUE)
  # 
  # plot(survfit( s ~greffe[,i],data=greffe),fun="cloglog" ,lty=1:4, col=2:5)
  # text(0, 0.7, paste("Test du logrank pondéré: p=", format(round(pw, 5), scien=F)), cex=1, adj=0)
  # 
  # dev.off() 
  # pdf(paste("C:/Users/adupont/Documents/projetstlouis/resultats/",i,"mi.pdf",sep=""), width=4, height=4,onefile = TRUE)
  # 
  # 
  # plot(ssi[1:nlevels(greffe[,i])-1,])
  # dev.off() 
  qq<-result.cox(mo)
  OS<-rbind(OS,qq)
}


### EFS###

EFS<-NULL

for (i in c("sex_donor","sex_patient","age_greffec",
                        "delai_dia_alloc","stade_dia","stade_diac",
                        "disease_status_at_transplantc2",
                        "disease_status_at_transplantc",
                        "disease_status_at_transplant","rechute_post_allo","karnofsky_greffec2",
            "karnofsky_greffec3","previous_autoc",
            "programme_autoalloc","rechute_post_allo","nbr_lignes_avt_alloc",
            "nbr_lignes_avt_alloc2",
            "donnor","hla_matchc","hla_match","sex_dp3",
            "sex_dp2","cmv_dp2","stem_cell_source","tbi","intensite_condi","manipu_cells",
            "nbr_donneurc","manipu_cells"
            ,"anapathc2"
                       )
            ){
  mo<-coxph( efs ~ greffe[,i],data=greffe)  
              a<-summary(coxph( efs ~ greffe[,i],data=greffe))
              assign(paste(i, "efs", sep="_"),a)
              capture.output(a, file=paste("C:/Users/adupont/Documents/projetstlouis/resultatsefs/",i,"m.txt",sep=""))
              ss<-cox.zph( coxph( efs ~ greffe[,i],data=greffe),transform = "log")
              capture.output(ss, file=paste("C:/Users/adupont/Documents/projetstlouis/resultatsefs/",i,"ss.txt",sep=""))
              ssi<-cox.zph( coxph( efs ~ greffe[,i],data=greffe),transform = "identity")
              ssk<-cox.zph( coxph( efs ~ greffe[,i],data=greffe),transform = "km")
              capture.output(ssi, file=paste("C:/Users/adupont/Documents/projetstlouis/resultatsefs/",i,"identity.txt",sep=""))

              ssu<-survdiff( efs ~ greffe[,i],data=greffe, rho=1)
              pw<-(1-pchisq(ssu$chisq, 1))
              pdf(paste("C:/Users/adupont/Documents/projetstlouis/resultatsefs/",i,"m.pdf",sep=""), width=4, height=4,onefile = TRUE)
              #par(mfrow=c(2,2))
              plot(ss[1:nlevels(greffe[,i])-1,])
              dev.off()
              pdf(paste("C:/Users/adupont/Documents/projetstlouis/resultatsefs/",i,"m2.pdf",sep=""), width=4, height=4,onefile = TRUE)

              plot(survfit( efs ~greffe[,i],data=greffe),fun="log" ,lty=1:4, col=2:5)
              text(0, 0.7, paste("Test du logrank pondéré: p=", format(round(pw, 5), scien=F)), cex=1, adj=0)

              dev.off()

              pdf(paste("C:/Users/adupont/Documents/projetstlouis/resultatsefs/",i,"m3.pdf",sep=""), width=4, height=4,onefile = TRUE)

              plot(survfit( efs ~greffe[,i],data=greffe),fun="cloglog" ,lty=1:4, col=2:5)
              text(0, 0.7, paste("Test du logrank pondéré: p=", format(round(pw, 5), scien=F)), cex=1, adj=0)

              dev.off()
              pdf(paste("C:/Users/adupont/Documents/projetstlouis/resultatsefs/",i,"mi.pdf",sep=""), width=4, height=4,onefile = TRUE)


              plot(ssi[1:nlevels(greffe[,i])-1,])
              dev.off()
              
              qq<-result.cox(mo)
              EFS<-rbind(EFS,qq)
            }
            


###

for (i in c("sex_donor","sex_patient","age_greffec",
            "delai_dia_alloc","stade_dia","stade_diac",
            "disease_status_at_transplantc2",
            "disease_status_at_transplantc",
            "disease_status_at_transplant","rechute_post_allo","karnofsky_greffec2",
            "karnofsky_greffec3","previous_autoc",
            "programme_autoalloc","rechute_post_allo","nbr_lignes_avt_alloc",
            "nbr_lignes_avt_alloc2",
            "donnor","hla_matchc","hla_match","sex_dp3",
            "sex_dp2","cmv_dp2","stem_cell_source","tbi","intensite_condi","manipu_cells",
            "nbr_donneurc","manipu_cells"
            ,"anapathc2"
)
){
  
  
  x<-model.matrix(~greffe[,c(i)])[,-1]
  rcm<-crr(ftime=greffe$delai_pfs[!is.na(greffe[,c(i)])],
      fstatus=greffe$rechute_progression_dc[!is.na(greffe[,c(i)])],cov1=x,failcode=1)
  rc<-summary(crr(ftime=greffe$delai_pfs[!is.na(greffe[,c(i)])],
                  fstatus=greffe$rechute_progression_dc[!is.na(greffe[,c(i)])],cov1=x,failcode=1))
  capture.output(rc, file=paste("C:/Users/adupont/Documents/projetstlouis/resultatsrc/",i,"mrep.txt",sep=""))  
  
  
  plot(rcm$uftime,rcm$res)
  
  
  rcm2<-crr(ftime=greffe$delai_pfs[!is.na(greffe[,c(i)])],
      fstatus=greffe$rechute_progression_dc[!is.na(greffe[,c(i)])],cov1=x,failcode=2)
  rc2<-summary(crr(ftime=greffe$delai_pfs[!is.na(greffe[,c(i)])],
                  fstatus=greffe$rechute_progression_dc[!is.na(greffe[,c(i)])],cov1=x,failcode=2))
  capture.output(rc, file=paste("C:/Users/adupont/Documents/projetstlouis/resultatsrc/",i,"md.txt",sep=""))  
  
  plot(rcm2$uftime,rcm2$res)

}








x<-as.matrix(cbind(greffe$sex_dp3))
summary(crr(ftime=greffe$delai_pfs,fstatus=greffe$rechute_progression_dc,cov1=x,failcode=2) )
