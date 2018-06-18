
### Survie 60

# OS<-NULL
# for (i in c("sex_donor","sex_patient","age_greffec",
#             "delai_dia_alloc","stade_dia","stade_diac",
#             "disease_status_at_transplantc2",
#             "disease_status_at_transplantc",
#             "rechute_post_allo","karnofsky_greffec2","karnofsky_greffec3" ,
#             "previous_autoc",
#             "programme_autoalloc","rechute_post_allo",
#             "nbr_lignes_avt_alloc","nbr_lignes_avt_alloc2",
#             "donnor","hla_matchc","hla_match","sex_dp3",
#             "sex_dp2","cmv_dp2","stem_cell_source","tbi","intensite_condi",
#             "manipu_cells","nbr_donneurc","manipu_cells",
#             "agvhd","agvhd3","agvhd_grade","cgvhd",
#             "cgvhd_grade","prise_greffe","cause_death_c","best_response_after_allo",
#             "relapse_progression_transplant_c","anapathc2")
# ){
# mo<-coxph( s_60 ~ greffe[,i],data=greffe)
# a<-summary(coxph( s_60 ~ greffe[,i],data=greffe))
# assign(paste(i, "m", sep="_"),a)
# capture.output(a, file=paste("C:/Users/adupont/Documents/projetstlouis/resultats60/",i,"m.txt",sep=""))
# ss<-cox.zph( coxph( s_60 ~ greffe[,i],data=greffe),transform = "log")
# capture.output(ss, file=paste("C:/Users/adupont/Documents/projetstlouis/resultats60/",i,"ss.txt",sep=""))
# ssi<-cox.zph( coxph( s_60 ~ greffe[,i],data=greffe),transform = "identity")
# ssk<-cox.zph( coxph( s_60 ~ greffe[,i],data=greffe),transform = "km")
# capture.output(ssi, file=paste("C:/Users/adupont/Documents/projetstlouis/resultats60/",i,"identity.txt",sep=""))
# 
# ssu<-survdiff( s_60 ~ greffe[,i],data=greffe, rho=1)
# pw<-(1-pchisq(ssu$chisq, 1))
# pdf(paste("C:/Users/adupont/Documents/projetstlouis/resultats60/",i,"m.pdf",sep=""), width=4, height=4,onefile = TRUE)
# #par(mfrow=c(2,2))
# plot(ss[1:nlevels(greffe[,i])-1,])
# dev.off()
# pdf(paste("C:/Users/adupont/Documents/projetstlouis/resultats60/",i,"m2.pdf",sep=""), width=4, height=4,onefile = TRUE)
# 
# plot(survfit( s_60 ~greffe[,i],data=greffe),fun="log" ,lty=1:4, col=2:5)
# text(0, 0.7, paste("Test du logrank pondÃ©rÃ©: p=", format(round(pw, 5), scien=F)), cex=1, adj=0)
# 
# dev.off()
# 
# pdf(paste("C:/Users/adupont/Documents/projetstlouis/resultats60/",i,"m3.pdf",sep=""), width=4, height=4,onefile = TRUE)
# 
# plot(survfit( s_60 ~greffe[,i],data=greffe),fun="cloglog" ,lty=1:4, col=2:5)
# text(0, 0.7, paste("Test du logrank pondÃ©rÃ©: p=", format(round(pw, 5), scien=F)), cex=1, adj=0)
# 
# dev.off()
# pdf(paste("C:/Users/adupont/Documents/projetstlouis/resultats60/",i,"mi.pdf",sep=""), width=4, height=4,onefile = TRUE)
# 
# 
# plot(ssi[1:nlevels(greffe[,i])-1,])
# dev.off()
# # qq<-result.cox(mo)
# # OS<-rbind(OS,qq)
#  }
# 
# 
# summary(coxph( s_60 ~ age_greffe,data=greffe))
# plot(cox.zph( coxph( s_60 ~ age_greffe,data=greffe),transform = "identity"))
# 
# summary(coxph( s_60 ~ depletion ,data=greffe))
# plot(cox.zph( coxph( s_60 ~depletion,data=greffe),transform = "identity")[1])
# 
# summary(coxph( s_60 ~ anapathc2 ,data=greffe))
# plot(cox.zph( coxph( s_60 ~anapathc2,data=greffe),transform = "identity")[1 :4])
# 
# 
# summary(coxph( s_60 ~ hla_matchc2,data=greffe))
# plot(cox.zph( coxph( s_60 ~hla_matchc2,data=greffe),transform = "identity")[1 :2])
# 
# summary(coxph( s_60 ~ agvhd3,data=greffe))
# plot(cox.zph( coxph( s_60 ~agvhd3,data=greffe),transform = "identity"))
# 
# summary(coxph( s_60 ~ cgvhd,data=greffe))
# plot(cox.zph( coxph( s_60 ~cgvhd,data=greffe),transform = "identity"))
# 

OS<-NULL

for (i in c( "age_greffe","anapathc2",
             "delai_dia_alloc","stade_dia",
             
             "disease_status_at_transplantc",
             "karnofsky_greffec3" ,
             "rechute_post_allo",
             "nbr_lignes_avt_alloc",
             "donnor","sex_dp3",
             "cmv_dp2","stem_cell_source","intensite_condi","depletion")
){
  mo<-coxph( s_60 ~ greffe[,i],data=greffe)  
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
  # text(0, 0.7, paste("Test du logrank pondÃ©rÃ©: p=", format(round(pw, 5), scien=F)), cex=1, adj=0)
  # 
  # dev.off()    
  # 
  # pdf(paste("C:/Users/adupont/Documents/projetstlouis/resultats/",i,"m3.pdf",sep=""), width=4, height=4,onefile = TRUE)
  # 
  # plot(survfit( s ~greffe[,i],data=greffe),fun="cloglog" ,lty=1:4, col=2:5)
  # text(0, 0.7, paste("Test du logrank pondÃ©rÃ©: p=", format(round(pw, 5), scien=F)), cex=1, adj=0)
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


result.coxt <- function (modele)
{
  summary(modele)$conf.int ->  modele.detail
  res <- data.frame (Variable = names(modele$coef))
  res$HR <- round(modele.detail[,1],2)
  res$IC <- paste("[" , round(modele.detail[,3],2) , " - " , round(modele.detail[,4],2) ,"]",sep="")
  for (j in 1:length(res$IC))
  {
    res$pval[j] <- as.character(format.pv(summary(modele)$coef[j,5]))
  }
  res$Variable<- gsub("greffe_long2\\[, i\\]","",res$Variable)
  titre<-data.frame(Variable=ifelse(is.factor(greffe_long2[,i]),levels(greffe_long2[,i][1]),""),HR=ifelse(is.factor(greffe_long2[,i]),1.00,NA),IC=NA,pval=NA)
  res<-rbind(titre,res)
  res$p<-c(as.character(format.pv(summary(modele)$logtest[3])),rep("",nrow(res)-1))  
  return(res)
}


OST<-NULL

for (i in c( "agvhd","cgvhd")
){
  mo<-coxph( Surv(time=as.numeric(start), time2=as.numeric(stop), event=decesf)~ greffe_long2[,i],data=greffe_long2)  
  
  qq<-result.coxt(mo)
  OST<-rbind(OST,qq)
}


OS<-rbind(OS,OST)


OS$variable <- c("Age at graft","","subtypes","","","","","",
                 "Delay between diag and allo SCT > 12 mo","","Stage at diagnosis","","","",
                 
                 "Disease status at transplant","","",
                 "Karnofsky score","","",
                 "First graft relapse","","",
                 "No of lines before alloSCT","",
                 "HLA match","","Sex of donnor-patient","",
                 "CMV serostatus of donnor patient","","Source of stem cells","","","Conditionning intensity","","","Depletion","",
                 "Agvhd grade 3-4","","Cgvhd","")


OS<-OS[c("variable","Variable","HR","IC","pval","p")]

###

# finalos<-greffe[complete.cases(greffe[,c("anapathc2",
#                                          "disease_status_at_transplantc",
#                                          "karnofsky_greffec3" ,
#                                          "rechute_post_allo",
#                                          "nbr_lignes_avt_alloc",
#                                          "donnor","sex_dp3",
#                                          "cmv_dp2","stem_cell_source","intensite_condi")]),c("delai_dc_60","deces_60","anapathc2",
#                                                                                              "disease_status_at_transplantc",
#                                                                                              "karnofsky_greffec3" ,
#                                                                                              "rechute_post_allo",
#                                                                                              "nbr_lignes_avt_alloc",
#                                                                                              "donnor","sex_dp3",
#                                                                                              "cmv_dp2","stem_cell_source","intensite_condi")]
# 
# 
# 
# os_60f<-Surv(event=finalos$deces_60,time=as.numeric(finalos$delai_dc_60))
# 
# mos<-coxph( os_60f ~ .,data=finalos[,c("anapathc2", "disease_status_at_transplantc",
#                                        "karnofsky_greffec3" ,
#                                        "rechute_post_allo",
#                                        "nbr_lignes_avt_alloc",
#                                        "donnor", "stem_cell_source")])  
# 
# 
# 
# 
# stepAIC(mos,direction="both")
# 
# 
# 
# final_os_60<-coxph( s_60 ~ .,data=greffe[,c( "disease_status_at_transplantc",
#                                              "karnofsky_greffec3" ,
#                                              "rechute_post_allo",
#                                              "stem_cell_source")])
# 
# 
# 
# ss<-cox.zph( final_os_60,transform = "log")
# plot(ss)
# 

### EFS###

# EFS<-NULL
# 
# for (i in c("sex_donor","sex_patient","age_greffec",
#                         "delai_dia_alloc","stade_dia","stade_diac",
#                         "disease_status_at_transplantc2",
#                         "disease_status_at_transplantc",
#                         "disease_status_at_transplant","rechute_post_allo","karnofsky_greffec2",
#             "karnofsky_greffec3","previous_autoc",
#             "programme_autoalloc","rechute_post_allo","nbr_lignes_avt_alloc",
#             "nbr_lignes_avt_alloc2",
#             "donnor","hla_matchc","hla_match","sex_dp3",
#             "sex_dp2","cmv_dp2","stem_cell_source","tbi","intensite_condi","manipu_cells",
#             "nbr_donneurc","manipu_cells"
#             ,"anapathc2"
#                        )
#             ){
#   mo<-coxph( efs ~ greffe[,i],data=greffe)  
#               a<-summary(coxph( efs ~ greffe[,i],data=greffe))
#               assign(paste(i, "efs", sep="_"),a)
#               capture.output(a, file=paste("C:/Users/adupont/Documents/projetstlouis/resultatsefs/",i,"m.txt",sep=""))
#               ss<-cox.zph( coxph( efs ~ greffe[,i],data=greffe),transform = "log")
#               capture.output(ss, file=paste("C:/Users/adupont/Documents/projetstlouis/resultatsefs/",i,"ss.txt",sep=""))
#               ssi<-cox.zph( coxph( efs ~ greffe[,i],data=greffe),transform = "identity")
#               ssk<-cox.zph( coxph( efs ~ greffe[,i],data=greffe),transform = "km")
#               capture.output(ssi, file=paste("C:/Users/adupont/Documents/projetstlouis/resultatsefs/",i,"identity.txt",sep=""))
# 
#               ssu<-survdiff( efs ~ greffe[,i],data=greffe, rho=1)
#               pw<-(1-pchisq(ssu$chisq, 1))
#               pdf(paste("C:/Users/adupont/Documents/projetstlouis/resultatsefs/",i,"m.pdf",sep=""), width=4, height=4,onefile = TRUE)
#               #par(mfrow=c(2,2))
#               plot(ss[1:nlevels(greffe[,i])-1,])
#               dev.off()
#               pdf(paste("C:/Users/adupont/Documents/projetstlouis/resultatsefs/",i,"m2.pdf",sep=""), width=4, height=4,onefile = TRUE)
# 
#               plot(survfit( efs ~greffe[,i],data=greffe),fun="log" ,lty=1:4, col=2:5)
#               text(0, 0.7, paste("Test du logrank pondÃ©rÃ©: p=", format(round(pw, 5), scien=F)), cex=1, adj=0)
# 
#               dev.off()
# 
#               pdf(paste("C:/Users/adupont/Documents/projetstlouis/resultatsefs/",i,"m3.pdf",sep=""), width=4, height=4,onefile = TRUE)
# 
#               plot(survfit( efs ~greffe[,i],data=greffe),fun="cloglog" ,lty=1:4, col=2:5)
#               text(0, 0.7, paste("Test du logrank pondÃ©rÃ©: p=", format(round(pw, 5), scien=F)), cex=1, adj=0)
# 
#               dev.off()
#               pdf(paste("C:/Users/adupont/Documents/projetstlouis/resultatsefs/",i,"mi.pdf",sep=""), width=4, height=4,onefile = TRUE)
# 
# 
#               plot(ssi[1:nlevels(greffe[,i])-1,])
#               dev.off()
#               
#               qq<-result.cox(mo)
#               EFS<-rbind(EFS,qq)
#             }
#             
# 
# 
# ###
# 
# for (i in c("sex_donor","sex_patient","age_greffec",
#             "delai_dia_alloc","stade_dia","stade_diac",
#             "disease_status_at_transplantc2",
#             "disease_status_at_transplantc",
#             "disease_status_at_transplant","rechute_post_allo","karnofsky_greffec2",
#             "karnofsky_greffec3","previous_autoc",
#             "programme_autoalloc","rechute_post_allo","nbr_lignes_avt_alloc",
#             "nbr_lignes_avt_alloc2",
#             "donnor","hla_matchc","hla_match","sex_dp3",
#             "sex_dp2","cmv_dp2","stem_cell_source","tbi","intensite_condi","manipu_cells",
#             "nbr_donneurc","manipu_cells"
#             ,"anapathc2"
# )
# ){
#   
#   
#   x<-model.matrix(~greffe[,c(i)])
#   x<-x[,2:ncol(x)]
#   rcm<-crr(ftime=greffe$delai_pfs[!is.na(greffe[,c(i)])],
#       fstatus=greffe$rechute_progression_dc[!is.na(greffe[,c(i)])],cov1=x,failcode=1)
#   rc<-summary(crr(ftime=greffe$delai_pfs[!is.na(greffe[,c(i)])],
#                   fstatus=greffe$rechute_progression_dc[!is.na(greffe[,c(i)])],cov1=x,failcode=1))
#   capture.output(rc, file=paste("C:/Users/adupont/Documents/projetstlouis/resultatsrc/",i,"mrep.txt",sep=""))  
#  s<-as.data.frame(x)
#   for (j in 1:ncol(s)){
#     pdf(paste("C:/Users/adupont/Documents/projetstlouis/resultatsrc/",i,j,"mrep.pdf",sep=""), width=4, height=4,onefile = TRUE)
#     
#   scatter.smooth(rcm$uftime,rcm$res[,j])
#   dev.off()}
#   
#   rcm2<-crr(ftime=greffe$delai_pfs[!is.na(greffe[,c(i)])],
#       fstatus=greffe$rechute_progression_dc[!is.na(greffe[,c(i)])],cov1=x,failcode=2)
#   rc2<-summary(crr(ftime=greffe$delai_pfs[!is.na(greffe[,c(i)])],
#                   fstatus=greffe$rechute_progression_dc[!is.na(greffe[,c(i)])],cov1=x,failcode=2))
#   capture.output(rc, file=paste("C:/Users/adupont/Documents/projetstlouis/resultatsrc/",i,"md.txt",sep=""))  
#   
#   for (j in 1:ncol(s)){
#     pdf(paste("C:/Users/adupont/Documents/projetstlouis/resultatsrc/",i,j,"md.pdf",sep=""), width=4, height=4,onefile = TRUE)
#     
#     scatter.smooth(rcm2$uftime,rcm2$res[,j])
#     dev.off()}
# }
# 

### PFS###

# PFS<-NULL
# 
# for (i in c("sex_donor","sex_patient","age_greffec",
#             "delai_dia_alloc","stade_dia","stade_diac",
#             "disease_status_at_transplantc2",
#             "disease_status_at_transplantc",
#             "disease_status_at_transplant","rechute_post_allo","karnofsky_greffec3",
#             "karnofsky_greffec3","previous_autoc",
#             "programme_autoalloc","rechute_post_allo","nbr_lignes_avt_alloc",
#             "nbr_lignes_avt_alloc",
#             "donnor","hla_matchc2","hla_match","sex_dp3",
#             "sex_dp2","cmv_dp2","stem_cell_source","tbi","intensite_condi","manipu_cells",
#             "nbr_donneurc","manipu_cells"
#             ,"anapathc2"
# )
# ){
#   mo<-coxph( pfss_60 ~ greffe[,i],data=greffe)  
#   a<-summary(coxph( pfss_60 ~ greffe[,i],data=greffe))
#   assign(paste(i, "pfs_60", sep="_"),a)
#   capture.output(a, file=paste("C:/Users/adupont/Documents/projetstlouis/resultatspfs60/",i,"m.txt",sep=""))
#   ss<-cox.zph( coxph( pfss_60 ~ greffe[,i],data=greffe),transform = "log")
#   capture.output(ss, file=paste("C:/Users/adupont/Documents/projetstlouis/resultatspfs60/",i,"ss.txt",sep=""))
#   ssi<-cox.zph( coxph( pfss_60 ~ greffe[,i],data=greffe),transform = "identity")
#   ssk<-cox.zph( coxph( pfss_60 ~ greffe[,i],data=greffe),transform = "km")
#   capture.output(ssi, file=paste("C:/Users/adupont/Documents/projetstlouis/resultatspfs60/",i,"identity.txt",sep=""))
#   
#   ssu<-survdiff( pfss_60 ~ greffe[,i],data=greffe, rho=1)
#   pw<-(1-pchisq(ssu$chisq, 1))
#   pdf(paste("C:/Users/adupont/Documents/projetstlouis/resultatspfs60/",i,"m.pdf",sep=""), width=4, height=4,onefile = TRUE)
#   #par(mfrow=c(2,2))
#   plot(ss[1:nlevels(greffe[,i])-1,])
#   dev.off()
#   pdf(paste("C:/Users/adupont/Documents/projetstlouis/resultatspfs60/",i,"m2.pdf",sep=""), width=4, height=4,onefile = TRUE)
#   
#   plot(survfit( pfss_60 ~greffe[,i],data=greffe),fun="log" ,lty=1:4, col=2:5)
#   text(0, 0.7, paste("Test du logrank pondÃ©rÃ©: p=", format(round(pw, 5), scien=F)), cex=1, adj=0)
#   
#   dev.off()
#   
#   pdf(paste("C:/Users/adupont/Documents/projetstlouis/resultatspfs60/",i,"m3.pdf",sep=""), width=4, height=4,onefile = TRUE)
#   
#   plot(survfit( pfss_60 ~greffe[,i],data=greffe),fun="cloglog" ,lty=1:4, col=2:5)
#   text(0, 0.7, paste("Test du logrank pondÃ©rÃ©: p=", format(round(pw, 5), scien=F)), cex=1, adj=0)
#   
#   dev.off()
#   pdf(paste("C:/Users/adupont/Documents/projetstlouis/resultatspfs60/",i,"mi.pdf",sep=""), width=4, height=4,onefile = TRUE)
#   
#   
#   plot(ssi[1:nlevels(greffe[,i])-1,])
#   dev.off()
#   
#   qq<-result.cox(mo)
#   PFS<-rbind(PFS,qq)
# }
# 



### PFS 60 jours ###

# PFS<-NULL
# 
# for (i in c("sex_donor","sex_patient","age_greffec",
#             "delai_dia_alloc","stade_dia","stade_diac",
#             "disease_status_at_transplantc2",
#             "disease_status_at_transplantc",
#             "disease_status_at_transplant","rechute_post_allo","karnofsky_greffec3",
#             "karnofsky_greffec3","previous_autoc",
#             "programme_autoalloc","rechute_post_allo","nbr_lignes_avt_alloc",
#             "nbr_lignes_avt_alloc",
#             "donnor","hla_matchc2","hla_match","sex_dp3",
#             "sex_dp2","cmv_dp2","stem_cell_source","tbi","intensite_condi","manipu_cells",
#             "nbr_donneurc","manipu_cells"
#             ,"anapathc2"
# )
# ){
#   mo<-coxph( pfss_60 ~ greffe[,i],data=greffe)  
#   a<-summary(coxph( pfss_60 ~ greffe[,i],data=greffe))
#   assign(paste(i, "pfs_60", sep="_"),a)
#   capture.output(a, file=paste("C:/Users/adupont/Documents/projetstlouis/resultatspfs60/",i,"m.txt",sep=""))
#   ss<-cox.zph( coxph( pfss_60 ~ greffe[,i],data=greffe),transform = "log")
#   capture.output(ss, file=paste("C:/Users/adupont/Documents/projetstlouis/resultatspfs60/",i,"ss.txt",sep=""))
#   ssi<-cox.zph( coxph( pfss_60 ~ greffe[,i],data=greffe),transform = "identity")
#   ssk<-cox.zph( coxph( pfss_60 ~ greffe[,i],data=greffe),transform = "km")
#   capture.output(ssi, file=paste("C:/Users/adupont/Documents/projetstlouis/resultatspfs60/",i,"identity.txt",sep=""))
#   
#   ssu<-survdiff( pfss_60 ~ greffe[,i],data=greffe, rho=1)
#   pw<-(1-pchisq(ssu$chisq, 1))
#   pdf(paste("C:/Users/adupont/Documents/projetstlouis/resultatspfs60/",i,"m.pdf",sep=""), width=4, height=4,onefile = TRUE)
#   #par(mfrow=c(2,2))
#   plot(ss[1:nlevels(greffe[,i])-1,])
#   dev.off()
#   pdf(paste("C:/Users/adupont/Documents/projetstlouis/resultatspfs60/",i,"m2.pdf",sep=""), width=4, height=4,onefile = TRUE)
#   
#   plot(survfit( pfss_60 ~greffe[,i],data=greffe),fun="log" ,lty=1:4, col=2:5)
#   text(0, 0.7, paste("Test du logrank pondÃ©rÃ©: p=", format(round(pw, 5), scien=F)), cex=1, adj=0)
#   
#   dev.off()
#   
#   pdf(paste("C:/Users/adupont/Documents/projetstlouis/resultatspfs60/",i,"m3.pdf",sep=""), width=4, height=4,onefile = TRUE)
#   
#   plot(survfit( pfss_60 ~greffe[,i],data=greffe),fun="cloglog" ,lty=1:4, col=2:5)
#   text(0, 0.7, paste("Test du logrank pondÃ©rÃ©: p=", format(round(pw, 5), scien=F)), cex=1, adj=0)
#   
#   dev.off()
#   pdf(paste("C:/Users/adupont/Documents/projetstlouis/resultatspfs60/",i,"mi.pdf",sep=""), width=4, height=4,onefile = TRUE)
#   
#   
#   plot(ssi[1:nlevels(greffe[,i])-1,])
#   dev.off()
#   
#   qq<-result.cox(mo)
#   PFS<-rbind(PFS,qq)
# }
# 
# 
# summary(coxph( pfss_60 ~ age_greffe,data=greffe))
# plot(cox.zph( coxph(pfss_60 ~ age_greffe,data=greffe),transform = "identity"))
# 
# summary(coxph( pfss_60 ~ . ,data=greffe))
# plot(cox.zph( coxph( pfss_60 ~depletion,data=greffe),transform = "identity")[1])
# 
# # summary(coxph( pfss_60 ~ agvhd3,data=greffe))
# # plot(cox.zph( coxph(pfss_60 ~ agvhd3,data=greffe),transform = "identity"))
# # 
# # summary(coxph( pfss_60 ~ cgvhd,data=greffe))
# # plot(cox.zph( coxph(pfss_60 ~ cgvhd,data=greffe),transform = "identity"))


PFS<-NULL

for (i in c( "age_greffe","anapathc2",
             "delai_dia_alloc","stade_dia",
             
             "disease_status_at_transplantc",
             "karnofsky_greffec3" ,
             "rechute_post_allo",
             "nbr_lignes_avt_alloc",
             "donnor","sex_dp3",
             "cmv_dp2","stem_cell_source","intensite_condi","depletion"
             
)
){
  mo<-coxph( pfss_60 ~ greffe[,i],data=greffe)  
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
  # text(0, 0.7, paste("Test du logrank pondÃ©rÃ©: p=", format(round(pw, 5), scien=F)), cex=1, adj=0)
  # 
  # dev.off()    
  # 
  # pdf(paste("C:/Users/adupont/Documents/projetstlouis/resultats/",i,"m3.pdf",sep=""), width=4, height=4,onefile = TRUE)
  # 
  # plot(survfit( s ~greffe[,i],data=greffe),fun="cloglog" ,lty=1:4, col=2:5)
  # text(0, 0.7, paste("Test du logrank pondÃ©rÃ©: p=", format(round(pw, 5), scien=F)), cex=1, adj=0)
  # 
  # dev.off() 
  # pdf(paste("C:/Users/adupont/Documents/projetstlouis/resultats/",i,"mi.pdf",sep=""), width=4, height=4,onefile = TRUE)
  # 
  # 
  # plot(ssi[1:nlevels(greffe[,i])-1,])
  # dev.off() 
  qq<-result.cox(mo)
  PFS<-rbind(PFS,qq)
}


result.coxtt <- function (modele)
{
  summary(modele)$conf.int ->  modele.detail
  res <- data.frame (Variable = names(modele$coef))
  res$HR <- round(modele.detail[,1],2)
  res$IC <- paste("[" , round(modele.detail[,3],2) , " - " , round(modele.detail[,4],2) ,"]",sep="")
  for (j in 1:length(res$IC))
  {
    res$pval[j] <- as.character(format.pv(summary(modele)$coef[j,5]))
  }
  res$Variable<- gsub("greffe_longpfs2\\[, i\\]","",res$Variable)
  titre<-data.frame(Variable=ifelse(is.factor(greffe_long2[,i]),levels(greffe_long2[,i][1]),""),HR=ifelse(is.factor(greffe_long2[,i]),1.00,NA),IC=NA,pval=NA)
  res<-rbind(titre,res)
  res$p<-c(as.character(format.pv(summary(modele)$logtest[3])),rep("",nrow(res)-1))  
  return(res)
}







PFSST<-NULL

for (i in c( "agvhd","cgvhd")
){
  mo<-coxph( Surv(time=as.numeric(start), time2=as.numeric(stop), event=pfs60f)~ greffe_longpfs2[,i],data=greffe_longpfs2)  
  
  qq<-result.coxtt(mo)
  PFSST<-rbind(PFSST,qq)
}


PFS<-rbind(PFS,PFSST)







PFS$variable <- c("Age at graft","","subtypes","","","","","",
                  "Delay between diag and allo SCT > 12 mo","","Stage at diagnosis","","","",
                  
                  "Disease status at transplant","","",
                  "Karnofsky score","","",
                  "First graft relapse","","",
                  "No of lines before alloSCT","",
                  "HLA match","","Sex of donnor-patient","",
                  "CMV serostatus of donnor patient","","Source of stem cells","","","Conditionning intensity","","","Depletion","",
                  "Agvhd grade 3-4","","Cgvhd","")


PFS<-PFS[c("variable","Variable","HR","IC","pval","p")]
### 




### EFS 60 jours ###

# EFS_60<-NULL
# 
# for (i in c("sex_donor","sex_patient","age_greffec",
#             "delai_dia_alloc","stade_dia","stade_diac",
#             "disease_status_at_transplantc2",
#             "disease_status_at_transplantc",
#             "disease_status_at_transplant","rechute_post_allo","karnofsky_greffec3",
#             "karnofsky_greffec3","previous_autoc",
#             "programme_autoalloc","rechute_post_allo","nbr_lignes_avt_alloc",
#             "nbr_lignes_avt_alloc",
#             "donnor","hla_matchc2","hla_match","sex_dp3",
#             "sex_dp2","cmv_dp2","stem_cell_source","tbi","intensite_condi","manipu_cells",
#             "nbr_donneurc","manipu_cells"
#             ,"anapathc2"
# )
# ){
#   mo<-coxph( efs_60 ~ greffe[,i],data=greffe)  
#   a<-summary(coxph( efs_60 ~ greffe[,i],data=greffe))
#   assign(paste(i, "efs_60", sep="_"),a)
#   capture.output(a, file=paste("C:/Users/adupont/Documents/projetstlouis/resultatsefs60/",i,"m.txt",sep=""))
#   ss<-cox.zph( coxph( efs_60 ~ greffe[,i],data=greffe),transform = "log")
#   capture.output(ss, file=paste("C:/Users/adupont/Documents/projetstlouis/resultatsefs60/",i,"ss.txt",sep=""))
#   ssi<-cox.zph( coxph( efs_60 ~ greffe[,i],data=greffe),transform = "identity")
#   ssk<-cox.zph( coxph( efs_60 ~ greffe[,i],data=greffe),transform = "km")
#   capture.output(ssi, file=paste("C:/Users/adupont/Documents/projetstlouis/resultatsefs60/",i,"identity.txt",sep=""))
#   
#   ssu<-survdiff( efs_60 ~ greffe[,i],data=greffe, rho=1)
#   pw<-(1-pchisq(ssu$chisq, 1))
#   pdf(paste("C:/Users/adupont/Documents/projetstlouis/resultatsefs60/",i,"m.pdf",sep=""), width=4, height=4,onefile = TRUE)
#   #par(mfrow=c(2,2))
#   plot(ss[1:nlevels(greffe[,i])-1,])
#   dev.off()
#   pdf(paste("C:/Users/adupont/Documents/projetstlouis/resultatsefs60/",i,"m2.pdf",sep=""), width=4, height=4,onefile = TRUE)
#   
#   plot(survfit( efs_60 ~greffe[,i],data=greffe),fun="log" ,lty=1:4, col=2:5)
#   text(0, 0.7, paste("Test du logrank pondÃ©rÃ©: p=", format(round(pw, 5), scien=F)), cex=1, adj=0)
#   
#   dev.off()
#   
#   pdf(paste("C:/Users/adupont/Documents/projetstlouis/resultatsefs60/",i,"m3.pdf",sep=""), width=4, height=4,onefile = TRUE)
#   
#   plot(survfit( efs_60 ~greffe[,i],data=greffe),fun="cloglog" ,lty=1:4, col=2:5)
#   text(0, 0.7, paste("Test du logrank pondÃ©rÃ©: p=", format(round(pw, 5), scien=F)), cex=1, adj=0)
#   
#   dev.off()
#   pdf(paste("C:/Users/adupont/Documents/projetstlouis/resultatsefs60/",i,"mi.pdf",sep=""), width=4, height=4,onefile = TRUE)
#   
#   
#   plot(ssi[1:nlevels(greffe[,i])-1,])
#   dev.off()
#   
#   qq<-result.cox(mo)
#   EFS_60<-rbind(EFS_60,qq)
# }
# 
# summary(coxph( efs_60 ~ age_greffe,data=greffe))
# plot(cox.zph( coxph( efs_60 ~ age_greffe,data=greffe),transform = "identity"))
# 
# summary(coxph( efs_60 ~ depletion ,data=greffe))
# plot(cox.zph( coxph( efs_60 ~depletion,data=greffe),transform = "identity")[1])

EFS<-NULL

for (i in c( "age_greffe","anapathc2",
             "delai_dia_alloc","stade_dia",
             
             "disease_status_at_transplantc",
             "karnofsky_greffec3" ,
             "rechute_post_allo",
             "nbr_lignes_avt_alloc",
             "donnor","sex_dp3",
             "cmv_dp2","stem_cell_source","intensite_condi","depletion"
             
)
){
  mo<-coxph( efs_60 ~ greffe[,i],data=greffe)  
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
  # text(0, 0.7, paste("Test du logrank pondÃ©rÃ©: p=", format(round(pw, 5), scien=F)), cex=1, adj=0)
  # 
  # dev.off()    
  # 
  # pdf(paste("C:/Users/adupont/Documents/projetstlouis/resultats/",i,"m3.pdf",sep=""), width=4, height=4,onefile = TRUE)
  # 
  # plot(survfit( s ~greffe[,i],data=greffe),fun="cloglog" ,lty=1:4, col=2:5)
  # text(0, 0.7, paste("Test du logrank pondÃ©rÃ©: p=", format(round(pw, 5), scien=F)), cex=1, adj=0)
  # 
  # dev.off() 
  # pdf(paste("C:/Users/adupont/Documents/projetstlouis/resultats/",i,"mi.pdf",sep=""), width=4, height=4,onefile = TRUE)
  # 
  # 
  # plot(ssi[1:nlevels(greffe[,i])-1,])
  # dev.off() 
  qq<-result.cox(mo)
  EFS<-rbind(EFS,qq)
}


EFS$variable <- c("Age at graft","","subtypes","","","","","",
                  "Delay between diag and allo SCT","","Stage at diagnosis","","","",
                  
                  "Disease status at transplant","","",
                  "Karnofsky score","","",
                  "First graft relapse","","",
                  "No of lines before alloSCT","",
                  "HLA match","","Sex of donnor-patient","",
                  "CMV serostatus of donnor patient","","Source of stem cells","","","Conditionning intensity","","","Depletion","")



EFS<-EFS[c("variable","Variable","HR","IC","pval","p")]



final_efs_602<-summary(coxph( efs_60 ~ .,data=greffe[,c(
  "anapathc2" ,
  "disease_status_at_transplantc",
  "stem_cell_source")]))

ssefs2<-cox.zph( coxph( efs_60 ~ .,data=greffe[,c(
  "anapathc2" ,
  "disease_status_at_transplantc",
  "stem_cell_source")]),transform = "log")
plot(ssefs2)

l<-lapply(list("anapathc2","disease_status_at_transplantc","stem_cell_source"),repli,data=greffe)

l<-unlist(l)

efsaic<-cox_multi(final_efs_602,l)

efsaic<-cbind(c("Subtypes","","","","","Disease status at transplant","","Source of stem cells",""),efsaic)





# 
# ###
# finalefs<-greffe[complete.cases(greffe[,c("anapathc2",
#                                           
#                                           "disease_status_at_transplantc",
#                                           "karnofsky_greffec3" ,
#                                           
#                                           "nbr_lignes_avt_alloc",
#                                           "donnor","sex_dp3",
#                                           "stem_cell_source","depletion")]),c("delai_rechutepg_60","evenefs_60",
#                                                                               "anapathc2",
#                                                                               
#                                                                               "disease_status_at_transplantc",
#                                                                               "karnofsky_greffec3" ,
#                                                                               
#                                                                               "nbr_lignes_avt_alloc",
#                                                                               "donnor","sex_dp3",
#                                                                               "stem_cell_source","depletion")]
# 
# 
# 
# efss_60f<-Surv(event=finalefs$evenefs_60,time=as.numeric(finalefs$delai_rechutepg_60))
# 
# moe<-coxph( efss_60f ~ .,data=finalefs[,c( "anapathc2",
#                                            
#                                            "disease_status_at_transplantc",
#                                            "karnofsky_greffec3" ,
#                                            
#                                            "nbr_lignes_avt_alloc",
#                                            "donnor","sex_dp3",
#                                            "stem_cell_source")])  
# 
# 
# 
# 
# stepAIC(moe,direction="both")
# 
# 
# (xtabs( ~ evenefs_60+ depletion+donnor+anapathc2+disease_status_at_transplantc+karnofsky_greffec3, data=finalefs))
# 
# 
# 
# final_efs_60<-coxph( efs_60 ~ .,data=greffe[,c(
#   "sex_dp3" ,
#   "disease_status_at_transplantc",
#   "stem_cell_source")])
# 
# 
# 
# ssefs<-cox.zph( final_efs_60,transform = "log")
# plot(ssefs)


### cAUSE sp�cifique mortalit�###

csp_60<-Surv(event=greffe$cause_death_c360,time=as.numeric(greffe$delai_dc_60))
CSP<-NULL

for (i in c( "age_greffe","anapathc2",
             "delai_dia_alloc","stade_dia",
             
             "disease_status_at_transplantc",
             "karnofsky_greffec3" ,
             "rechute_post_allo",
             "nbr_lignes_avt_alloc",
             "donnor","sex_dp3",
             "cmv_dp2","stem_cell_source","intensite_condi","depletion")
){
  mo<-coxph( csp_60 ~ greffe[,i],data=greffe)  
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
  # text(0, 0.7, paste("Test du logrank pondÃ©rÃ©: p=", format(round(pw, 5), scien=F)), cex=1, adj=0)
  # 
  # dev.off()    
  # 
  # pdf(paste("C:/Users/adupont/Documents/projetstlouis/resultats/",i,"m3.pdf",sep=""), width=4, height=4,onefile = TRUE)
  # 
  # plot(survfit( s ~greffe[,i],data=greffe),fun="cloglog" ,lty=1:4, col=2:5)
  # text(0, 0.7, paste("Test du logrank pondÃ©rÃ©: p=", format(round(pw, 5), scien=F)), cex=1, adj=0)
  # 
  # dev.off() 
  # pdf(paste("C:/Users/adupont/Documents/projetstlouis/resultats/",i,"mi.pdf",sep=""), width=4, height=4,onefile = TRUE)
  # 
  # 
  # plot(ssi[1:nlevels(greffe[,i])-1,])
  # dev.off() 
  qq<-result.cox(mo)
  CSP<-rbind(CSP,qq)
}




CSP$variable <- c("Age at graft","","subtypes","","","","","",
                  "Delay between diag and allo SCT > 12 mo","","Stage at diagnosis","","","",
                  
                  "Disease status at transplant","","",
                  "Karnofsky score","","",
                  "First graft relapse","","",
                  "No of lines before alloSCT","",
                  "HLA match","","Sex of donnor-patient","",
                  "CMV serostatus of donnor patient","","Source of stem cells","","","Conditionning intensity","","","Depletion",""
                 )


CSP<-  CSP[c("variable","Variable","HR","IC","pval","p")]


final_csp_602<-coxph( csp_60 ~ .,data=greffe[,c(
  "anapathc2" ,
  "intensite_condi2","sex_dp3","karnofsky_greffec3")])
ssefs2<-cox.zph( final_csp_602,transform = "log")
plot(ssefs2)


finalcsp<-greffe[complete.cases(greffe[,c(  "anapathc2" ,
                                            "sex_dp3","karnofsky_greffec3","age_greffe","nbr_lignes_avt_alloc","stem_cell_source","intensite_condi")]),]



csp_60f<-Surv(event=finalcsp$cause_death_c360,time=as.numeric(finalcsp$delai_dc_60))

moe<-coxph( csp_60f ~ .,data=finalcsp[,c(  "anapathc2" ,
                                           
                                           "sex_dp3","karnofsky_greffec3","age_greffe","nbr_lignes_avt_alloc","stem_cell_source","intensite_condi")])  




stepAIC(moe,direction="both")

final_csp_603<-coxph( csp_60 ~ sex_dp3 + karnofsky_greffec3 + age_greffe + strata(nbr_lignes_avt_alloc),data=greffe)

ssefs3<-cox.zph( final_csp_603,transform = "log")
plot(ssefs3)



l<-lapply(list("sex_dp3","karnofsky_greffec3"),repli,data=greffe)

l<-unlist(l)
l<-c(l,"age_greffe")


cspaic<-cox_multi(summary(final_csp_603),l)
cspaic<-cbind(c("Sex of donnor-patient","Karnofsky score","","Age at graft"),cspaic)




res.mart<-residuals(final_csp_603,type="martingale")
b<-which(is.na(csp_60))
c<-greffe$num_id[is.na(greffe$sex_dp)]
d<-greffe$num_id[is.na(greffe$karnofsky_greffec3)]
e<-greffe$num_id[is.na(greffe$nbr_lignes_avt_alloc)]

plot(greffe$age_greffe[!greffe$num_id %in% c(b,c,d,e)],res.mart)

lines(lowess(greffe$age_greffe[!greffe$num_id %in% c(b,c,d,e)],res.mart,iter=0))
