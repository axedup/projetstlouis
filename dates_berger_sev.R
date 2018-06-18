### Dates pour survie dépendantes du temps sans amputations ni evenement  ###

# prendre les dates, les nip et les délai d'intéret
# mettre en NA les valeurs avant la date de diag,ou après la durée de suivi de 15 ans 
# créer des indicateur


# berger$date_eve_amput_deces15<-ifelse(berger$delai_amput_deces<=15,as.numeric(berger$date_amput_deces),as.numeric(berger$date_diag) +365*15)
# 
# berger$amput_deces15<-ifelse(berger$delai_amput_deces<=15,berger$amput_deces,0)

dates<-berger[,c("nip","date_diag","date_colchi","date_fin_colchi","date_asp","date_fin_asp","date_pla",
                 "date_fin_pla","date_coag","date_fin_coag","date_statin","date_fin_statin",
                 "date_inh","date_fin_inh","date_vas","date_fin_vas","date_iec","date_fin_iec",
                 "date_gv1","date_gv2","date_gv3","date_gv4","date_gv5","date_eve_amput_deces15","eve_amput_deces15")]

for(i in c("date_diag","date_colchi","date_fin_colchi","date_asp","date_fin_asp","date_pla",
           "date_fin_pla","date_coag","date_fin_coag","date_statin","date_fin_statin",
           "date_inh","date_fin_inh","date_vas","date_fin_vas","date_iec","date_fin_iec",
           "date_gv1","date_gv2","date_gv3","date_gv4","date_gv5","date_eve_amput_deces15") ){
  
  #berger[,i]<-ifelse (berger[,i]<=berger$date_diag,berger$date_diag ,berger[,i])
  dates[,i]<-as.numeric(dates[,i])
}





for(i in c("date_colchi","date_asp","date_pla",
           "date_coag","date_statin",
           "date_inh","date_vas","date_iec",
           "date_gv1","date_gv2","date_gv3","date_gv4","date_gv5") ){
  
  #berger[,i]<-ifelse (berger[,i]<=berger$date_diag,berger$date_diag ,berger[,i])
  dates[,i]<-ifelse (dates[,i]>=dates$date_eve_amput_deces15,NA ,dates[,i])
}



dates$date_fin_colchi<-ifelse(is.na(dates$date_colchi),NA,dates$date_fin_colchi)
dates$date_fin_asp<-ifelse(is.na(dates$date_asp),NA,dates$date_fin_asp)
dates$date_fin_pla<-ifelse(is.na(dates$date_pla),NA,dates$date_fin_pla)
dates$date_fin_coag<-ifelse(is.na(dates$date_coag),NA,dates$date_fin_coag)
dates$date_fin_statin<-ifelse(is.na(dates$date_statin),NA,dates$date_fin_statin)
dates$date_fin_inh<-ifelse(is.na(dates$date_inh),NA,dates$date_fin_inh)
dates$date_fin_vas<-ifelse(is.na(dates$date_vas),NA,dates$date_fin_vas)
dates$date_fin_iec<-ifelse(is.na(dates$date_iec),NA,dates$date_fin_iec)





for(i in c("date_fin_colchi","date_fin_asp",
           "date_fin_pla","date_fin_coag","date_fin_statin",
           "date_fin_inh","date_fin_vas","date_fin_iec") ){
  
  #berger[,i]<-ifelse (berger[,i]<=berger$date_diag,berger$date_diag ,berger[,i])
  dates[,i]<-ifelse (dates[,i]<=dates$date_diag,NA ,dates[,i])
}


dates$date_colchi<-ifelse(is.na(dates$date_fin_colchi),NA,dates$date_colchi)
dates$date_asp<-ifelse(is.na(dates$date_fin_asp),NA,dates$date_asp)
dates$date_pla<-ifelse(is.na(dates$date_fin_pla),NA,dates$date_pla)
dates$date_coag<-ifelse(is.na(dates$date_fin_coag),NA,dates$date_coag)
dates$date_statin<-ifelse(is.na(dates$date_fin_statin),NA,dates$date_statin)
dates$date_inh<-ifelse(is.na(dates$date_fin_inh),NA,dates$date_inh)
dates$date_vas<-ifelse(is.na(dates$date_fin_vas),NA,dates$date_vas)
dates$date_iec<-ifelse(is.na(dates$date_fin_iec),NA,dates$date_iec)

for(i in c("date_gv1","date_gv2","date_gv3","date_gv4","date_gv5")){
  
  dates[,i]<-ifelse (dates[,i]< dates$date_diag,NA,dates[,i])
}



for(i in c("date_colchi","date_asp","date_pla",
           "date_coag","date_statin",
           "date_inh","date_vas","date_iec",
           "date_gv1","date_gv2","date_gv3","date_gv4","date_gv5")){
  
  dates[,i]<-ifelse (dates[,i]<=dates$date_diag,dates$date_diag ,dates[,i])
}


for(i in c("date_fin_colchi","date_fin_asp",
           "date_fin_pla","date_fin_coag","date_fin_statin",
           "date_fin_inh","date_fin_vas","date_fin_iec") ){
  
  #berger[,i]<-ifelse (berger[,i]<=berger$date_diag,berger$date_diag ,berger[,i])
  dates[,i]<-ifelse (dates[,i]>dates$date_eve_amput_deces15,dates$date_eve_amput_deces15 ,dates[,i])
}
PERSEV<-NULL
j=55

for (j in berger$nip ){
  
  statut <-dates[dates$nip==j, c("date_eve_amput_deces15","eve_amput_deces15")]
  
  
  
  personne<-dates[dates$nip==j, c("date_diag","date_colchi","date_fin_colchi","date_asp","date_fin_asp","date_pla",
                                  "date_fin_pla","date_coag","date_fin_coag","date_statin","date_fin_statin",
                                  "date_inh","date_fin_inh","date_vas","date_fin_vas","date_iec","date_fin_iec",
                                  "date_gv1","date_gv2","date_gv3","date_gv4","date_gv5","date_eve_amput_deces15")]
  personnea<-unlist(personne)
  personneb<-personnea[order(personnea)]
  personnec<-as.data.frame(personneb)
  personnec$value<-rownames(personnec)
  
  
  personnec<-personnec[!is.na(personnec$personneb),]
  
  
  personnec$colchi<-NA
  personnec$colchi<-ifelse(personnec$value=="date_colchi" & !is.na(personnec$personneb),1,
                           ifelse(personnec$value=="date_fin_colchi" & !is.na(personnec$personneb),0,personnec$colchi))
  deb<-match(1,personnec$colchi)
  fin<-match(0,personnec$colchi)
  if(!is.na(deb) & !is.na(fin)){
    personnec$colchi[1:deb-1]<-rep(0,deb-1)
    
    personnec$colchi[(fin+1):nrow(personnec)]<-rep(0,nrow(personnec)-fin)
  }
  
  if(!is.na(deb) & !is.na(fin) & fin-deb > 1){
    personnec$colchi[(deb+1):(fin-1 )]<-rep(1,fin-deb-1)
  }
  
  
  personnec$asp<-NA
  personnec$asp<-ifelse(personnec$value=="date_asp" & !is.na(personnec$personneb),1,
                        ifelse(personnec$value=="date_fin_asp" & !is.na(personnec$personneb),0,
                               personnec$asp))
  deb<-match(1,personnec$asp)
  fin<-match(0,personnec$asp)
  if(!is.na(deb) & !is.na(fin)){
    personnec$asp[1:deb-1]<-rep(0,deb-1)
    
    personnec$asp[(fin+1):nrow(personnec)]<-rep(0,nrow(personnec)-fin)
  }
  
  if(!is.na(deb) & !is.na(fin) & fin-deb > 1){
    personnec$asp[(deb+1):(fin-1 )]<-rep(1,fin-deb-1)
  }
  
  
  
  personnec$pla<-NA
  personnec$pla<-ifelse(personnec$value=="date_pla" & !is.na(personnec$personneb),1,
                        ifelse(personnec$value=="date_fin_pla" & !is.na(personnec$personneb),0,
                               personnec$pla))
  deb<-match(1,personnec$pla)
  fin<-match(0,personnec$pla)
  if(!is.na(deb) & !is.na(fin)){
    personnec$pla[1:deb-1]<-rep(0,deb-1)
    
    personnec$pla[(fin+1):nrow(personnec)]<-rep(0,nrow(personnec)-fin)
  }
  
  if(!is.na(deb) & !is.na(fin) & fin-deb > 1){
    personnec$pla[(deb+1):(fin-1 )]<-rep(1,fin-deb-1)
    
  }
  
  
  personnec$coag<-NA
  personnec$coag<-ifelse(personnec$value=="date_coag" & !is.na(personnec$personneb),1,
                         ifelse(personnec$value=="date_fin_coag" & !is.na(personnec$personneb),0,
                                personnec$coag))
  deb<-match(1,personnec$coag)
  fin<-match(0,personnec$coag)
  if(!is.na(deb) & !is.na(fin)){
    personnec$coag[1:deb-1]<-rep(0,deb-1)
    
    personnec$coag[(fin+1):nrow(personnec)]<-rep(0,nrow(personnec)-fin)
  }
  
  if(!is.na(deb) & !is.na(fin) & fin-deb > 1){
    personnec$coag[(deb+1):(fin-1 )]<-rep(1,fin-deb-1)
  }
  
  
  personnec$statin<-NA
  personnec$statin<-ifelse(personnec$value=="date_statin" & !is.na(personnec$personneb),1,
                           ifelse(personnec$value=="date_fin_statin" & !is.na(personnec$personneb),0,
                                  personnec$statin))
  deb<-match(1,personnec$statin)
  fin<-match(0,personnec$statin)
  if(!is.na(deb) & !is.na(fin)){
    personnec$statin[1:deb-1]<-rep(0,deb-1)
    
    personnec$statin[(fin+1):nrow(personnec)]<-rep(0,nrow(personnec)-fin)
  }
  
  if(!is.na(deb) & !is.na(fin) & fin-deb > 1){
    personnec$statin[(deb+1):(fin-1 )]<-rep(1,fin-deb-1)
  }
  
  personnec$inh<-NA
  personnec$inh<-ifelse(personnec$value=="date_inh" & !is.na(personnec$personneb),1,
                        ifelse(personnec$value=="date_fin_inh" & !is.na(personnec$personneb),0,
                               personnec$inh))
  deb<-match(1,personnec$inh)
  fin<-match(0,personnec$inh)
  if(!is.na(deb) & !is.na(fin)){
    personnec$inh[1:(deb-1)]<-rep(0,deb-1)
    
    personnec$inh[(fin+1):nrow(personnec)]<-rep(0,nrow(personnec)-fin)
  }
  
  if(!is.na(deb) & !is.na(fin) & fin-deb > 1){
    personnec$inh[(deb+1):(fin-1 )]<-rep(1,fin-deb-1)
  } 
  
  personnec$vas<-NA
  personnec$vas<-ifelse(personnec$value=="date_vas" & !is.na(personnec$personneb),1,
                        ifelse(personnec$value=="date_fin_vas" & !is.na(personnec$personneb),0,
                               personnec$vas))
  deb<-match(1,personnec$vas)
  fin<-match(0,personnec$vas)
  if(!is.na(deb) & !is.na(fin)){
    personnec$vas[1:deb-1]<-rep(0,deb-1)
    
    personnec$vas[(fin+1):nrow(personnec)]<-rep(0,nrow(personnec)-fin)
  }
  
  if(!is.na(deb) & !is.na(fin) & fin-deb > 1){
    personnec$vas[(deb+1):(fin-1 )]<-rep(1,fin-deb-1)
  }
  
  personnec$iec<-NA
  personnec$iec<-ifelse(personnec$value=="date_iec" & !is.na(personnec$personneb),1,
                        ifelse(personnec$value=="date_fin_iec" & !is.na(personnec$personneb),0,
                               personnec$iec))
  
  deb<-match(1,personnec$iec)
  fin<-match(0,personnec$iec)
  if(!is.na(deb) & !is.na(fin)){
    personnec$iec[1:deb-1]<-rep(0,deb-1)
    
    personnec$iec[(fin+1):nrow(personnec)]<-rep(0,nrow(personnec)-fin)
  }
  
  if(!is.na(deb) & !is.na(fin) & fin-deb > 1){
    personnec$iec[(deb+1):(fin-1 )]<-rep(1,fin-deb-1)}
  
  personnec$tt<-ifelse(personnec$colchi==1 | personnec$asp==1 | personnec$pla==1| 
                         personnec$coag==1|personnec$statin==1
                       |personnec$inh==1| personnec$vas==1,1,0 )
  
  
  
  
  
  
  personnec$tt<-ifelse((personnec$colchi==0| is.na(personnec$colchi)| personnec$asp==0 |
                          is.na(personnec$asp)|
                          personnec$pla==0| is.na(personnec$pla)|
                          personnec$coag==0|is.na(personnec$coag)|
                          personnec$statin==0 |is.na(personnec$statin)|
                          personnec$inh==0| is.na(personnec$inh)|personnec$vas==0| is.na(personnec$vas)) 
                       &  (!personnec$tt==1 | is.na(personnec$tt)) ,0,personnec$tt )
  
  
  for(p in 1: nrow(personnec)){
    
    d<-personnec$personneb[p] 
    remp<-max(personnec$tt[personnec$personneb==d],na.rm=TRUE)
    personnec$tt<-remp
    
    
  }
  
  personnec$v1<-ifelse(personnec$value=="date_gv1" & !is.na(personnec$personneb),1,NA)
  personnec$v2<-ifelse(personnec$value=="date_gv2" & !is.na(personnec$personneb),1,NA)
  personnec$v3<-ifelse(personnec$value=="date_gv3" & !is.na(personnec$personneb),1,NA)
  personnec$v4<-ifelse(personnec$value=="date_gv4" & !is.na(personnec$personneb),1,NA)
  personnec$v5<-ifelse(personnec$value=="date_gv5" & !is.na(personnec$personneb),1,NA)
  
  personnec$revas<-ifelse(personnec$v1==1|personnec$v2==1|personnec$v3==1|personnec$v4==1|
                            personnec$v5==1,1,0)
  
  
  deb<-match(1,personnec$revas)
  
  if(!is.na(deb)){
    personnec$revas[1:deb-1]<-rep(0,deb-1)
    
    personnec$revas[(deb+1):nrow(personnec)]<-rep(1,nrow(personnec)-deb)
  }
  
  
  if(is.na(deb)){
    personnec$revas<-rep(0,length(personnec$revas))
    
    
  }
  
  
  
  for(p in 1: nrow(personnec)){
    
    d<-personnec$personneb[p] 
    remp<-max(personnec$revas[personnec$personneb==d],na.rm=TRUE)
    personnec$revas<-remp
    
    
  }
  
  
  ### ajout nip ###
  
  personnec$nip<-rep(j,nrow(personnec))
  
  
  ### ajout statut 
  
  personnec<-merge(personnec,statut,by.x="personneb",by.y="date_eve_amput_deces15",all.x=TRUE)
  personnec$eve_amput_deces15<-ifelse(is.na(personnec$eve_amput_deces15),0, personnec$eve_amput_deces15)
  
  
  ### supression ligne inutuile : pour l'instant on travaille sur une seule revascularisation donc on se fiche de v1,v2
  
  # personned<-subset(personnec,!duplicated(personnec[,c("personneb","tt","v1","v2","v2","v3"
  #                                                      ,"v4","v5","eve_amput_deces15","revas")]))
  
  
  personned<-subset(personnec,!duplicated(personnec[,c("personneb","tt","eve_amput_deces15","revas")]))
  
  
  ### préparer les dates 
  
  personned$start<-personned$personneb-personned$personneb[1]
  
  personned$end<-NA
  personned$daf15<-NA
  #data$pfs60f<-NA
  for (j in 1: nrow(personned)-1){
    personned$end[j]<-personned$start[j+1]
    personned$daf15[j]<-personned$eve_amput_deces15[j+1]
    #data$pfs60f[j]<-data$pfs[j+1]
  }
  
  personned$end[nrow(personned)]<-personned$start[nrow(personned)]
  #data$end<-as.Date(data$end,origin="1970-01-01")
  #data$TT[1]<-"Chmioth"
  personned$daf15[nrow(personned)]<-NA
  #data$pfs60f[nrow(data)]<-NA
  
  
  
  
  
  
  PERSEV<-rbind(PERSEV,personned)
}


PERSEV<-PERSEV[!is.na(PERSEV$daf15),]

PERS$check<-PERS$start-PERS$end

result.coxt2 <- function (modele)
{
  summary(modele)$conf.int ->  modele.detail
  res <- data.frame (Variable = names(modele$coef))
  res$HR <- round(modele.detail[,1],2)
  res$IC <- paste("[" , round(modele.detail[,3],2) , " - " , round(modele.detail[,4],2) ,"]",sep="")
  for (j in 1:length(res$IC))
  {
    res$pval[j] <- as.character(format.pv(summary(modele)$coef[j,5]))
  }
  res$Variable<- gsub("PERSEV\\[, i\\]","",res$Variable)
  titre<-data.frame(Variable=ifelse(is.factor(PERS[,i]),levels(PERS[,i][1]),""),HR=ifelse(is.factor(PERS[,i]),1.00,NA),IC=NA,pval=NA)
  res<-rbind(titre,res)
  res$p<-c(as.character(format.pv(summary(modele)$logtest[3])),rep("",nrow(res)-1))  
  return(res)
}


OSTEV<-NULL

for (i in c( "revas","tt")
){
  mo<-coxph( Surv(time=as.numeric(start), time2=as.numeric(end), event=daf15)~PERSEV[,i],data=PERSEV)  
  
  qq<-result.coxt2(mo)
  OSTEV<-rbind(OSTEV,qq)
}

