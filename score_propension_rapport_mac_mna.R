# greffematchit<-greffe[!is.na(greffe$age_greffe)
#                       & !is.na(greffe$nbr_lignes_avt_allo)
#                       & !is.na(greffe$sex_patient)
#                       # !is.na(greffe$stade_dia)
#                       & !is.na(greffe$anapathc2)
#                       & !is.na(greffe$centre)
#                       & !is.na(greffe$disease_status_at_transplantc)
#                       & !is.na(greffe$karnofsky_greffec2)
#                       & !is.na(greffe$rechute_prem_greffec)
#                       & !is.na(greffe$donnor)
#                       & !is.na(greffe$sex_dp3)
#                       & !is.na(greffe$cmv_dp2)
#                       & !is.na(greffe$stem_cell_source)
#                       & !is.na(greffe$tbi)
#                       & !is.na(greffe$rechute_progression_dc)
#                       & !is.na(greffe$pfs_60)
#                       &  !is.na(greffe$delai_pfs_60)
#                       & !is.na(greffe$rechute_progression_dc60)
#                       , c("num_id","intensite_condi2","age_greffe","nbr_lignes_avt_allo" ,
#                           "sex_patient" , "anapathc2",
#                           "centre" , "disease_status_at_transplantc",
#                           "karnofsky_greffec3" ,"rechute_prem_greffec","donnor",
#                           "sex_dp3","cmv_dp2","stem_cell_source","tbi","rechute_progressionc",
#                           "delai_pfs","rechute_progression_dc", "pfs_60","delai_pfs_60","rechute_progression_dc60","cause_death_c360","
#                           delai_dc_60","deces_60")]


#### Essai matching####

#av<-greffe$num_id[greffe$deces==1 & greffe$delai_dc<=3]
greffematchit<-greffe[ !is.na(greffe$age_greffe)
                       & !is.na(greffe$intensite_condi2)
                      & greffe$age_greffe <=50
                      & !is.na(greffe$nbr_lignes_avt_allo)
                      #& !is.na(greffe$sex_patient)
                      # !is.na(greffe$stade_dia)
                      & !is.na(greffe$anapathc2)
                      #& !is.na(greffe$centre)
                      & !is.na(greffe$disease_status_at_transplantc)
                      & !is.na(greffe$karnofsky_greffec3)
                      & greffe$karnofsky_greffe >70
                      & !is.na(greffe$previous_auto)
                      & greffe$previous_auto==0
                      & !is.na(greffe$donnor)
                      & !is.na(greffe$sex_dp3)
                      & !is.na(greffe$cmv_dp2)
                      & !is.na(greffe$stem_cell_source)
                      & ! greffe$stem_cell_source=="BM"
                      & !is.na(greffe$tbi)
                      & !is.na(greffe$rechute_progression_dc)
                      & !is.na(greffe$pfs_60)
                     &  !is.na(greffe$delai_pfs_60)
                     & !is.na(greffe$rechute_progression_dc60)
                     & !is.na(greffe$delai_dia_allo)
                     & !is.na(greffe$delai_dc_60)
                     & !is.na(greffe$deces_60)
                     & !is.na(greffe$cause_death_c360)
                     
                     
                       # & ! greffe$num_id %in%  av
                      , c("num_id","intensite_condi2","age_greffe","nbr_lignes_avt_allo" ,"nbr_lignes_avt_alloc",
                          "anapathc2","delai_dia_allo","delai_dia_alloc",
                          "disease_status_at_transplantc",
                          "karnofsky_greffec3" ,"karnofsky_greffe","rechute_prem_greffec","donnor",
                          "sex_dp3","cmv_dp2","stem_cell_source","tbi","rechute_progressionc",
                          "delai_pfs","rechute_progression_dc", "pfs_60","delai_pfs_60","rechute_progression_dc60",
                          "cause_death_c360","delai_dc_60","deces_60")]



greffematchit$intensite_condi2<-ifelse(greffematchit$intensite_condi2=="RIC/NMA",1,0)
table(greffematchit$intensite_condi2)
table(greffematchit$karnofsky_greffe)

table(greffematchit$nbr_lignes_avt_allo,greffematchit$intensite_condi2)
# greffematchit<-greffematchit[!greffematchit$centre %in% c("C.H.R.U Brest[659]",
#                                                          "Gustave Roussy[666",
#                                                                         "H Bretonneau[272]",
#                                                                         "H Charles Nicolle[932]",        
#                                                                         "H Sud[955]",
#                                                                         "liege",
#                                                                         "Pellegrin-Enfants[978]"),]
# 


#### Match it ####
set.seed(123)
testmatchit<-matchit(intensite_condi2 ~ age_greffe + nbr_lignes_avt_allo+
       disease_status_at_transplantc + delai_dia_alloc,caliper=0.15,
        data = greffematchit, method = "nearest",replace=FALSE)

greffematchit<-match.data(testmatchit)



### vérification qualité du matchin ###


# plot(testmatchit,type="jitter")
# fn_bal <- function(dta, variable) {
#   dta$variable <- dta[, variable]
# 
#   dta$intensite_condi2 <- as.factor(dta$intensite_condi2)
#   #support <- c(min(dta$variable), max(dta$variable))#
#   ggplot(dta, aes(x = distance, y = variable, color = intensite_condi2)) +
#     geom_point(alpha = 0.2, size = 1.3) +
#     geom_smooth(method = "loess", se = F) +
#     xlab("Propensity score") +
#     ylab(variable) +
#     theme_bw() 
#     #ylim(support)#
#   }
# 
# 
# 
# fn_bal(dta=greffematchit,"nbr_lignes_avt_allo" )
# fn_bal(dta=greffematchit,"age_greffe" )
# fn_bal(dta=greffematchit,"anapathc2" )
# fn_bal(dta=greffematchit,"sex_patient" )
# fn_bal(dta=greffematchit,"centre" )
# fn_bal(dta=greffematchit,"stem_cell_source" )
# fn_bal(dta=greffematchit,"disease_status_at_transplantc" )
# fn_bal(dta=greffematchit,"disease_status_at_transplantc" )
# fn_bal(dta=greffematchit,"donnor" )
# fn_bal(dta=greffematchit,"sex_dp3" )


summary(testmatchit)






### trouver le cas/t ###



db<-testmatchit$match.matrix
#db<-db[!is.na(db[,1]),]
greffematchit$clu<-NA

for (i in 1:nrow(db)){
  c<-rownames(db)[i]
  t<-db[i,1]
  greffematchit$clu<-ifelse(rownames(greffematchit)==c & is.na(greffematchit$clu),i,greffematchit$clu)
  greffematchit$clu<-ifelse(rownames(greffematchit)==t & is.na(greffematchit$clu),i,greffematchit$clu)

}


nonmatche<-rownames(db)[is.na(db)]

greffematchit<-greffematchit[!rownames(greffematchit)%in% nonmatche,]

####


# tapply(X = greffematchit[,"age_greffe"], INDEX = greffematchit$intensite_condi2,summary)
# tapply(X = greffematchit[,"nbr_lignes_avt_allo"], INDEX = greffematchit$intensite_condi2,summary)
# tapply(X = greffematchit[,"sex_patient"], INDEX = greffematchit$intensite_condi2,summary)
# tapply(X = greffematchit[,"anapathc2"], INDEX = greffematchit$intensite_condi2,summary)
# tapply(X = greffematchit[,"disease_status_at_transplantc"], INDEX = greffematchit$intensite_condi2,summary)
#tapply(X = greffematchit[,"karnofsky_greffe"], INDEX = greffematchit$intensite_condi2,table)
# tapply(X = greffematchit[,"rechute_prem_greffec"], INDEX = greffematchit$intensite_condi2,summary)
# tapply(X = greffematchit[,"donnor"], INDEX = greffematchit$intensite_condi2,summary)
# tapply(X = greffematchit[,"sex_dp3"], INDEX = greffematchit$intensite_condi2,summary)
# tapply(X = greffematchit[,"cmv_dp2"], INDEX = greffematc

# tapply(X = greffematchit[,"stem_cell_source"], INDEX = greffematchit$intensite_condi2,summary)
# tapply(X = greffematchit[,"centre"], INDEX = greffematchit$intensite_condi2,summary)


verif<-summary(testmatchit,standardize = TRUE)

aploter<-data.frame(cbind( var=1:7,avant=verif$sum.all[,4],apres=verif$sum.matched[,4]))


plot(y = aploter$var,x=aploter$avant)
points(y = aploter$var,x=aploter$apres,col="red")
abline(v=0)

plot(testmatchit)

plot(testmatchit,type="hist")

### test analyse ###
result.coxr <- function (modele)
{
  summary(modele)$conf.int ->  modele.detail
  res <- data.frame (Variable = names(modele$coef))
  res$HR <- round(modele.detail[,1],2)
  res$IC <- paste("[" , round(modele.detail[,3],2) , " - " , round(modele.detail[,4],2) ,"]",sep="")
  for (j in 1:length(res$IC))
  {
    res$pval[j] <- as.character(format.pv(summary(modele)$coef[j,6]))
  }
  res$Variable<- gsub("greffe\\[, i\\]","",res$Variable)
  titre<-data.frame(Variable=ifelse(is.factor(greffe[,i]),levels(greffe[,i][1]),""),HR=ifelse(is.factor(greffe[,i]),1.00,NA),IC=NA,pval=NA)
  res<-rbind(titre,res)
  res$p<-c(as.character(format.pv(summary(modele)$logtest[3])),rep("",nrow(res)-1))  
  return(res)
}







pfssp<-coxph(Surv(event=greffematchit$pfs_60,time=greffematchit$delai_pfs_60)~intensite_condi2+cluster(clu), greffematchit) 
tpfssp<-result.coxr(pfssp)
tpfssp[,1]<-c("MAC","RIC/NMA")



cspsp<-coxph(Surv(event=greffematchit$cause_death_c360,time=greffematchit$delai_dc_60)~intensite_condi2+cluster(clu), greffematchit) 
tcspsp<-result.coxr(cspsp)
tcspsp[,1]<-c("MAC","RIC/NMA")

ossp<-coxph(Surv(event=greffematchit$deces_60,time=greffematchit$delai_dc_60)~intensite_condi2+cluster(clu), greffematchit) 
tossp<-result.coxr(ossp)
tossp[,1]<-c("MAC","RIC/NMA")


greffematchit$rechute_progression_dc60f<-as.factor(greffematchit$rechute_progression_dc60)
greffematchit$intensite_condi2<-as.matrix(greffematchit$intensite_condi2)


greffematchitp<-crrc(ftime=greffematchit$delai_pfs_60, fstatus =greffematchit$rechute_progression_dc60f,cov1=greffematchit$intensite_condi2,
cluster=greffematchit$clu)

tgreffematchitp<-NULL
tgreffematchitp$hr<-c(1,round(exp(greffematchitp$coef),2))
tgreffematchitp$ic<-c(NA,paste("[",round(exp(greffematchitp$coef-1.96*sqrt(greffematchitp$var)),2),"-", 
  round(exp(greffematchitp$coef+1.96*sqrt(greffematchitp$var)),2),"]") )
tgreffematchitp$p<-c(NA,gsub("\\[[0-9]{1}\\]","",capture.output(greffematchitp)[7]))

tgreffematchitp<-data.frame(cbind(tossp[,1],tgreffematchitp$hr,tgreffematchitp$ic,tgreffematchitp$p))
colnames(tgreffematchitp)<-c("Variables","HR","IC","pval")

# 
# #### suite ####
# fgwt<-greffematchitp$fgwt
# coxph(Surv(event=fgstatus,time=fgstart,time2=fgstop)~
#         intensite_condi2+cluster(clu),weights=fgwt, greffematchitp) 
# 
# 
# greffe_longpfsanp<-left_join(greffe_longpfsan, greffematchit[,c("num_id",'clu',"intensite_condi2")],by="num_id")
# 
# 
# summary(coxph(Surv(time=as.numeric(start), time2=as.numeric(stop), event=pfs60f) ~cgvhd+ intensite_condi2
#               + cgvhd*intensite_condi2+ cluster(clu), greffe_longpfsanp))
# 
# 
# ### Conclusion : les cas et les témoins sont trop différents au controles visuels sans mettre un caliper mais p
# #peu de matching donc ipw 
# greffe$intensite_condi2b<-ifelse(greffe$intensite_condi2=="RIC",1,0)
# 
# greffematchit<-greffematchit[order(greffematchit$num_id),]
# 
# test<-ipwpoint(intensite_condi2, family="binomial", link="logit", numerator = ~1,
#          denominator=~ age_greffe + nbr_lignes_avt_allo +sex_patient  + anapathc2+
#            centre + disease_status_at_transplantc+ karnofsky_greffec3 + rechute_prem_greffec
#          + donnor + sex_dp3+cmv_dp2 + stem_cell_source + tbi, greffematchit)
# greffematchit$wa <- test$ipw.weights
# greffematchit$id <- 1:nrow(greffematchit)
# wa <- test$ipw.weights
# 
# 
# coxph(Surv(event=greffematchit$pfs_60,time=greffematchit$delai_pfs_60)~intensite_condi2+cluster(id),weights=wa, greffematchit) 
# coxph(Surv(event=greffematchit$pfs_60,time=greffematchit$delai_pfs_60)~intensite_condi2, greffematchit) 
# 
# 
# ### ajout gvh : peut -on ajouter une variable qui dépend du temps avec des pondération ? 
# 
# greffematchittps<-left_join(greffe_longpfsan, greffematchit[,c("intensite_condi2","wa","num_id","pfs_60","delai_pfs_60")], by="num_id")
# 
# coxph(Surv(time=as.numeric(start), time2=as.numeric(stop), event=pfs60f)~intensite_condi2,weights=wa, greffematchittps) 
# 
# coxph(Surv(time=as.numeric(start), time2=as.numeric(stop), event=pfs60f)~intensite_condi2+cgvhd*intensite_condi2+cluster(id),weights=wa, greffematchittps) 
# 
# coxph(Surv(time=as.numeric(start), time2=as.numeric(stop), event=pfs60f)~intensite_condi2+cgvhd+cluster(id),weights=wa, greffematchittps) 
# 
# 
# 
# 
# ### Risque compétitifs quid de la pondération ?###
# greffematchit<-greffematchit[!is.na(greffematchit$rechute_progression_dc) & 
#                                !is.na(greffematchit$delai_pfs),]
# greffematchit$rechute_progression_dc<-as.factor(greffematchit$rechute_progression_dc)
# # faire cause spécifque de mortalité : 
# poidsfg<-finegray(Surv(event=greffematchit$rechute_progression_dc,time=greffematchit$delai_pfs) ~ ., data=greffematchit)
# 
# 
# 
# 
# etime <- with(mgus2, ifelse(pstat==0, futime, ptime))
# event <- with(mgus2, ifelse(pstat==0, 2*death, 1))
# event <- factor(event, 0:2, labels=c("censor", "pcm", "death"))
# 
# # FG model for PCM
# pdata <- finegray(Surv(etime, event) ~ ., data=mgus2)