### survie d�pendante du temps : OS  ### 

greffe<-greffe[order(greffe$num_id),]

greffe$j060t<-as.numeric(greffe$j0)
greffe$agvhd_date60t<-ifelse(difftime(greffe$agvhd_date,greffe$j0)/30.25<60 & greffe$agvhd3==1 & !is.na(greffe$agvhd3),greffe$agvhd_date,NA)
greffe$cgvhd_date60t<-ifelse(difftime(greffe$cgvhd_date,greffe$j0)/30.25<60,greffe$cgvhd_date,NA)
greffe$date_fu60t<-ifelse(difftime(greffe$date_fu,greffe$j0)/30.25<60,greffe$date_fu,greffe$j0+(60*30.25))

### Garde 3-4 

greffe$agvhd_trash0<-0
greffe$agvhd_trash1<-ifelse(!is.na(greffe$agvhd_date) & difftime(greffe$agvhd_date,greffe$j0)/30.25<60 & greffe$agvhd3==1 & !is.na(greffe$agvhd3),1,0)
greffe$agvhd_trash2<-ifelse(!is.na(greffe$agvhd_date) & difftime(greffe$agvhd_date,greffe$j0)/30.25<60 & greffe$agvhd3==1 & !is.na(greffe$agvhd3),1,0)
greffe$agvhd_trash3<-ifelse(!is.na(greffe$agvhd_date) & difftime(greffe$agvhd_date,greffe$j0)/30.25<60 & greffe$agvhd3==1 & !is.na(greffe$agvhd3),1,0)

greffe$cgvhd_trash0<-0
greffe$cgvhd_trash1<-0
greffe$cgvhd_trash2<-ifelse(!is.na(greffe$cgvhd_date) & difftime(greffe$cgvhd_date,greffe$j0)/30.25<60,1,0)
greffe$cgvhd_trash3<-ifelse(!is.na(greffe$cgvhd_date) & difftime(greffe$cgvhd_date,greffe$j0)/30.25<60,1,0)


greffe$deces_60trash0<-0
greffe$deces_60trash1<-0
greffe$deces_60trash2<-0
greffe$deces_60trash3<-ifelse(greffe$deces_60==1,1,0)
ifelse(greffe$deces_60==1,1,0)


### non ####
greffe$pfs_60trash0<-0
greffe$pfs_60trash1<-0
greffe$pfs_60trash2<-0
greffe$pfs_60trash3<-ifelse(greffe$pfs_60==1,1,0)

### non###
# greffe$efs_60trash0<-0
# greffe$efs_60trash1<-0
# greffe$efs_60trash2<-ifelse(greffe$efs_60==1,1,0)
# greffe$efs_60trash3<-ifelse(greffe$efs_60==1,1,0)




greffe_l<-reshape(greffe[,c("num_id","j060t","agvhd_date60t","cgvhd_date60t","date_fu60t")], varying =
                    c("j060t","agvhd_date60t","cgvhd_date60t","date_fu60t"), v.names="dates",
                  timevar="ref", times=c("j060t","agvhd_date60t","cgvhd_date60t","date_fu60t"),
                  direction="long")
# 
# 
greffe_l<-greffe_l[order(greffe_l$num_id),]





greffe_l2<-reshape(greffe[,c("num_id","deces_60trash0","deces_60trash1","deces_60trash2","deces_60trash3")], varying =
                     c("deces_60trash0","deces_60trash1","deces_60trash2","deces_60trash3"), v.names="deces",
                   timevar="ref2", times=c("deces_60trash0","deces_60trash1","deces_60trash2","deces_60trash3"),
                   direction="long")
# 
# 
greffe_l2<-greffe_l2[order(greffe_l2$num_id),]



greffe_l3<-reshape(greffe[,c("num_id","agvhd_trash0","agvhd_trash1","agvhd_trash2","agvhd_trash3")], varying =
                     c("agvhd_trash0","agvhd_trash1","agvhd_trash2","agvhd_trash3"), v.names="agvhd",
                   timevar="refa", times=c("agvhd_trash0","agvhd_trash1","agvhd_trash2","agvhd_trash3"),
                   direction="long")
# 
# 
greffe_l3<-greffe_l3[order(greffe_l3$num_id),]

greffe_l4<-reshape(greffe[,c("num_id","cgvhd_trash0","cgvhd_trash1","cgvhd_trash2","cgvhd_trash3")], varying =
                     c("cgvhd_trash0","cgvhd_trash1","cgvhd_trash2","cgvhd_trash3"), v.names="cgvhd",
                   timevar="refc", times=c("cgvhd_trash0","cgvhd_trash1","cgvhd_trash2","cgvhd_trash3"),
                   direction="long")
# 
# 
greffe_l4<-greffe_l4[order(greffe_l4$num_id),]


greffe_l5<-reshape(greffe[,c("num_id","pfs_60trash0","pfs_60trash1","pfs_60trash2","pfs_60trash3")], varying =
                     c("pfs_60trash0","pfs_60trash1","pfs_60trash2","pfs_60trash3"), v.names="pfs",
                   timevar="refd", times=c("pfs_60trash0","pfs_60trash1","pfs_60trash2","pfs_60trash3"),
                   direction="long")
# 
# 
greffe_l5<-greffe_l5[order(greffe_l5$num_id),]





greffe_trash<-cbind(greffe_l,greffe_l2,greffe_l3,greffe_l4,greffe_l5) 
greffe_trash<-greffe_trash[!is.na(greffe_trash$dates),]
greffe_trash$dates<-as.numeric(greffe_trash$dates)

greffe_long<-NULL 
for( i in unique(greffe_trash$num_id)){
  
  data<-greffe_trash[greffe_trash$num_id==i,]
  data$end<-NA
  data$decesf<-NA
  data$pfs60f<-NA
  for (j in 1: nrow(data)-1){
    data$end[j]<-data$dates[j+1]
    data$decesf[j]<-data$deces[j+1]
    data$pfs60f[j]<-data$pfs[j+1]
  }
  
  data$end[nrow(data)]<-data$dates[nrow(data)]
  #data$end<-as.Date(data$end,origin="1970-01-01")
  #data$TT[1]<-"ChmiothÃÂ©rapie non dÃÂ©butÃÂ©e"
  data$decesf[nrow(data)]<-NA
  data$pfs60f[nrow(data)]<-NA
  greffe_long<-rbind(greffe_long,data)
}


greffe_long

greffe_long$dt<-greffe_long$end-greffe_long$dates

greffe_long2<-greffe_long[!greffe_long$dt==0,]
table(greffe_long2$decesf,exclude=NULL)

### Calcul D�LAI : logique on ne travaille pas avec les dates et chaque patient n'est pas suivi de tel date � telle date
###


a<-table(greffe_long2$num_id)
b<-rep(greffe$j060t,a)
greffe_long2$start<-greffe_long2$dates-b
greffe_long2$stop<-greffe_long2$end-b

greffe_long2$cgvhd<-as.factor(greffe_long2$cgvhd)
greffe_long2$agvhd<-as.factor(greffe_long2$agvhd)


# ### Deces 60j ###
# 
# decestpscg<-coxph(Surv(time=as.numeric(start), time2=as.numeric(stop), event=decesf) ~cgvhd ,greffe_long2)
# result.cox(decestpscg)
# 
# 
# decestpsag<-summary(coxph(Surv(time=as.numeric(start), time2=as.numeric(stop), event=decesf) ~agvhd+ cluster(num_id), greffe_long2))
# 
# 
# coxph(Surv(time=as.numeric(dates), time2=as.numeric(end), event=decesf) ~cgvhd, greffe_long2)
# 
# # 
# # 
# # 
# # result.coxt <- function (modele)
# # {
# #   summary(modele)$conf.int ->  modele.detail
# #   res <- data.frame (Variable = names(modele$coef))
# #   res$HR <- round(modele.detail[,1],2)
# #   res$IC <- paste("[" , round(modele.detail[,3],2) , " - " , round(modele.detail[,4],2) ,"]",sep="")
# #   for (j in 1:length(res$IC))
# #   {
# #     res$pval[j] <- as.character(format.pv(summary(modele)$coef[j,5]))
# #   }
# #   res$Variable<- gsub("greffe_long2\\[, i\\]","",res$Variable)
# #   titre<-data.frame(Variable=ifelse(is.factor(greffe_long2[,i]),levels(greffe_long2[,i][1]),""),HR=ifelse(is.factor(greffe_long2[,i]),1.00,NA),IC=NA,pval=NA)
# #   res<-rbind(titre,res)
# #   res$p<-c(as.character(format.pv(summary(modele)$logtest[3])),rep("",nrow(res)-1))  
# #   return(res)
# # }
# # 
# # 
# # OST<-NULL
# # 
# # for (i in c( "agvhd","cgvhd")
# # ){
# #   mo<-coxph( Surv(time=as.numeric(start), time2=as.numeric(stop), event=decesf)~ greffe_long2[,i],data=greffe_long2)  
# #  
# #   qq<-result.coxt(mo)
# #   OST<-rbind(OST,qq)
# # }
# 
# 
# 
# 
# 
# 
# 
# greffe_longdeces<-left_join(greffe_long2[,c("num_id", "ref", "dates",  "ref2", "deces", 
#                                             "refa", "agvhd", "refc", "cgvhd", 
#                                             "id", "num_id", "refd", "pfs", "end", "decesf", "pfs60f", 
#                                             "dt","start","stop")], greffe[,
#   c("num_id","anapathc2", "disease_status_at_transplantc",
#   "karnofsky_greffec3" ,
#   "rechute_post_allo",
#   "nbr_lignes_avt_alloc",
#   "donnor", "stem_cell_source","sex_dp3")], by="num_id")
# 
# greffe_longdeces<-greffe_longdeces[order(greffe_longdeces$num_id),]
# 
# head(greffe_longdeces)
# 
# 
# greffe_longdeces$agvhd<-as.factor(greffe_longdeces$agvhd)
# 
# 
# 
# # summary(coxph(Surv(time=as.numeric(dt), event=decesf) ~
# #                             
# #                 rechute_post_allo+ cluster(num_id),greffe_longdeces))
# 
# 
# ### Test on doit avoir les m�mes r�sultats ### 
# coxph(Surv(time=as.numeric(start), time2=as.numeric(stop), event=decesf) ~rechute_post_allo, greffe_longdeces)
# coxph(Surv(time=as.numeric(start), time2=as.numeric(stop), event=decesf) ~sex_dp3, greffe_longdeces)
# 
# 
# decestpstt<-summary(coxph(Surv(time=as.numeric(start), time2=as.numeric(stop), event=decesf) ~agvhd+
#                             
#                             anapathc2 +rechute_post_allo +
#                             nbr_lignes_avt_alloc +
#                             donnor + stem_cell_source + sex_dp3+
#                             disease_status_at_transplantc + karnofsky_greffec3
#                    , greffe_longdeces))
# 
# 
# repli<-function(data,i){
#   
#   re<-rep(i,length(levels(data[,i]))-1)
#   return(re)
# }
# 
# l<-lapply(list("agvhd","anapathc2","rechute_post_allo","nbr_lignes_avt_alloc","donnor","stem_cell_source"
#                ,"sex_dp3","disease_status_at_transplantc","karnofsky_greffec3"),repli,data=greffe_longdeces)
# 
# l<-unlist(l)
# 
# 
# # corres<-data.frame( v=c("agvhd","anapathc2","rechute_post_allo","nbr_lignes_avt_alloc","donnor","stem_cell_source"
# #                         ,"sex_dp3","disease_status_at_transplantc","karnofsky_greffec3"),
# #                     nom=c("Agvhd grade 3-4","Histologic subtypes","Relapse post allo","N lines","HLA match","stem cell source"
# #                           ,"Sex d/p","Disease status at transplant","Karnofsky score"))
# # 
# # 
# # repli<-function(data,i){
# #   
# #   re<-rep(i,length(levels(data[,i]))-1)
# #   return(re)
# # }
# 
# cox_multi<-function(model, var){
#   res2<-NULL
#   row<-attr(model$coefficients,"dimnames")[[1]]
#   for(i in 1:nrow(model$coefficients))
#   {
#   
#       resi <- c(gsub(pattern=var[i],replacement="", row[i]),paste(format.hr(model$coefficients[i,2])," (",format.hr(model$conf.int[i,3]),"--", 
#                       format.hr(model$conf.int[i,4]), ")",sep=""),format.pv(model$coefficients[i,5]))
#     res2 <- rbind(res2,resi)
#   }
#   
# 
#   colnames(res2) <- c("Variable","HR (95\\%CI)","\\emph{P}")
#   return(res2)
# }
# 
#   
# finalosgl<-greffe_longdeces[complete.cases(greffe_longdeces[,c("agvhd","anapathc2","rechute_post_allo","nbr_lignes_avt_alloc","donnor","stem_cell_source"
#                                                                 ,"sex_dp3","disease_status_at_transplantc","karnofsky_greffec3")]),]        
# 
# stepAIC(coxph(Surv(time=as.numeric(start), time2=as.numeric(stop), event=decesf) ~agvhd+
#                 
#                 anapathc2 +rechute_post_allo +
#                 nbr_lignes_avt_alloc +
#                 donnor + stem_cell_source + sex_dp3+
#                 disease_status_at_transplantc + karnofsky_greffec3
#               , finalosgl),direction="both")
# 
# 
# decesincom<-summary(coxph(Surv(time=as.numeric(start), time2=as.numeric(stop), event=decesf) ~agvhd+
# rechute_post_allo +sex_dp3+karnofsky_greffec3, greffe_longdeces))
# 
# cont<-cox.zph(coxph(Surv(time=as.numeric(start), time2=as.numeric(stop), event=decesf) ~agvhd+
#         rechute_post_allo +sex_dp3+karnofsky_greffec3, greffe_longdeces))
# 
# plot(cont)
# 
# l<-lapply(list("agvhd","rechute_post_allo"
#                ,"sex_dp3","karnofsky_greffec3"),repli,data=greffe_longdeces)
# 
# l<-unlist(l)
# 
# osaic<-cox_multi(decesincom,l)

# 
# 
# 
# coxph(Surv(time=as.numeric(dates), time2=as.numeric(end), event=pfs60f) ~cgvhd, greffe_long2)
# 
# 
# test<-greffe[greffe$num_id %in% 1: 100,]
# 
# 
# st_60<-Surv(event=greffe$deces_60,time=as.numeric(greffe$delai_dc_60))
# coxph( st_60 ~ rechute_post_allo,data=greffe) 
# 
# 
# 
# summary(coxph(Surv(time=as.numeric(dates), time2=as.numeric(end), event=decesf) ~
#                 
#                 1 ,greffe_longdeces[greffe_longdeces$num_id %in% 1:100,]))
# greffe_longdeces$rechute_post_allo
# 
# 
# 
# 
# zu<-Surv(time=greffe_longdeces$dates, time2=greffe_longdeces$end, event=greffe_longdeces$decesf)
# coxph(zu ~rechute_post_allo+ cluster(num_id), greffe_longdeces)
