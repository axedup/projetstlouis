### survie dé©pendante du temps : OS  ### 

greffe<-greffe[order(greffe$num_id),]

greffe$j060t<-as.numeric(greffe$j0)
greffe$agvhd_date60t<-ifelse(difftime(greffe$agvhd_date,greffe$j0)/30.25<60 & greffe$agvhd3==1 & !is.na(greffe$agvhd3),greffe$agvhd_date,NA)-1
greffe$cgvhd_date60t<-ifelse(difftime(greffe$cgvhd_date,greffe$j0)/30.25<60,greffe$cgvhd_date,NA)-1
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
  #data$TT[1]<-"ChmiothÃÂÃÂ©rapie non dÃÂÃÂ©butÃÂÃÂ©e"
  data$decesf[nrow(data)]<-NA
  data$pfs60f[nrow(data)]<-NA
  greffe_long<-rbind(greffe_long,data)
}


greffe_long

greffe_long$dt<-greffe_long$end-greffe_long$dates

greffe_long2<-greffe_long[!greffe_long$dt==0,]
table(greffe_long2$decesf,exclude=NULL)

### Calcul DéLAI : logique on ne travaille pas avec les dates et chaque patient n'est pas suivi de tel date à telle date
###


a<-table(greffe_long2$num_id)
b<-rep(greffe$j060t,a)
greffe_long2$start<-greffe_long2$dates-b
greffe_long2$stop<-greffe_long2$end-b

### Deces 60j ###

decestpscg<-summary(coxph(Surv(time=as.numeric(start), time2=as.numeric(stop), event=decesf) ~cgvhd ,greffe_long2))
decestpsag<-summary(coxph(Surv(time=as.numeric(start), time2=as.numeric(stop), event=decesf) ~agvhd+ cluster(num_id), greffe_long2))


coxph(Surv(time=as.numeric(dates), time2=as.numeric(end), event=decesf) ~cgvhd, greffe_long2)

greffe_longdeces<-left_join(greffe_long2[,c("num_id", "ref", "dates",  "ref2", "deces", 
                                            "refa", "agvhd", "refc", "cgvhd", 
                                            "id", "num_id", "refd", "pfs", "end", "decesf", "pfs60f", 
                                            "dt","start","stop")], greffe[,
                                                                          c("num_id","anapathc2", "disease_status_at_transplantc",
                                                                            "karnofsky_greffec3" ,
                                                                            "rechute_post_allo",
                                                                            "nbr_lignes_avt_alloc",
                                                                            "donnor", "stem_cell_source","sex_dp3","delai_dia_alloc")], by="num_id")

greffe_longdeces<-greffe_longdeces[order(greffe_longdeces$num_id),]

head(greffe_longdeces)






# summary(coxph(Surv(time=as.numeric(dt), event=decesf) ~
#                             
#                 rechute_post_allo+ cluster(num_id),greffe_longdeces))


### Test on doit avoir les mêmes résultats ### 
coxph(Surv(time=as.numeric(start), time2=as.numeric(stop), event=decesf) ~rechute_post_allo, greffe_longdeces)
coxph(Surv(time=as.numeric(start), time2=as.numeric(stop), event=decesf) ~sex_dp3, greffe_longdeces)


# decestpstt<-summary(coxph(Surv(time=as.numeric(start), time2=as.numeric(stop), event=decesf) ~agvhd+
#                             
#                             anapathc2 +
#                             rechute_post_allo +
#                             nbr_lignes_avt_alloc +
#                             donnor + stem_cell_source + sex_dp3+
#                             disease_status_at_transplantc + karnofsky_greffec3
#                    , greffe_longdeces))
greffe_longdeces$agvhd<-as.factor(greffe_longdeces$agvhd)
levels(greffe_longdeces$agvhd)<-c("No Agvhd or grade 1-2","Grade 3-4 Agvhd")



decestpstt2<-summary(coxph(Surv(time=as.numeric(start), time2=as.numeric(stop), event=decesf) ~agvhd+
                             
                             
                             rechute_post_allo +
                             strata(delai_dia_alloc)+
                             sex_dp3+
                             disease_status_at_transplantc + karnofsky_greffec3
                           , greffe_longdeces))




l<-lapply(list("agvhd","rechute_post_allo","sex_dp3","disease_status_at_transplantc","karnofsky_greffec3"),repli,data=greffe_longdeces)

l<-unlist(l)

osaic<-cox_multi(decestpstt2,l)
#osaic<-data.frame(osaic)

osaic<-cbind(c("Agvhd","First graft relapse","","Sex of donnor-patient","Disease status at transplant","","Karnofsky score",""),osaic)
osaic

merde<-cox.zph((coxph(Surv(time=as.numeric(start), time2=as.numeric(stop), event=decesf) ~agvhd+
                        
                        
                        rechute_post_allo +
                        delai_dia_alloc+
                        sex_dp3+
                        disease_status_at_transplantc + karnofsky_greffec3
                      , greffe_longdeces)))


plot(merde)        

merde<-cox.zph((coxph(Surv(time=as.numeric(start), time2=as.numeric(stop), event=decesf) ~agvhd+
                        
                        
                        rechute_post_allo +
                        strata(delai_dia_alloc)+
                        sex_dp3+
                        disease_status_at_transplantc + karnofsky_greffec3
                      , greffe_longdeces)),transform = "log")



plot(merde)    

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
greffe_longdeces$delai_dia_alloc

facile<-unique(greffe_longdeces$num_id)[table(greffe_longdeces$num_id)==1]

PE<-NULL
for (i in facile){
  
  pe<-greffe_longdeces[greffe_longdeces$num_id==i,]
  
  if (pe$stop<=200){
    pe$delai_dia_alloca<- pe$delai_dia_alloc
  pe$delai_dia_allocb<- 0
  }
  
  
  if (pe$stop>200){
    
    pe<-rbind(pe,pe)
    pe$stop[1]<-200
    pe$start[2]<-200
    pe$delai_dia_alloca[1]<- pe$delai_dia_alloc
    pe$delai_dia_alloca[2]<- 0
    pe$delai_dia_allocb[1]<- 0
    pe$delai_dia_alloca[2]<- pe$delai_dia_alloc
    
    
  }
  PE<-rbind(PE,pe)
  
}