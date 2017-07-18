### survie dÃ©Â©pendante du temps : PFS  ### 

greffe<-greffe[order(greffe$num_id),]

greffe$j060t<-as.numeric(greffe$j0)
greffe$agvhd_date60tpfs<-ifelse(difftime(greffe$agvhd_date,greffe$j0)/30.25<60 & greffe$agvhd3==1 
                                & !is.na(greffe$agvhd3) & difftime(greffe$agvhd_date, greffe$date_rechute_progression)
 <0 ,greffe$agvhd_date,NA)-1
greffe$cgvhd_date60tpfs<-ifelse(difftime(greffe$cgvhd_date,greffe$j0)/30.25<60 & 
                                  difftime(greffe$cgvhd_date, greffe$date_rechute_progression)
                                                                <0,greffe$cgvhd_date,NA)-1
greffe$date_60tpfs<-ifelse(difftime(greffe$date_rechute_progression,greffe$j0)/30.25<60,greffe$date_rechute_progression,greffe$j0+(60*30.25))

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



### PFS ####
greffe$pfs_60trash0<-0
greffe$pfs_60trash1<-0
greffe$pfs_60trash2<-0
greffe$pfs_60trash3<-ifelse(greffe$pfs_60==1,1,0)



# ### non###
# greffe$efs_60trash0<-0
# greffe$efs_60trash1<-0
# greffe$efs_60trash2<-ifelse(greffe$efs_60==1,1,0)
# greffe$efs_60trash3<-ifelse(greffe$efs_60==1,1,0)




greffe_l<-reshape(greffe[,c("num_id","j060t","agvhd_date60tpfs","cgvhd_date60tpfs","date_60tpfs")], varying =
                    c("j060t","agvhd_date60tpfs","cgvhd_date60tpfs","date_60tpfs"), v.names="dates",
                  timevar="ref", times=c("j060t","agvhd_date60tpfs","date_60tpfs","date_fu60t"),
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





greffe_trashpfspfs<-cbind(greffe_l,greffe_l2,greffe_l3,greffe_l4,greffe_l5) 
greffe_trashpfspfs<-greffe_trashpfspfs[!is.na(greffe_trashpfspfs$dates),]
greffe_trashpfspfs$dates<-as.numeric(greffe_trashpfspfs$dates)

greffe_longpfs<-NULL 
for( i in unique(greffe_trashpfspfs$num_id)){
  
  data<-greffe_trashpfspfs[greffe_trashpfspfs$num_id==i,]
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
  #data$TT[1]<-"Chmioth"
  data$decesf[nrow(data)]<-NA
  data$pfs60f[nrow(data)]<-NA
  greffe_longpfs<-rbind(greffe_longpfs,data)
}


greffe_longpfs

greffe_longpfs$dt<-greffe_longpfs$end-greffe_longpfs$dates


### Calcul DéLAI : logique on ne travaille pas avec les dates et chaque patient n'est pas suivi de tel date Ã  telle date
###


vu<-as.numeric(table(greffe_longpfs$num_id))
b<-rep(greffe$j060t,vu)
greffe_longpfs$start<-greffe_longpfs$dates-b
greffe_longpfs$stop<-greffe_longpfs$end-b

greffe_longpfs2<-greffe_longpfs[!greffe_longpfs$dt==0,]
table(greffe_longpfs2$pfs60f,exclude=NULL)




### PFS 60j ###

decestpscg<-summary(coxph(Surv(time=as.numeric(start), time2=as.numeric(stop), event=pfs60f) ~cgvhd ,greffe_longpfs2))
decestpsag<-summary(coxph(Surv(time=as.numeric(start), time2=as.numeric(stop), event=pfs60f) ~agvhd+ cluster(num_id), greffe_longpfs2))


coxph(Surv(time=as.numeric(dates), time2=as.numeric(end), event=decesf) ~cgvhd, greffe_long2)

greffe_longpfsan<-left_join(greffe_longpfs2[,c("num_id", "ref", "dates",  "ref2", "deces", 
                                            "refa", "agvhd", "refc", "cgvhd", 
                                            "id", "num_id", "refd", "pfs", "end", "decesf", "pfs60f", 
                                            "dt","start","stop")], greffe[,
                                                                          c("num_id","anapathc2", "disease_status_at_transplantc",
                                                                            "karnofsky_greffec3" ,
                                                                            "rechute_post_allo",
                                                                            "nbr_lignes_avt_alloc",
                                                                            "donnor", "stem_cell_source","sex_dp3","depletion","delai_dia_alloc")], by="num_id")

greffe_longpfsan<-greffe_longpfsan[order(greffe_longpfsan$num_id),]



greffe_longpfsan$agvhd<-as.factor(greffe_longpfsan$agvhd)
greffe_longpfsan$cgvhd<-as.factor(greffe_longpfsan$cgvhd)



# summary(coxph(Surv(time=as.numeric(dt), event=decesf) ~
#                             
#                 rechute_post_allo+ cluster(num_id),greffe_longdeces))


### Test on doit avoir les mêmes résultats ### 
coxph(Surv(time=as.numeric(start), time2=as.numeric(stop), event=pfs60f) ~rechute_post_allo, greffe_longpfsan)
coxph(Surv(time=as.numeric(start), time2=as.numeric(stop), event=pfs60f) ~sex_dp3,greffe_longpfsan)



### prendre kar 3 catégories et anapathc2 ###

pfsstt<-summary(coxph(Surv(time=as.numeric(start), time2=as.numeric(stop), event=pfs60f) ~
                            
                            anapathc2 +
                        delai_dia_alloc+
                            nbr_lignes_avt_alloc +
                            donnor + stem_cell_source +
                            disease_status_at_transplantc + karnofsky_greffec3
                          , greffe_longpfsan))

cox.zph(coxph(Surv(time=as.numeric(start), time2=as.numeric(stop), event=pfs60f) ~
                
                anapathc2 +
                delai_dia_alloc+
                nbr_lignes_avt_alloc +
                donnor + stem_cell_source +
                disease_status_at_transplantc + karnofsky_greffec3
              , greffe_longpfsan))


# summary(coxph(Surv(time=as.numeric(dt), event=decesf) ~
#                             
#                 rechute_post_allo+ cluster(num_id),greffe_longdeces))


#

finalpfsgl<-greffe_longpfsan[complete.cases
                             (greffe_longpfsan
                             [,c("anapathc2","nbr_lignes_avt_alloc","donnor",
                                 "stem_cell_source"
,"disease_status_at_transplantc","delai_dia_alloc","karnofsky_greffec3")]),]        

stepAIC(coxph(Surv(time=as.numeric(start), time2=as.numeric(stop), event=pfs60f) ~
                
                anapathc2 +
                delai_dia_alloc+
                nbr_lignes_avt_alloc +
                donnor + stem_cell_source +
                disease_status_at_transplantc + karnofsky_greffec3
              , finalpfsgl),direction="both")


pfsincom<-summary(coxph(Surv(time=as.numeric(start), time2=as.numeric(stop), event=pfs60f) ~
                          nbr_lignes_avt_alloc+karnofsky_greffec3+ stem_cell_source, greffe_longpfsan))

cont<-cox.zph(coxph(Surv(time=as.numeric(start), time2=as.numeric(stop), event=pfs60f) ~
                      nbr_lignes_avt_alloc+karnofsky_greffec3+ stem_cell_source, greffe_longpfsan))

plot(cont)

l<-lapply(list("nbr_lignes_avt_alloc","karnofsky_greffec3","stem_cell_source"),repli,data=greffe_longpfsan)

l<-unlist(l)

pfsaic<-cox_multi(pfsincom,l)

pfsaic<-cbind(c("N of lines","Karnofsky score","","Disease status at transplant",""),pfsaic)





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
