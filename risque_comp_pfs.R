### survie risques compétitifs  dépendant du temps : PFS  ### 

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
greffe$rechute_progression_dc60trash0<-0
greffe$rechute_progression_dc60trash1<-0
greffe$rechute_progression_dc60trash2<-0
greffe$rechute_progression_dc60trash3<-ifelse(greffe$rechute_progression_dc60==0,0,greffe$rechute_progression_dc60)



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


greffe_l5<-reshape(greffe[,c("num_id","rechute_progression_dc60trash0","rechute_progression_dc60trash1","rechute_progression_dc60trash2","rechute_progression_dc60trash3")], varying =
                     c("rechute_progression_dc60trash0","rechute_progression_dc60trash1","rechute_progression_dc60trash2","rechute_progression_dc60trash3"), v.names="pfs",
                   timevar="refd", times=c("rechute_progression_dc60trash0","rechute_progression_dc60trash1","rechute_progression_dc60trash2","rechute_progression_dc60trash3"),
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

greffe_longpfs2$help<-greffe_longpfs2[,1]

### Rechute avec risques compétitifs et cgvh chronique agvhd : on va travailler ###


crr()
greffe_longpfs2test<-greffe_longpfs2[,c("start","stop","pfs60f","help","cgvhd","agvhd")]

greffe_longpfs2test$pfs60f<-as.factor(greffe_longpfs2test$pfs60f)

greffe_longpfs2test<-finegray(Surv(time=as.numeric(start), time2=as.numeric(stop), event=pfs60f)~.,id=help, data=greffe_longpfs2test)



greffematchit$rechute_progression_dc60f<-as.factor(greffematchit$rechute_progression_dc60)
greffe_longpfs2testcontrol<-finegray(Surv(time=delai_pfs_60, event=rechute_progression_dc60f)~., data=greffematchit)




greffe_longpfsancom<-left_join(greffe_longpfs2test, greffe[,   c("num_id","anapathc2", "disease_status_at_transplantc",
                                                                               "karnofsky_greffec3" ,
                                                                               "rechute_post_allo",
                                                                               "nbr_lignes_avt_alloc",
                                                                               "donnor", "stem_cell_source","sex_dp3","depletion","delai_dia_alloc")], by=c("help"="num_id"))
poids<-greffe_longpfsancom$fgwt



### vérifications  ###

summary(coxph(Surv(time=as.numeric(fgstart), time2=as.numeric(fgstop), event=fgstatus)~sex_dp3,weights = fgwt,data=greffe_longpfsancom))
coxph(Surv(delefs_ttt, efs_ttt)~ttt_simpl+cluster(id),weights=wa, baz)



greffe$cov1<-ifelse(greffe$sex_dp3=="F/M",1,0)

a<-as.matrix(greffe$cov1)
summary(crr(ftime=greffe$delai_pfs_60,fstatus=greffe$rechute_progression_dc60,cencode=0,cov1=a))
