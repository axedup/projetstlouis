### survie dépendante du temps ### 

greffe<-greffe[order(greffe$num_id),]

greffe$agvhd_trash0<-0
greffe$agvhd_trash1<-ifelse(!is.na(greffe$agvhd_date),1,0)
greffe$agvhd_trash2<-ifelse(!is.na(greffe$agvhd_date),1,0)
greffe$agvhd_trash3<-ifelse(!is.na(greffe$agvhd_date),1,0)

greffe$cgvhd_trash0<-0
greffe$cgvhd_trash1<-0
greffe$cgvhd_trash2<-ifelse(!is.na(greffe$cgvhd_date),1,0)
greffe$cgvhd_trash3<-ifelse(!is.na(greffe$cgvhd_date),1,0)


greffe$deces_60trash0<-0
greffe$deces_60trash1<-0
greffe$deces_60trash2<-ifelse(greffe$deces_60==1,1,0)
greffe$deces_60trash3<-ifelse(greffe$deces_60==1,1,0)



greffe$pfs_60trash0<-0
greffe$pfs_60trash1<-0
greffe$pfs_60trash2<-ifelse(greffe$pfs_60==1,1,0)
greffe$pfs_60trash3<-ifelse(greffe$pfs_60==1,1,0)

### non###
greffe$efs_60trash0<-0
greffe$efs_60trash1<-0
greffe$efs_60trash2<-ifelse(greffe$efs_60==1,1,0)
greffe$efs_60trash3<-ifelse(greffe$efs_60==1,1,0)




greffe_l<-reshape(greffe[,c("num_id","j0","agvhd_date","cgvhd_date","date_fu")], varying =
             c("j0","agvhd_date","cgvhd_date","date_fu"), v.names="dates",
           timevar="ref", times=c("j0","agvhd_date","cgvhd_date","date_fu"),
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
  for (j in 1: nrow(data)-1){
    data$end[j]<-data$dates[j+1]
    
  }
  
  data$end[nrow(data)]<-data$dates[nrow(data)]
  #data$end<-as.Date(data$end,origin="1970-01-01")
  #data$TT[1]<-"ChmiothÃ©rapie non dÃ©butÃ©e"
  #data$TT[nrow(data)]<-NA
  greffe_long<-rbind(greffe_long,data)
}


greffe_long





coxph(Surv(time=as.numeric(datedebut), time2=as.numeric(datefin), event=TT) ~sex +strata(strates) + cluster(nip), DATA1)


