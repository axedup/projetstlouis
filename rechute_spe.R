### rechute en cause spécifique ###

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
greffe$pfs_60trash3<-ifelse(greffe$rechute_progression_60==1,1,0)



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
                                                                               "donnor", "stem_cell_source","sex_dp3","depletion","delai_dia_alloc","intensite_condi3")], by="num_id")

greffe_longpfsan<-greffe_longpfsan[order(greffe_longpfsan$num_id),]



greffe_longpfsan$agvhd<-as.factor(greffe_longpfsan$agvhd)
greffe_longpfsan$cgvhd<-as.factor(greffe_longpfsan$cgvhd)



# summary(coxph(Surv(time=as.numeric(dt), event=decesf) ~
#                             
#                 rechute_post_allo+ cluster(num_id),greffe_longdeces))


### Test on doit avoir les mêmes résultats ### 
coxph(Surv(time=as.numeric(start), time2=as.numeric(stop), event=pfs60f) ~rechute_post_allo, greffe_longpfsan)
coxph(Surv(time=delai_pfs_60, event=rechute_progression_60) ~rechute_post_allo, greffe)

coxph(Surv(time=as.numeric(start), time2=as.numeric(stop), event=pfs60f) ~cgvhd,greffe_longpfsan)



### prendre kar 3 catégories et anapathc2 ###

pfsstt<-summary(coxph(Surv(time=as.numeric(start), time2=as.numeric(stop), event=pfs60f) ~
                        
                        anapathc2 +
                        delai_dia_alloc+
                        nbr_lignes_avt_alloc +
                        donnor + stem_cell_source +
                        disease_status_at_transplantc + karnofsky_greffec3
                      , greffe_longpfsan))

cox.zph(coxph(Surv(time=as.numeric(start), time2=as.numeric(stop), event=pfs60f) ~
                agvhd+
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
                               [,c("agvhd","nbr_lignes_avt_alloc","donnor",
                                   "stem_cell_source"
                                   ,"disease_status_at_transplantc","delai_dia_alloc","karnofsky_greffec3","depletion","cgvhd")]),]        


stepAIC(coxph(Surv(time=as.numeric(start), time2=as.numeric(stop), event=pfs60f) ~
                agvhd+
                anapathc2 +
                delai_dia_alloc+
                nbr_lignes_avt_alloc +
                donnor + stem_cell_source +
                disease_status_at_transplantc + karnofsky_greffec3 +cgvhd
              , finalpfsgl),direction="both")

pfsincom<-summary(coxph(Surv(time=as.numeric(start), time2=as.numeric(stop), event=pfs60f) ~
                          nbr_lignes_avt_alloc+karnofsky_greffec3+ stem_cell_source +cgvhd+agvhd , greffe_longpfsan))

cont<-cox.zph(coxph(Surv(time=as.numeric(start), time2=as.numeric(stop), event=pfs60f) ~
                      nbr_lignes_avt_alloc+karnofsky_greffec3+ stem_cell_source+cgvhd +agvhd, greffe_longpfsan))

plot(cont)

l<-lapply(list("nbr_lignes_avt_alloc","karnofsky_greffec3","stem_cell_source","cgvhd","agvhd"),repli,data=greffe_longpfsan)

l<-unlist(l)

pfsaic<-cox_multi(pfsincom,l)

pfsaic<-cbind(c("N of lines","Karnofsky score","","Cell source","","cgvhd","","agvhd",""),pfsaic)

### Test interaction ###


av<-greffe$num_id[greffe$deces==1 & greffe$delai_dc<=3]
coxph(Surv(time=as.numeric(start), time2=as.numeric(stop), event=pfs60f) ~
        cgvhd*intensite_condi3,greffe_longpfsan[!greffe_longpfsan$num_id %in% av,])

# strata car en fait de travailler en sous population cela revient à calculer une fct
#de risque de base 

## sous groupe RIC
summary(coxph(Surv(time=as.numeric(start), time2=as.numeric(stop), event=pfs60f) ~cgvhd,
              greffe_longpfsan[greffe_longpfsan$intensite_condi3=="RIC" & !greffe_longpfsan$num_id %in% av,]))

## sous groupe MAC


mac<-greffe_longpfsan[greffe_longpfsan$intensite_condi3=="MAC",]
summary(coxph(Surv(time=as.numeric(start), time2=as.numeric(stop), event=pfs60f) ~cgvhd,
              mac[!mac$num_id %in% av,]))

# coxph( pfss_60 ~ sex_dp3 + intensite_condi3,data=greffe) 
# 
# 
# 
# mac<-greffe[greffe$intensite_condi3=="MAC",]
# coxph(Surv(event=mac$pfs_60,time=as.numeric(mac$delai_pfs_60))~mac$sex_dp3)
# 
# glm(mac$pfs_60~mac$sex_dp3,data=mac,family="binomial")
# glm(greffe$pfs_60~greffe$sex_dp3 * greffe$intensite_condi3,data=greffe,family="binomial")
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



library(KMsurv)
data(bmt)

########### transform the data set bmt in counting process form 
##########  for the time-dependent covariate GvHD ('da'), in a competing risks framework
# new variables: ID, Start, Stop, Status, TimeDep
n= dim(bmt)[1]
bmt$ID=seq(1:n)
bmt$Start= rep(0,n)
bmt$Stop= bmt[,"t2"]
bmt$Status = bmt$TimeDep = rep(0,n) 

# assign the right values to the new variables
temp1 =bmt[bmt$da==1,]
temp1$Start=temp1$ta
bmt[bmt$da==1,]$Stop = bmt[bmt$da==1,]$ta
temp1$TimeDep = 1
# 'Status' is a status variable with value 2: relapse and 3:death
bmt[(bmt$da==0)&(bmt$d1==1),]$Status = 3
bmt[(bmt$da==0)&(bmt$d2==1),]$Status = 2
temp1[temp1$d1==1,]$Status =3
temp1[temp1$d2==1,]$Status =2

# bind together 'bmt' and the additional data set 'temp1' for individuals with 'da'==1
bmtCP=rbind(bmt,temp1)
ii=order(bmtCP$ID,bmtCP$da)
bmtCP=bmtCP[ii,]    # bmtCP: final data set in counting process form

# add columns 'From' (=1 for all subjects) and 'To' to the data set 'bmtCP'
bmtCP$To = bmtCP$From = rep(1,dim(bmtCP)[1])
bmtCP[bmtCP$da==0,]$To = bmtCP[bmtCP$da==0,]$Status
bmtCP[((bmtCP$da==1)&(bmtCP$Start!=0)),]$To = bmtCP[((bmtCP$da==1)&(bmtCP$Start!=0)),]$Status

########## transform the data set 'bmtCP' in long form with the new intermediate state 4
new = newdata_TD4(from="From", to="To", start="Start", stop="Stop", Td="TimeDep", ID="ID", 
                  data=bmtCP, causes=c(2,3), newstate=4, cen =0)

######### estimate cumulative incidence e transition probabilities
# names for final states (events) and intermediate state can be any number, for semplicity  
transP = TP_mstate(from="From", to="To", start="Start", stop="Stop", status="status", tr="tr", data=new, 
                   causes=c(2,3), time.s = 0, interstate=4, new.data = NULL, model = NULL)
# cumulative incidence function for cause 2:relapse and 3:death (transition prob. from initial state 1 to final state 2 or final state 3)
transP$P[1,2,]   # i=1, j=2
transP$P[1,3,]   # i=1, j=3
plot(transP$time, transP$P[1,3,],type="s")
lines(transP$time, transP$P[1,2,],type="s",lty=2)

# transition probabilities from the initial state 1 to the intermediate state 4-GvHD (always in the last column)
transP$P[1,4,]   # i=1, j=4
plot(transP$time, transP$P[1,4,],type="s")

# transition probabilities from the intermediate state 4-GvHD to the finals events 2-relapse or 3-death
transP$P[4,2,]   # i=4, j=2
transP$P[4,3,]   # i=4, j=3
plot(transP$time, transP$P[4,3,],type="s")
lines(transP$time, transP$P[4,2,],type="s",lty=2)

# probability of surviving without relapse (2) and without GvHD (4) 
transP$P[1,1,]   # i=1, j=1
plot(transP$time, transP$P[1,1,],type="s")

# probability that the event 2:relapse (or the event 3:death) have occurred within the time interval (s,t]=(0,526],
# given that the patient is alive and without GvHD at time s=0.
ii=which(transP$time==526)     #find the index in the matrix for which  t=526 
transP$P[1,2,ii]    
transP$P[1,3,ii]    

######### add transition-specific covariates for covariates 'age' and 'group'
new$age.c = new$z1 - mean(new$z1)
new$group = as.factor(new$group)
new1 = CScovariates(from = "From", tr="tr", causes=c(2,3), interstate=4, data=new, type = "mstate", cov=c("age.c","group"))


######## predict cumulative incidence and transition probabilities, by Cox regression models for the transition-specific hazards
new1$cov.names
# Cox model with trnsitions as strata
model1 <- coxph(Surv(Start,Stop,status==1) ~ age.c12 + age.c13 + age.c14 + 
                  age.c42 + age.c43 + group12 + group13 + group14 + group42 + group43 + strata(tr), data=new1$newdata)
summary(model1)

# predictions of transition probabilities for a patient with age =50 (age.c=21.21382) in group AML (2),
# given that the patient is alive without GvHD at time s=100.
TP1 = TP_mstate(from="From", to="To", start="Start", stop="Stop", status="status", tr="tr", data=new1$newdata, 
                causes=c(2,3), time.s = 100, interstate=4, new.data = c(21.21382,2), model = model1)
# cumulative incidence probability for cause 2:relapse and 3:death, 
# conditional on being alive without GvHD at time s=100
# (transition prob. from initial state 1 at time s to final state 2 or final state 3).
TP1$P[1,2,]   # i=1, j=2
TP1$P[1,3,]   # i=1, j=3
plot(TP1$time, TP1$P[1,3,],type="s")
lines(TP1$time, TP1$P[1,2,],type="s",lty=2)


