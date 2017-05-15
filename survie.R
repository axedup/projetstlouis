
### Survie ###

### OS 
s<-Surv(event=greffe$deces,time=greffe$delai_dc)

a <- survfit( s ~ 1)
plot(a)

a <- survfit( s ~ anapathc,data=greffe)
plot(a)


z<-coxph( s ~ sex_patient,data=greffe)

u<-summary(z)

### EFS (rechute, progression, deces)
efs<-Surv(event=greffe$rechute_progressionc,time=greffe$delai_efs)

e <- survfit( efs ~ 1)
plot(e)


### GVHD

gvhd<-Surv(event=greffe$gvhd,time=greffe$delai_gvhd)

e <- survfit( gvhd ~ 1)
plot(e)
