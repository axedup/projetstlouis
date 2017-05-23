fit1 <- coxph(Surv(time, status) ~ karno + age + trt, veteran)

# a cox.zph plot of the data suggests that the effect of Karnofsky score
#  has faded by 120 days-- plot(cox.zph(fit)[1]) -- and begins to
#  diminish by 60 days.
# Fit a model with separate coefficeints for the three intervals
#
vet2 <- survSplit(Surv(time, status) ~., veteran,
                  cut=c(30), episode ="timegroup")
fit2 <- coxph(Surv(tstart, time, status) ~ karno* strata(timegroup) +
                age + trt, data= vet2)
c(overall= coef(fit1)[1],
  t0_60  = coef(fit2)[1],
  t60_120= sum(coef(fit2)[c(1,4)]),
  t120   = sum(coef(fit2)[c(1,5)]))


greffe$delai_dcn<-as.numeric(greffe$delai_dc)
b<-Surv(time=greffe$delai_dcn,event=greffe$deces)
vet3 <- survSplit(Surv(delai_dcn,deces) ~., data=greffe,
                  cut=c(30,60), episode ="timegroup",end="delai_dcn")


fit3 <- coxph(Surv(tstart, delai_dcn, deces) ~ hla_matchc , data= vet3)
plot(survfit(Surv(tstart, delai_dcn, deces) ~hla_matchc,data=vet3),fun="cloglog" ,lty=1:4, col=2:5)

vet3$hlatps<-sqrt(vet3$delai_dcn)*as.numeric(vet3$hla_matchc)


coxt<-coxph(Surv(tstart, delai_dcn, deces) ~ as.numeric(hla_matchc):tstart, data= vet3)

summary(coxt)
cox.zph(coxt)
plot(survfit(Surv(tstart, delai_dcn, deces) ~as.numeric(hla_matchc):tstart,data=vet3),fun="cloglog" ,lty=1:4, col=2:5)



coxtt<-coxph(Surv(tstart, delai_dcn, deces) ~ as.numeric(karnofsky_greffec):tstart, data= vet3)

summary(coxtt)
cox.zph(coxtt)
plot(cox.zph(coxtt))


####○

coxtt<-coxph(Surv(delai_dc_48, deces_48) ~ cgvhd+as.numeric(cgvhd):delai_dc_48, data= greffe)


summary(coxtt)
cox.zph(coxtt)
plot(cox.zph(coxtt)[1])


### 
coxts<-coxph(Surv(delai_dc_48, deces_48) ~ disease_status_at_transplantc+stem_cell_source+nbr_lignes_avt_alloc2+karnofsky_greffec3 +strata(cgvhd), data= greffe)
summary(coxts)


stepAIC(coxts, scale = 0,direction = c("both"),
        trace = 1, keep = NULL, steps = 1000, use.start = FALSE,
        k = 2)