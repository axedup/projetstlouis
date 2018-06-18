### pb avec les dates ###


berger$date_symjm<-format(as.Date(berger$date_sym), "%d-%m")
table(berger$date_symjm)


TITI<-NULL
for(i in 
    c( "date_sym", "ddn",
       "date_diag",  "date_colchi", 
       "date_fin_colchi", "date_asp", "date_fin_asp", 
       "date_pla", "date_fin_pla",  "date_coag", "date_fin_coag", 
       "date_statin", "date_fin_statin", "date_inh", 
       "date_fin_inh",  "date_vas", "date_fin_vas",  "date_iec", 
       "date_fin_iec", "date_amput1",  
       "date_amput2",  "date_amput3",  "date_evt1",  "date_evt2","date_evt3", 
       "date_evt4",  
       "date_evt5",  "date_evt6", "date_evt7", "date_gv1", 
       "date_gv2", "date_gv3", "date_gv4", 
       "date_gv5",
       "date_fu")){
  tu<-paste0(i,"jm")
  berger[,tu]<-format(as.Date(berger[,i]), "%d-%m")
  titi<-prop.table(table(berger[,tu]))
  TITI<-c(TITI,titi)
}


cbind(c( "date_sym", 
         "date_diag",  "date_colchi", 
         "date_fin_colchi", "date_asp", "date_fin_asp", 
         "date_pla", "date_fin_pla",  "date_coag", "date_fin_coag", 
         "date_statin", "date_fin_statin", "date_inh", 
         "date_fin_inh",  "date_vas", "date_fin_vas",  "date_iec", 
         "date_fin_iec", "date_amput1",  
         "date_amput2",  "date_amput3",  "date_evt1",  "date_evt2","date_evt3", 
         "date_evt4",  
         "date_evt5",  "date_evt6", "date_evt7", "date_gv1", 
         "date_gv2", "date_gv3", "date_gv4", 
         "date_gv5",
         "date_fu"),TITI)



uu<-berger[berger$date_symjm=="01-07",c("date_sym","date_diag")]
table(difftime(uu$date_sym,uu$date_diag,units="days"))
