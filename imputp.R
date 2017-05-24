ini <- mice(greffe, max=0, print=F)
pred <- ini$predictorMatrix
pred[,"REMARQUE"] <- 0
pred[,"num_id"] <- 0
pred[,"j0"] <- 0
pred[,"first_name"] <- 0
pred[,"family_name"] <- 0
pred[,"sex_patient."] <- 0
pred[,"tbi_dose"] <- 0
pred[,"date_greffe_prece"] <- 0
pred[,"date_dia_l"] <- 0
pred[,"delai_allo_greffe_prece"]<-0
pred[,"tbi_dose"]<-0

pred[,"T.cell.depletion.with.MoAB..0.non..1..partial.T.depletion.."] <- 0
pred[,"immunosupression.détails"] <- 0
pred[,"stem_cell_.source"] <- 0
pred[,"immunosupression.détails"] <- 0
pred[,"Total.infused.nucleated.cells..donneur.1.."] <- 0
pred[,"Number.of.infused.CD34.positive.cells...donneur.1.."] <- 0
pred[,"Number.of.infused.CD3.positive.cells..T.cells....donneur.1..x.10E6.kg"] <- 0
pred[,"Total.infused.nucleated.cells..donneur.2.."] <- 0
pred[,"Number.of.infused.CD34.positive.cells...donneur..."] <- 0
pred[,"Number.of.infused.CD3.positive.cells..T.cells....donneur.2..x.10E6.kg"] <- 0
pred[,"date_rechute."] <- 0
pred[,"best_response_after_allo."] <- 0
pred[,"j0."] <- 0
pred[,"remarque.concernat.les.chimio.avant.allo"] <- 0
pred[,"previous_auto.1"] <- 0
pred[,"j0."] <- 0
pred[,c("X1ERE.LIGNE.ttt..détails.",                                            
        "X2.e.ligne..détails." ,                                                
        "X3.e.ligne..détails.",                                                 
        "X4.e.ligne..détails."  ,                                               
        "X5.e.ligne..détails.")]<-0


pred[,c("Neutrophils....0.5.x.10.9.L.reached..i..ANC.recovery...i...",
  "Date.neutrophils....0.5.engraftment.date" ,                            
  "Date.platelets.....b.50..b..as.yyyy.mm.dd" ,                           
  "Interval.to.neutrophil.recovery..i..engraftment...i...days.." ,"chimérisme"    ,                                                       
  "Date.of.chimaerism..as.yyyy.mm.dd","agvhd_date.","agvhd_grade." ,                                                          
  "delai_agvhd_allo",                                                     
  "aGvHD.stage.in.skin.",                                                 
  "aGvHD.stage.in.liver.",                                                
  "aGvHD.stage.in.gut.",                                                  
  "aGvHD.resolution.",                                                    
  "Date.of.resolution.of.aGvHD.",                                         
  "cgvhd_date.",  "cgvhd.",                                                        
  "J.de.l.évaluation.du.chimérisme", 
  "delai_cgvhd_allo",
  "delai_allo_fu",                                                        
  "date_fu." ,
  "ddn_donor",
  "Secondary.malignancy...clonal.complication.",                          
  "relapse_progression_transplant_2.",                                    
  "Additional..treatment..includes.cell.therapy..",                       
  "ttt.a.la.rechute...en.détail.",                                        
  "date_rechutep",                                                        
  "date_fu",                                                              
  "agvhd_date",                                                           
  "cgvhd_date" ,
  "agvhd",
  "agvhd3",                                                               
  "cgvhd",
  "gvhd",
  "survival_status_FU.",
  "cgvhd_grade",
  "cgvhd.",
  "prise_greffe1",
  "status_FU.",                                                           
  "survival_status_FU.",
  "anapath",
  "anapathc",                                                             
  "anapathc2",                                                            
  "disease_status_at_transplant",                              
  "disease_status_at_transplantc2",                                       
  
  "best_response_after_allo.",                                             
  "hla_matchc",                                                           
  "donnor",                                                               
  "cause_death.",                                                          
  "condit_details" ,
  "cause_death_c2",                                                       
  "delai_dia_alloc" ,                                                     
  "nbr_lignes_avt_alloc" ,                                                
  "nbr_lignes_avt_alloc2",                                               
  "programme_autoallo"     ,                                             
  "previous_auto",                                                       
  "rechute_prem_greffe"  ,                                               
  "rechute_post_allo" ,                                                   
  
  
  "karnofsky_greffec2"  ,                                                 
  "karnofsky_greffec3"   ,                                                
  "stade_diac"  ,                                                         
  "sex_dp2"   ,                                                           
  "sex_dp3"   ,                                                           
  "cmv_dp2"    ,                                                          
  "rechute"   ,                                                           
  "rechute_progression"   ,                                               
  "relapse_progression_transplant_c"  ,                                   
  "rechute_progressionc" ,                                                 
  "rechute_progression_dc" ,                                              
  "rechute_dc" ,                                                          
  "g_p",                                                                  
  "rechute_progression_ghvd_dc" ,                                         
  "efs_statut"  ,                                                         
  "delai_dc",                                                             
  "delai_gvhd" ,                                                          
  "date_rechute" ,                                                        
  "date_rechute_progression" ,                                            
  "date_rechute_progression_gvhd" ,                                       
  "delai_pfs" ,                                                           
  "delai_rechute"  ,                                                      
  "delai_rechutepg" )] <- 0

meth <- ini$meth
meth["gvhd"] <- "~I(ifelse(agvhd==1| cgvhd==1,1,0))" # non imputÃ© mais calculÃ©
meth["cgvhd"] <- "~I(ifelse(cgvhd. %in% c(deces avant J100,no),0,1))" # non imputÃ© mais calculÃ©
meth["agvhd3"] <- "~I(as.factor(ifelse(agvhd_grade %in% c('Grade III', 'Grade IV'),1,0)))" # non imputÃ© mais calculÃ©
meth["agvhd"] <-"~I(as.factor(ifelse(greffe$agvhd_grade %in% c('No aGvHD present (Grade 0)'),0,1)))" # non imputÃ© mais calculÃ©
meth["agvhd_date"] <-"" # non imputÃ© mais calculÃ©
meth["cgvhd_date"] <- "" # non imputÃ© mais calculÃ©
meth["date_rechutep"] <- "" 
meth["cause_death."] <- "" 
meth["status_FU."] <- "" 
#meth["status_FU"] <- "" 
meth["survival_status_FU."] <- "" 
meth["survival_status_FU"] <- "" 


meth["date_fu." ] <- "" 
meth["ddn_donor"] <- "" 
meth["Secondary.malignancy...clonal.complication."] <- ""                           
meth["relapse_progression_transplant_2."] <- ""                                     
 meth["Additional..treatment..includes.cell.therapy.."] <- ""                        
 meth["ttt.a.la.rechute...en.détail."] <- ""    
 meth["delai_allo_fu"] <- ""    
 meth["cgvhd_grade"] <- "" 
 meth["cgvhd."] <- "" 
 meth["delai_cgvhd_allo"] <- "" 
 meth["cgvhd_date."] <- "" 
 
 meth[c("delai_agvhd_allo", "aGvHD.stage.in.skin.")]<-c(rep("",2))                                                     
 meth[c("aGvHD.stage.in.skin.",                                                 
 "aGvHD.stage.in.liver.",                                                
 "aGvHD.stage.in.gut.",                                                  
 "aGvHD.resolution.",                                                    
 "Date.of.resolution.of.aGvHD.",
 "agvhd_date.",
 "delai_cgvhd_allo",
 "agvhd_grade.",
 "J.de.l.évaluation.du.chimérisme",
 "Neutrophils....0.5.x.10.9.L.reached..i..ANC.recovery...i...",
 "Date.neutrophils....0.5.engraftment.date" ,                            
 "Date.platelets.....b.50..b..as.yyyy.mm.dd" ,                           
 "Interval.to.neutrophil.recovery..i..engraftment...i...days.." ,"chimérisme"    ,                                                       
 "Date.of.chimaerism..as.yyyy.mm.dd",
 "chimérisme",
"X1ERE.LIGNE.ttt..détails."                                            ,
"X2.e.ligne..détails.",
"X3.e.ligne..détails.",                                                 
"X4.e.ligne..détails." ,                                                
"X5.e.ligne..détails.",  
"previous_auto.1" ,
"remarque.concernat.les.chimio.avant.allo" ,
"date_rechute." ,
"j0." ,   
"X.relapse_progression_after_allo",     
"best_response_after_allo." ,
"disease_status_at_transplant", 
"Total.infused.nucleated.cells..donneur.1..",                           
"Number.of.infused.CD34.positive.cells...donneur.1..",                  
 "Number.of.infused.CD3.positive.cells..T.cells....donneur.1..x.10E6.kg",
 "Total.infused.nucleated.cells...donneur.2..",                          
 "Number.of.infused.CD34.positive.cells...donneur.2.",                   
 "Number.of.infused.CD3.positive.cells..T.cells..donneur.2.x.10E6.kg")] <-rep("",34)


 meth[c("sex_dp",                                                               
"cmv_dp",
"ddn_donor",
"stem_cell_.source", 
"T.cell.depletion.with.MoAB..0.non..1..partial.T.depletion..",          
 "immunosupression.détails",
 "tbi_dose",
 "condit_details" ,
 "date_greffe_prece" ,                                                   
 "delai_allo_greffe_prece",  
 "anapathc",
 "date_dia_l", 
 "sex_patient.",
 "REMARQUE" ,                                                            
  "num_id"  ,                                                             
"j0",                                                                   
"first_name",                                                           
 "family_name",                                                          
"ddn" ,
"date_gvhd")] <-rep("",20)                                                           
 
 meth["age_greffec"] <-"~I(as.factor(ifelse(age_greffe<49,'< 49 years','> 49 years')))"                                                         
meth["prise_greffe1"] <-"~I(ifelse(greffe$prise_greffe %in% c('deces avant J30)))"
                                                        "lost graft","Lost graft","no engraftment[1]","No engraftment[1]"),0,1)                                                        
meth["survival_status_FU"]<-""                                                   
meth["deces"]<-"~I(ifelse(patients$survival_status_FU %in% c('alive'),0,1))"                                                                
meth["anapath"]<-""                                                   
meth[ "anapathc"] <-""                                                            
      meth["anapathc2"]   <-""                                                         
                                      
meth[ "disease_status_at_transplantc2"]<-""                                       
                                                  
                                         
meth ["hla_matchc"]<-"~I(as.factor(ifelse(hla_match %in% c('Identical sibling','Matched unrelated'),'1','0')))"                                                           
meth [ "donnor"] <- "~I(as.factor(ifelse(greffe$hla_match %in% c('Identical sibling','Mismatched relative'),'1','0'))"                                                               
meth [ "cause_death"] <-""                                                         
meth [ "cause_death_c"] <-""                                                        
meth [ "cause_death_c2"] <-"~I(ifelse(grepl(pat='HSCT',greffe$cause_death_c)& !is.na(greffe$cause_death_c),1,0)))"                                                       
meth[ "delai_dia_alloc"] <-"~I(as.factor(ifelse(greffe$delai_dia_allo<365,1,0)))"                                                     
meth[ "nbr_lignes_avt_alloc"]<-""                                                   
meth[ "nbr_lignes_avt_alloc2"]<-""                                                 
meth["programme_autoalloc"] <-"~I(as.factor(greffe$programme_autoallo))"                                                 
meth["previous_autoc"]<-"~I(as.factor(greffe$previous_auto))"                                                      
meth[ "rechute_prem_greffec"]<-as.factor(as.character(greffe$rechute_prem_greffe))                                                 
meth ["rechute_post_allo"]<-as.factor(ifelse(greffe$rechute_prem_greffec=="1","1","0"))                                                    
meth[ "nbr_donneurc"] <-as.factor(as.character(greffe$nbr_donneur))                                                         
meth[ "karnofsky_greffec"] <-as.factor(as.character(greffe$karnofsky_greffe))                                                   
meth["karnofsky_greffec2"]<-as.factor(ifelse(greffe$karnofsky_greffec %in% c("100","90","80"),"Normal activities 
                                            with or without efforts","Unable to carry on normal activity"))                                                 
meth["karnofsky_greffec3"]<-""                                                   
meth["stade_diac"] <-as.factor(ifelse(greffe$stade_dia %in% c("III","IV"), "III-IV","I-II"))                                                           
meth["sex_dp2"] <-as.factor(ifelse(greffe$sex_dp %in% c("F/F", "F/F/F", "M/M", 
                                                          "M/M/M"),"sex idem","different sex"))                                                             
meth["sex_dp3"] <-as.factor(ifelse(greffe$sex_dp %in% c("M/F"),"M/F","Others"))                                                            
meth["cmv_dp2"] <-ifelse(greffe$cmv_dp %in% c("neg/pos"),"neg/pos", "autres")                                                             
meth[c("rechute",                                                          
 "rechute_progression" ,                                                 
 "relapse_progression_transplant_c" ,                                    
 "rechute_progressionc"    ,                                             
 "rechute_progression_dc"  ,                                             
 "rechute_dc" ,                                                          
 "g_p",                                                                  
"rechute_progression_ghvd_dc" ,                                         
 "efs_statut",                                                           
"delai_dc"  ,                                                           
"delai_gvhd" ,                                                          
"date_rechute",                                                        
"date_rechute_progression" ,                                            
"date_rechute_progression_gvhd" ,                                       
 "delai_pfs"  ,                                                          
 "delai_rechute" ,                                                       
 "delai_rechutepg" ,                                                     
                                                           
 "deces_48" ,                                                            
 "delai_dc_48")] <-rep("",19)
 
 
 set.seed(36524)
essai <- mice(greffe, m=1, maxit=1, pred=pred,meth=meth)
 