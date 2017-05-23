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
pred["delai_allo_greffe_prece"]<-0
pred["tbi_dose"]
"ddn_donor"
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


c("Neutrophils....0.5.x.10.9.L.reached..i..ANC.recovery...i...",
"Date.neutrophils....0.5.engraftment.date" ,                            
 "Date.platelets.....b.50..b..as.yyyy.mm.dd" ,                           
 "Interval.to.neutrophil.recovery..i..engraftment...i...days.." "chimérisme"    ,                                                       
 "Date.of.chimaerism..as.yyyy.mm.dd","agvhd_date.","agvhd_grade." ,                                                          
 "delai_agvhd_allo",                                                     
 "aGvHD.stage.in.skin.",                                                 
 "aGvHD.stage.in.liver.",                                                
 "aGvHD.stage.in.gut.",                                                  
 "aGvHD.resolution.",                                                    
 "Date.of.resolution.of.aGvHD.",                                         
 "cgvhd_date.",                                                          
 "J.de.l.évaluation.du.chimérisme", 
"delai_cgvhd_allo",
"delai_allo_fu",                                                        
 "date_fu." ,

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
 "cause_death",                                                          
                                                        
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
 "delai_rechutepg" )