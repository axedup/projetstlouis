### Import données ###

# 2 bases : une base patient (285)
# une base greffe car un patient a eu deux greffes




greffe<-read.csv2("C:/Users/adupont/Documents/projetstlouis/data/lnhtallogreffe.csv",na.strings = c("unknown","?"))
patients<-read.csv2("C:/Users/adupont/Documents/projetstlouis/data/lnhtallopatients.csv",na.strings = c("unknown","?"))

controle<-merge(greffe,patients,by=c("num_id","first_name","family_name"))

# 286 greffe
# 285 patients 


greffe<-greffe[!is.na(greffe$num_id),]

table(greffe$ddn)
table(greffe$j0)
table(greffe$date_dia_l)
table(greffe$date_greffe_prece)
table(greffe$agvhd_date.)
table(greffe$date_fu)


greffe$j0<-as.Date(as.character(greffe$j0),format = "%d/%m/%Y")
patients$j0<-as.Date(as.character(patients$j0),format = "%d/%m/%Y")

greffe$ddn<-as.Date(as.character(greffe$ddn),format = "%d/%m/%Y")
greffe$date_dia_l<-as.Date(as.character(greffe$date_dia_l),format = "%d/%m/%Y")
greffe$date_greffe_prece<-as.Date(as.character(greffe$date_greffe_prece),format = "%d/%m/%Y")
greffe$date_fu<-as.Date(as.character(greffe$date_fu),format = "%d/%m/%Y")
patients$date_fu<-as.Date(as.character(patients$date_fu),format = "%d/%m/%Y")

greffe$agvhd_date<-as.Date(as.character(greffe$agvhd_date),format = "%d/%m/%Y")
table(greffe$agvhd_date,exclude = NULL)
greffe$cgvhd_date<-as.Date(as.character(greffe$cgvhd_date),format = "%d/%m/%Y")
table(greffe$cgvhd_date,exclude = NULL)

table(greffe$agvhd_grade,exclude=TRUE)
greffe$agvhd<-ifelse(greffe$agvhd_grade %in% c("No aGvHD present (Grade 0)[7]",
                                              "no aGvHD present (Grade 0)[7]"),0,1)
table(greffe$agvhd)

table(greffe$cgvhd.)
greffe$cgvhd<-ifelse(greffe$cgvhd. %in% c("deces avant J100",
                                               "no[1]","No[1]"),0,1)
table(greffe$cgvhd)

greffe$gvhd<-ifelse(greffe$agvhd==1,1,0)
greffe$gvhd<-ifelse(greffe$cgvhd==1 & greffe$agvhd==0 ,1,0)
table(greffe$gvhd)

greffe$date_gvhd<-ifelse(greffe$agvhd==1,greffe$agvhd_date,NA)
greffe$date_gvhd<-ifelse(greffe$cgvhd==1 & greffe$agvhd==0 ,greffe$cgvhd_date,greffe$date_gvhd)
table(greffe$date_gvhd)
greffe$date_gvhd<-as.Date(greffe$date_gvhd,origin = "1970-01-01")
table(greffe$date_gvhd)
greffe$date_gvhd



label(greffe$sex_dp) <- "Sex of patient/donor" 




table(greffe$prise_greffe,exclude = NULL)

greffe$prise_greffe1<-ifelse(greffe$prise_greffe %in% c("deces avant J30",
                                          "lost graft","Lost graft","no engraftment[1]","No engraftment[1]"),0,1)
levels(greffe$prise_greffe)<-c( "deces avant J30", "engrafted", "engrafted", "lost graft", 
                              "lost graft", "no engraftment", "no engraftment")

table(greffe$prise_greffe,exclude = NULL)
table(greffe$prise_greffe1,exclude = NULL)

table(patients$survival_status_FU.)
levels(patients$survival_status_FU)<-c("alive", "alive", "dead", "dead")
table(patients$survival_status_FU)
patients$deces<-ifelse(patients$survival_status_FU %in% c("alive"),0,1)
table(patients$deces)


table(greffe$survival_status_FU.)
greffe$survival_status_FU<-greffe$survival_status_FU.
levels(greffe$survival_status_FU)<-c("alive", "alive", "dead", "dead")
table(greffe$survival_status_FU)
greffe$deces<-ifelse(greffe$survival_status_FU %in% c("alive"),0,1)
table(greffe$deces)





table(greffe$anapath,exclude = NULL)
greffe$anapath2<-greffe$anapath
greffe$anapathc<-greffe$anapath
levels(greffe$anapath2)<-c( "AITL", "ALCL", "ALCL", "ALCL", "ATLL", 
                  "EATL", "HS", "LGL", "NK leukemia", "NK/T nasal", "NOS")
levels(greffe$anapathc)<-c( "AITL", "ALCL", "ALCL", "ALCL", "Others", 
                           "Others", "Others", "Others", "Others", "Others", "NOS")

table(greffe$anapathc,exclude = NULL)
table(greffe$anapath,exclude = NULL)


table(greffe$stade_dia,exclude = NULL)
table(greffe$disease_status_at_transplant,exclude = NULL)
greffe$disease_status_at_transplantc<-greffe$disease_status_at_transplant
levels(greffe$disease_status_at_transplantc)<-c("CR", "CR", "CR", "CR", "PD", "PR", 
                                                "PR", "PR", 
                                        "PR", "PR", NA)
table(greffe$disease_status_at_transplantc,exclude = NULL)

table(greffe$stem_cell_.source)
greffe$stem_cell_source<-greffe$stem_cell_.source
levels(greffe$stem_cell_source)<-c("BM", "CB", "PB", "PB")



levels(patients$sex_patient)<-c("Female", "Male", "Male")
table(patients$sex_patient)

levels(greffe$tbi)<-c("No", "No", "Yes")
levels(greffe$best_response_after_allo)<-c("cr", "cr", "cr", "Not evaluable",
                                           "Not evaluable", 
                                           "Not evaluated", "PD", "PR", NA)

table(greffe$best_response_after_allo)
table(greffe$tbi)


table(greffe$hla_match)
levels(greffe$hla_match)<-c("Identical sibling", "Identical sibling", "Matched unrelated", 
                             "Mismatched relative", "Mismatched unrelated", "Unrelated CB")

greffe$hla_matchc<-ifelse(greffe$hla_match %in% c("Identical sibling","Matched unrelated"),"1","0")
greffe$donnor<-ifelse(greffe$hla_match %in% c("Identical sibling","Mismatched relative"),"1","0")
levels(greffe$manipu_cells)<-c("none", "none", "unknown", "yes")





table(greffe$cause_death)
levels(greffe$cause_death)<-c("HSCT related pneumopathie interstititelle", "HSCT related MAT", 
  "HSCT related MVO", "HSCT related", "HSCT related infection adénovirus", 
  "HSCT related IRA sur toxicité ciclosporine", "HSCT related MOF", 
  "HSCT related pneumopathie interstititelle", "HSCT related PTLD", 
  "HSCT related SDRA", "HSCT related toxicité cardiaque", 
  "HSCT related toxicité cérérale ( oed cérébral)", "HSCT related toxicité rénale et muqueuse du conditionnement", 
  "HSCT related", "HSCT relatedGVHd", "HSCT related GVHd+infection", 
  "HSCT related infection", "HSCT related infection ", "HSCT relatedinfection CMV", 
  "HSCT related infection CMV+ grade 4 GVHdig", "HSCT related infection fongique", 
  "HSCT related infection pulmonaire", "HSCT related infection virale", 
  "HSCT related MOF",NA, "Other", "relapse or progression of original disease", 
  "relapse or progression of original disease", "Secondary malignancy","unknown")
table(greffe$cause_death)

table(greffe$cause_death,greffe$cause_deces)

greffe$cause_death_c<-as.character(greffe$cause_death)
greffe$cause_death_c<-ifelse((grepl(pat="infection",greffe$cause_death)|
                               grepl(pat="HSCT related infection virale",greffe$cause_death)) 
                               
                               & greffe$cause_deces==2,
"HSCT-infection",greffe$cause_death_c)
greffe$cause_death_c<-ifelse(grepl(pat="GVHd",greffe$cause_death) & greffe$cause_deces==2 &
                               !is.na(greffe$cause_deces),
                             "HSCT-GVHd",greffe$cause_death_c)

greffe$cause_death_c<-ifelse(grepl(pat="GVHd",greffe$cause_death) & 
                               (grepl(pat="infection",greffe$cause_death)|
                                  grepl(pat="HSCT related infection virale",greffe$cause_death))
                             & greffe$cause_deces==2 &
                               !is.na(greffe$cause_deces),
                             "HSCT-GVHd + infection",greffe$cause_death_c)

greffe$cause_death_c<-ifelse(grepl(pat="toxicité",greffe$cause_death) 
                             & greffe$cause_deces==2 &
                               !is.na(greffe$cause_deces),
                             "HSCT-toxicité",greffe$cause_death_c)

table(greffe$cause_death_c,greffe$cause_death)
table(greffe$cause_death_c)


greffe$delai_dia_alloc<-ifelse(greffe$delai_dia_allo<365,1,0)













### pb relapse, progression###
table(greffe$best_response_after_allo)
table(greffe$X.relapse_progression_after_allo)
table(greffe$relapse_progression_transplant_2.)
table(greffe$relapse_progression_transplant_2.,greffe$X.relapse_progression_after_allo)
table(greffe$best_response_after_allo,greffe$relapse_progression_transplant_2)
## un patient qui a une best réponse à PD  a bien une continous progression dans relapse
#et progression 
# un patient a une CR puis rechute après 


greffe$rechute_progression<-ifelse(greffe$relapse_progression_transplant_2.
                                   %in% c("No[1]","Non applicable " ,"unknown"),0,1)

table(greffe$rechute_progression)


greffe$rechute_progression_dc<-ifelse(greffe$rechute_progression==1,2,
                                      0)
greffe$rechute_progression_dc<-ifelse(greffe$deces==1 & !greffe$rechute_progression_dc==0,1,
                                      greffe$rechute_progression_dc)                                   


table(greffe$rechute_progression_dc)

### Dates et délais###

patients$delai_dc<-difftime(patients$date_fu,patients$j0)
 
greffe$delai_gvhd<-difftime(greffe$date_gvhd,greffe$j0)

### on va exclure la première greffe du patient greffé deux fois car ça première greffe est 
#un échec pour l'analyse 


#res <- compareGroups(anapathc ~ . , data = greffe[,c("anapathc","prise_greffe")])
#restab <- createTable(res, hide.no = "no", type = 1)
#export2latex(restab, loc = "bottom",caption = "Descriptives by year.", size = "small")

c("previous_auto", "previous_allo", 
  "tbi", "sex_dp", "cmv_dp", "best_response_after_allo", "date_rechute.", "X.relapse_progression_after_allo",  "status_FU.", "survival_status_FU.", 
  "cause_deces", "Main.cause.of.death.", "date_fu", "agvhd_date", 
  "cgvhd_date", "agvhd", "cgvhd", "prise_greffe1", "anapath2", 
  "anapathc", "disease_status_at_transplantc", "stem_cell_source", "survival_status_FU", 
  "gvhd", "date_gvhd", "delai_gvhd")
