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
greffe$date_rechutep<-greffe$date_rechute
greffe$date_rechutep<-as.Date(as.character(greffe$date_rechutep),format = "%d/%m/%Y")

greffe$date_fu<-as.Date(as.character(greffe$date_fu),format = "%d/%m/%Y")
patients$date_fu<-as.Date(as.character(patients$date_fu),format = "%d/%m/%Y")

greffe$agvhd_date<-as.Date(as.character(greffe$agvhd_date),format = "%d/%m/%Y")
table(greffe$agvhd_date,exclude = NULL)
greffe$cgvhd_date<-as.Date(as.character(greffe$cgvhd_date),format = "%d/%m/%Y")
table(greffe$cgvhd_date,exclude = NULL)

table(greffe$agvhd_grade,exclude=TRUE)
greffe$agvhd<-ifelse(greffe$agvhd_grade %in% c("No aGvHD present (Grade 0)[7]",
                                              "no aGvHD present (Grade 0)[7]"),0,1)

greffe$agvhd<-as.factor(greffe$agvhd)
table(greffe$agvhd)
table(greffe$agvhd_grade)

levels(greffe$agvhd_grade)<-c("Grade I", "Grade II", "Grade III", "Grade IV", 
                        "No aGvHD present (Grade 0)", "No aGvHD present (Grade 0)", 
                        "Present, grade unknown")

table(greffe$agvhd_grade)
greffe$agvhd_grade<-relevel(greffe$agvhd_grade,ref="No aGvHD present (Grade 0)")

table(greffe$cgvhd.)
greffe$cgvhd<-ifelse(greffe$cgvhd. %in% c("deces avant J100",
                                               "no[1]","No[1]"),0,1)
table(greffe$cgvhd)
table(greffe$cgvhd_grade)
greffe$cgvhd<-as.factor(greffe$cgvhd)

greffe$cgvhd_gradec<-greffe$cgvhd_grade
levels(greffe$cgvhd_gradec)<-c("no cGvh", "extensive", "extensive", "limited", 
                       "no cGvh", NA)

table(greffe$cgvhd_grade)
levels(greffe$cgvhd_grade)<-c("deces avant J100", "extensive", "extensive", "limited", 
  "no cGvh", "unknown")
table(greffe$cgvhd_grade)



greffe$gvhd<-ifelse(greffe$agvhd==1,1,0)
greffe$gvhd<-ifelse(greffe$cgvhd==1 & greffe$agvhd==0 ,1,0)
table(greffe$gvhd)

greffe$date_gvhd<-ifelse(greffe$agvhd==1,greffe$agvhd_date,greffe$date_fu)
greffe$date_gvhd<-ifelse(greffe$cgvhd==1 & greffe$agvhd==0 ,greffe$cgvhd_date,greffe$date_gvhd)

table(greffe$date_gvhd)
greffe$date_gvhd<-as.Date(greffe$date_gvhd,origin = "1970-01-01")
table(greffe$date_gvhd,exclude=NULL)
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
table(patients$survival_status_FU,exclude=NULL)
patients$deces<-ifelse(patients$survival_status_FU %in% c("alive"),0,1)
table(patients$deces,exclude=NULL)


table(greffe$survival_status_FU.)
greffe$survival_status_FU<-greffe$survival_status_FU.
levels(greffe$survival_status_FU)<-c("alive", "alive", "dead", "dead")
table(greffe$survival_status_FU)
greffe$deces<-ifelse(greffe$survival_status_FU %in% c("alive"),0,1)
table(greffe$deces)





table(greffe$anapath,exclude = NULL)
greffe$anapath2<-greffe$anapath
greffe$anapathc<-greffe$anapath
greffe$anapathc2<-greffe$anapath
levels(greffe$anapath2)<-c( "AITL", "ALCL", "ALCL", "ALCL", "ATLL", 
                  "EATL", "HS", "LGL", "NK leukemia", "NK/T nasal", "NOS")



table(greffe$anapathc2,exclude = NULL)


levels(greffe$anapathc)<-c( "AITL", "ALCL", "ALCL", "ALCL", "Others", 
                           "Others", "Others", "Others", "Others", "Others", "NOS")

levels(greffe$anapathc2)<-c( "AITL", "ALCL", "ALCL", "ALCL", "ATLL", 
                            "Others", "Others", "Others", "Others", "NK/T nasal", "NOS")
greffe$anapathc2<-reorder(greffe$anapathc2,new.order = c(6,1,2,3,5,4))

table(greffe$anapathc,exclude = NULL)
table(greffe$anapath,exclude = NULL)
table(greffe$anapathc2,exclude = NULL)

table(greffe$stade_dia,exclude = NULL)
table(greffe$disease_status_at_transplant,exclude = NULL)
greffe$disease_status_at_transplantc<-greffe$disease_status_at_transplant
levels(greffe$disease_status_at_transplantc)<-c("CR", "CR", "CR", "CR", "PD", "PR", 
                                                "PR", "PR", 
                                        "PR", "PR", NA)
table(greffe$disease_status_at_transplantc,exclude = NULL)

greffe$disease_status_at_transplantc2<-greffe$disease_status_at_transplant
levels(greffe$disease_status_at_transplantc2)<-c("CR/PR", "CR/PR", "CR/PR", "CR/PR", "PD", "CR/PR", 
                                                "CR/PR", "CR/PR", 
                                                "CR/PR", "CR/PR", NA)
table(greffe$disease_status_at_transplantc2,exclude = NULL)


table(greffe$stem_cell_.source)
greffe$stem_cell_source<-greffe$stem_cell_.source
levels(greffe$stem_cell_source)<-c("BM", "CB", "PB", "PB")



levels(patients$sex_patient)<-c("Female", "Male", "Male")
table(patients$sex_patient)

levels(greffe$sex_patient)<-c("Female", "Male", "Male")
table(greffe$sex_patient)


levels(greffe$tbi)<-c("No", "No", "Yes")
levels(greffe$best_response_after_allo)<-c("cr", "cr", "cr", "Not evaluable",
                                           "Not evaluable", 
                                           "Not evaluated", "PD", "PR", NA)

table(greffe$best_response_after_allo,exclude=NULL)
table(greffe$tbi)


table(greffe$hla_match)
levels(greffe$hla_match)<-c("Identical sibling", "Identical sibling", "Matched unrelated", 
                             "Mismatched relative", "Mismatched unrelated", "Unrelated CB")

greffe$hla_matchc<-ifelse(greffe$hla_match %in% c("Identical sibling","Matched unrelated"),"1","0")
greffe$hla_matchc<-as.factor(greffe$hla_matchc)
greffe$donnor<-ifelse(greffe$hla_match %in% c("Identical sibling","Mismatched relative"),"1","0")
greffe$donnor<-as.factor(greffe$donnor)

table(greffe$manipu_cells,exclude=NULL)
levels(greffe$manipu_cells)<-c("none", "none", NA, "yes")
table(greffe$manipu_cells,exclude=NULL)




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

greffe$cause_death_c<-as.factor(greffe$cause_death_c)


greffe$delai_dia_alloc<-as.factor(ifelse(greffe$delai_dia_allo<365,1,0))

greffe$nbr_lignes_avt_alloc<-NA
greffe$nbr_lignes_avt_alloc<-ifelse(greffe$nbr_lignes_avt_allo <=2 & !is.na(greffe$nbr_lignes_avt_allo),
                                    "1 or 2", greffe$nbr_lignes_avt_alloc)
greffe$nbr_lignes_avt_alloc<-ifelse(greffe$nbr_lignes_avt_allo >2 & !is.na(greffe$nbr_lignes_avt_allo),
                                    ">2", greffe$nbr_lignes_avt_alloc)
greffe$nbr_lignes_avt_alloc<-as.factor(greffe$nbr_lignes_avt_alloc)


greffe$nbr_lignes_avt_alloc2<-NA
greffe$nbr_lignes_avt_alloc2<-ifelse(greffe$nbr_lignes_avt_allo <=3 & !is.na(greffe$nbr_lignes_avt_allo),
                                    greffe$nbr_lignes_avt_allo, greffe$nbr_lignes_avt_alloc2)
greffe$nbr_lignes_avt_alloc2<-ifelse(greffe$nbr_lignes_avt_allo >3 & !is.na(greffe$nbr_lignes_avt_allo),
                                    4, greffe$nbr_lignes_avt_alloc2)
greffe$nbr_lignes_avt_alloc2<-as.factor(greffe$nbr_lignes_avt_alloc2)




table(greffe$nbr_lignes_avt_alloc,exclude = NULL)
table(greffe$nbr_lignes_avt_alloc2,exclude = NULL)
table(greffe$intensite_condi,exclude = NULL)

greffe$programme_autoalloc<-as.factor(greffe$programme_autoallo)
greffe$previous_autoc<-as.factor(greffe$previous_auto)
greffe$rechute_prem_greffec<-as.factor(as.character(greffe$rechute_prem_greffe))

table(greffe$nbr_donneur)
greffe$nbr_donneurc<-as.factor(as.character(greffe$nbr_donneur))
table(greffe$nbr_donneurc)

greffe$karnofsky_greffec<-as.factor(as.character(greffe$karnofsky_greffe))

greffe$karnofsky_greffec2<-as.factor(ifelse(greffe$karnofsky_greffec %in% c("100","90","80"),"Normal activities 
                                            with or without efforts","Unable to carry on normal activity"))


greffe$stade_diac<-as.factor(ifelse(greffe$stade_dia %in% c("III","IV"), "III-IV","I-II"))


greffe$hla_matchc<-relevel(greffe$hla_matchc,ref="1")


greffe$sex_dp2<-as.factor(ifelse(greffe$sex_dp %in% c("F/F", "F/F/F", "M/M", 
                                            "M/M/M"),"sex idem","different sex"))

greffe$sex_dp2<-relevel(greffe$sex_dp2,ref="sex idem")

greffe$cmv_dp2<-ifelse(greffe$cmv_dp %in% c("neg/neg"),"all neg", "different")
greffe$cmv_dp2<-ifelse(greffe$cmv_dp %in% c("pos/pos", "pos/pos/pos"),"all positive", "different")
greffe$cmv_dp2<-as.factor(greffe$cmv_dp2)


levels(greffe$sex_donor)<-c("Female", "Female", "Male", "Male", NA)

### Les ages###

summary(greffe$age_donor) # pour les CB l'age est sans intéret
# a ne pas décrire (subset sur l'origine de la transplant)
greffe$age_donor<-as.numeric(greffe$age_donor)

summary(greffe$age_dia) 
summary(greffe$age_greffe)






### pb relapse, progression###
table(greffe$best_response_after_allo)
table(greffe$X.relapse_progression_after_allo)
table(greffe$relapse_progression_transplant_2.,exclude=NULL)
table(greffe$relapse_progression_transplant_2.,greffe$X.relapse_progression_after_allo)
table(greffe$best_response_after_allo,greffe$relapse_progression_transplant_2)
## un patient qui a une best réponse à PD  a bien une continous progression dans relapse
#et progression 
# un patient a une CR puis rechute après 

levels(greffe$relapse_progression_transplant_2.)
dput(levels(greffe$relapse_progression_transplant_2.))

greffe$rechute<-ifelse(greffe$relapse_progression_transplant_2.
                                   %in% c("yes[2]", "Yes[2]"),1,0)

greffe$rechute<-ifelse(is.na(greffe$relapse_progression_transplant_2.),NA,greffe$rechute)

table(greffe$rechute,exclude=NULL)



greffe$rechute_progression<-ifelse(greffe$relapse_progression_transplant_2.
                                   %in% c("No[1]","Non applicable " ),0,1)
greffe$rechute_progression<-ifelse(is.na(greffe$relapse_progression_transplant_2.),NA,greffe$rechute_progression)



greffe$relapse_progression_transplant_c<-greffe$relapse_progression_transplant_2.
levels(greffe$relapse_progression_transplant_c)<-c("continuous progression", "continuous progression", "No", 
  "Non applicable ", "yes", "yes")

table(greffe$relapse_progression_transplant_c)

table(greffe$relapse_progression_transplant_2.)

# 2 NA mais qui ont une date de FU (seul leur statut vital est connu) :
# soit on les exclut de l'analyse (statut rechute en NA)
# soit on les met à 0 et perdus de vu

table(greffe$rechute_progression,exclude=NULL)
table(greffe$rechute_progression,is.na(greffe$date_rechutep))


# non applicable décès précoce pas le temps d'avoir une récidive #
greffe$rechute_progressionc<-ifelse(greffe$relapse_progression_transplant_2.
                                   %in% c("No[1]","Non applicable " ,"unknown"),0,1)
greffe$rechute_progressionc<-ifelse(greffe$deces==1 & greffe$rechute_progressionc==0,1,greffe$rechute_progressionc)


table(greffe$rechute_progressionc,exclude=NULL)


# 2 dc, 1 rechute/progression

greffe$rechute_progression_dc<-ifelse(greffe$rechute_progression==1,1,
                                      0)
greffe$rechute_progression_dc<-ifelse(greffe$deces==1 & !greffe$rechute_progression_dc==1,2,
                                      greffe$rechute_progression_dc)                                   


table(greffe$rechute_progression_dc,exclude=NULL)
#il reste les 2 NA# 

# 2 dc, 1 rechute

greffe$rechute_dc<-ifelse(greffe$rechute==1,1,0)
greffe$rechute_dc<-ifelse(greffe$deces==1 & !greffe$rechute_dc==1,2,
                                      greffe$rechute_dc)                                   


table(greffe$rechute_dc,exclude=NULL)
#il reste les 2 NA# 


# 3 dc, 2 rechute/progression,1 ghvd

greffe$g_p<-ifelse(difftime(greffe$date_rechutep,greffe$date_gvhd)>=0,"g","r")



greffe$rechute_progression_ghvd_dc<-ifelse(greffe$rechute_progression==1,2,0)
table(greffe$rechute_progression_ghvd_dc,exclude=NULL)

greffe$rechute_progression_ghvd_dc<-ifelse(greffe$gvhd==1 & !greffe$rechute_progression_ghvd_dc==2,1,
                                           greffe$rechute_progression_ghvd_dc)
table(greffe$rechute_progression_ghvd_dc,exclude=NULL)
greffe$rechute_progression_ghvd_dc<-ifelse(greffe$deces==1 & !greffe$rechute_progression_ghvd_dc==2
                                           & !greffe$rechute_progression_ghvd_dc==1,3,
                          greffe$rechute_progression_ghvd_dc)                                   
table(greffe$rechute_progression_ghvd_dc,exclude=NULL)

greffe$rechute_progression_ghvd_dc<-ifelse(greffe$gvhd==1 & greffe$rechute_progression==1 & greffe$g_p=="g",1,greffe$rechute_progression_ghvd_dc)


table(greffe$rechute_progression_ghvd_dc,exclude=NULL)
#il reste les 2 NA# 






### Dates et délais###

patients$delai_dc<-difftime(patients$date_fu,patients$j0)/30

greffe$delai_dc<-difftime(greffe$date_fu,greffe$j0)/30 


greffe$delai_gvhd<-difftime(greffe$date_gvhd,greffe$j0)/30



greffe$date_rechute<-ifelse(greffe$rechute==1,greffe$date_rechutep,greffe$date_fu)
greffe$date_rechute<-as.Date(greffe$date_rechute,origin = "1970-01-01")
table(is.na(greffe$rechute), is.na(greffe$date_rechute))


greffe$date_rechute_progression<-ifelse(greffe$rechute_progression==1,greffe$date_rechutep,greffe$date_fu)
table(is.na(greffe$rechute_progression), is.na(greffe$date_rechute_progression))
greffe$date_rechute_progression<-as.Date(greffe$date_rechute_progression,origin = "1970-01-01")


greffe$date_rechute_progression_gvhd<-ifelse(greffe$rechute_progression_ghvd_dc==2,greffe$date_rechutep,greffe$date_fu)
greffe$date_rechute_progression_gvhd<-ifelse(greffe$rechute_progression_ghvd_dc==1,greffe$date_gvhd,greffe$date_rechute_progression_gvhd)
table(is.na(greffe$date_rechute_progression_gvhd),is.na(greffe$rechute_progression_ghvd_dc))
greffe$date_rechute_progression_gvhd<-as.Date(greffe$date_rechute_progression_gvhd,origin = "1970-01-01")


greffe$delai_efs<-difftime(greffe$date_rechute_progression,greffe$j0)/30.25
greffe$delai_rechute<-difftime(greffe$date_rechute,greffe$j0)/30.25
greffe$delai_rechutepg<-difftime(greffe$date_rechute_progression_gvhd,greffe$j0)/30.25


### on va exclure la première greffe du patient greffé deux fois car ça première greffe est 
#un échec pour l'analyse 
quali(x=c("sex_patient"),nomx=c("Sexe"), data=patients,RAPPORT=F,SAVEFILE=T,chemin="C:/Users/Louise/Documents/Desespoir/Bases/resultats/")





#res <- compareGroups(anapathc ~ . , data = greffe[,c("anapathc","prise_greffe")])
#restab <- createTable(res, hide.no = "no", type = 1)
#export2latex(restab, loc = "bottom",caption = "Descriptives by year.", size = "small")

c("previous_auto", "previous_allo", 
  "tbi", "sex_dp", "cmv_dp", "best_response_after_allo", "date_rechute.", "X.relapse_progression_after_allo",  "status_FU.", "survival_status_FU.", 
  "cause_deces", "Main.cause.of.death.", "date_fu", "agvhd_date", 
  "cgvhd_date", "agvhd", "cgvhd", "prise_greffe1", "anapath2", 
  "anapathc", "disease_status_at_transplantc", "stem_cell_source", "survival_status_FU", 
  "gvhd", "date_gvhd", "delai_gvhd")
