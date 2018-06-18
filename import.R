### Import données ###

# 2 bases : une base patient (285)
# une base greffe car un patient a eu deux greffes




greffe<-read.csv2("C:/Users/adupont/Documents/projetstlouis/data/lnhtallogreffe.csv",na.strings = c("unknown","?"))
patients<-read.csv2("C:/Users/adupont/Documents/projetstlouis/data/lnhtallopatients.csv",na.strings = c("unknown","?"))

controle<-merge(greffe,patients,by=c("num_id","first_name","family_name"))

# 286 greffe
# 285 patients 
############# on vire le patient a deux greffe#####

greffe<-greffe[!greffe$ddn=="11/08/1964",]
####
table(greffe$ddn)
table(greffe$j0)
table(greffe$date_dia_l)
table(greffe$date_greffe_prece)
table(greffe$agvhd_date.)
table(greffe$date_fu)

greffe$j0..1<-NULL
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
greffe$agvhd<-ifelse(is.na(greffe$agvhd_grade),NA,greffe$agvhd)


table(greffe$agvhd)
table(greffe$agvhd_grade)

levels(greffe$agvhd_grade)<-c("Grade I", "Grade II", "Grade III", "Grade IV", 
                        "No aGvHD present (Grade 0)", "No aGvHD present (Grade 0)", 
                        "Present, grade unknown")

table(greffe$agvhd_grade)
greffe$agvhd_grade<-relevel(greffe$agvhd_grade,ref="No aGvHD present (Grade 0)")

greffe$agvhd3<-ifelse(greffe$agvhd_grade %in% c("Grade III", "Grade IV"),1,0)
greffe$agvhd3<-ifelse(is.na(greffe$agvhd_grade),NA,greffe$agvhd3)

greffe$agvhd3<-as.factor(greffe$agvhd3)

str(greffe$agvhd3)

table(greffe$cgvhd.)
greffe$cgvhd<-ifelse(greffe$cgvhd. %in% c("deces avant J100",
                                               "no[1]","No[1]"),0,1)
greffe$cgvhd<-ifelse(is.na(greffe$cgvhd.),NA,greffe$cgvhd)
table(greffe$cgvhd)
table(greffe$cgvhd_grade)


greffe$cgvhd.<-as.factor(greffe$cgvhd.)
levels(greffe$cgvhd.)<-c("Early death", "no", "no", "yes")

greffe$cgvhd_gradec<-greffe$cgvhd_grade
levels(greffe$cgvhd_gradec)<-c("no cGvh", "extensive", "extensive", "limited", 
                       "no cGvh", "grade unknown")

table(greffe$cgvhd_grade)
levels(greffe$cgvhd_grade)<-c("Early death (100D)", "Extensive", "Extensive", "Limited", 
  "No cGvh", "grade unknown")
table(greffe$cgvhd_grade)



greffe$gvhd<-ifelse(greffe$agvhd==1,1,0)
greffe$gvhd<-ifelse(greffe$cgvhd==1 & greffe$agvhd==0,1,greffe$gvhd)
table(greffe$gvhd)

greffe$date_gvhd<-ifelse(greffe$agvhd==1,greffe$agvhd_date,greffe$date_fu)
greffe$date_gvhd<-ifelse(greffe$cgvhd==1 & greffe$agvhd==0 ,greffe$cgvhd_date,greffe$date_gvhd)

table(greffe$date_gvhd)
greffe$date_gvhd<-as.Date(greffe$date_gvhd,origin = "1970-01-01")
table(greffe$date_gvhd,exclude=NULL)
greffe$date_gvhd

greffe$agvhd<-factor(greffe$agvhd,label=c("No","Yes"))
greffe$cgvhd<-factor(greffe$cgvhd,label=c("No or early death","Yes"))
#label(greffe$sex_dp) <- "Sex of patient/donor" 

greffe$age_greffec<-as.factor(ifelse(greffe$age_greffe<49,"< 49 years","> 49 years"))
table(greffe$age_greffec)

table(greffe$prise_greffe,exclude = NULL)

greffe$prise_greffe1<-ifelse(greffe$prise_greffe %in% c("deces avant J30",
                                          "lost graft","Lost graft","no engraftment[1]","No engraftment[1]"),0,1)
levels(greffe$prise_greffe)<-c( "Early death (30D)", "Engrafted", "Engrafted", "Lost graft", 
                              "Lost graft", "No engraftment", "No engraftment")

table(greffe$prise_greffe,exclude = NULL)
table(greffe$prise_greffe1,exclude = NULL)

table(patients$survival_status_FU.,exclude=NULL)
levels(patients$survival_status_FU)<-c("Alive", "Alive", "Dead", "Dead")
table(patients$survival_status_FU,exclude=NULL)
    patients$deces<-ifelse(patients$survival_status_FU %in% c("Alive"),0,1)
table(patients$deces,exclude=NULL)


table(greffe$survival_status_FU.)
greffe$survival_status_FU<-greffe$survival_status_FU.
levels(greffe$survival_status_FU)<-c("Alive", "Alive", "Dead", "Dead")
table(greffe$survival_status_FU)
greffe$deces<-ifelse(greffe$survival_status_FU %in% c("Alive"),0,1)
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
greffe$disease_status_at_transplantc<-reorder(greffe$disease_status_at_transplantc,new.order=c(1,3,2))
table(greffe$disease_status_at_transplantc,exclude = NULL)

greffe$disease_status_at_transplantc2<-greffe$disease_status_at_transplant
levels(greffe$disease_status_at_transplantc2)<-c("CR/PR", "CR/PR", "CR/PR", "CR/PR", "PD", "CR/PR", 
                                                "CR/PR", "CR/PR", 
                                                "CR/PR", "CR/PR", NA)
table(greffe$disease_status_at_transplantc2,exclude = NULL)


table(greffe$stem_cell_.source,exclude=NULL)
greffe$stem_cell_source<-greffe$stem_cell_.source
levels(greffe$stem_cell_source)<-c("BM", "CB", "PB", "PB")
table(greffe$stem_cell_source)


levels(patients$sex_patient)<-c("Female", "Male", "Male")
table(patients$sex_patient,exclude=NULL)

levels(greffe$sex_patient)<-c("Female", "Male", "Male")
table(greffe$sex_patient)


levels(greffe$tbi)<-c("No", "No", "Yes")
levels(greffe$best_response_after_allo)<-c("CR", "CR", "CR", "Not evaluable",
                                           "Not evaluable", 
                                           "Not evaluated", "PD", "PR", NA)

table(greffe$best_response_after_allo,exclude=NULL)
table(greffe$tbi)


table(greffe$hla_match)
levels(greffe$hla_match)<-c("Identical sibling", "Identical sibling", "Matched unrelated", 
                             "Mismatched relative", "Mismatched unrelated", "Unrelated CB")

greffe$hla_matchc<-ifelse(greffe$hla_match %in% c("Unrelated CB"),"Mismatched unrelated",as.character(greffe$hla_match))
greffe$hla_matchc<-factor(greffe$hla_matchc)
table(greffe$hla_matchc,exclude=NULL)

greffe$hla_matchc2<-ifelse(greffe$hla_match %in% c("Unrelated CB"),"Mismatched unrelated",as.character(greffe$hla_match))
greffe$hla_matchc2<-ifelse(greffe$hla_matchc2 %in% c("Mismatched unrelated","Mismatched relative"),"Alternative donnors",as.character(greffe$hla_match))
greffe$hla_matchc2<-factor(greffe$hla_matchc2)
table(greffe$hla_matchc2,exclude=NULL)



greffe$donnor<-ifelse(greffe$hla_match %in% c("Identical sibling","Matched unrelated"),"1","0")
greffe$donnor<-factor(greffe$donnor,labels=c("HLA mismatched","HLA matched"))
table(greffe$donnor)

table(greffe$manipu_cells,exclude=NULL)
levels(greffe$manipu_cells)<-c("No", "No", NA, "Yes")
table(greffe$manipu_cells,exclude=NULL)




table(greffe$cause_death)
levels(greffe$cause_death)<-c("HSCT related ILD", "HSCT related MAT", 
  "HSCT related MVO", "HSCT related", "HSCT related infection adénovirus", 
  "HSCT related IRA sur toxicité ciclosporine", "HSCT related MOF", 
  "HSCT related pneumopathie interstititelle", "HSCT related PTLD", 
  "HSCT related SDRA", "HSCT related toxicité cardiaque", 
  "HSCT related toxicité cérérale ( oed cérébral)", "HSCT related toxicité rénale et muqueuse du conditionnement", 
  "HSCT related", "HSCT relatedGVHd", "HSCT related GVHd+infection", 
  "HSCT related infection", "HSCT related infection ", "HSCT relatedinfection CMV", 
  "HSCT related infection CMV+ grade 4 GVHdig", "HSCT related infection fongique", 
  "HSCT related infection pulmonaire", "HSCT related infection virale", 
  "HSCT related MOF",NA, "Other", "Relapse or progression of original disease", 
  "Relapse or progression of original disease", "Secondary malignancy","Unknown")
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
                             "HSCT-toxicity",greffe$cause_death_c)

table(greffe$cause_death_c,greffe$cause_death)
table(greffe$cause_death_c,exclude=NULL)

greffe$cause_death_c<-as.factor(greffe$cause_death_c)

# 1 deces HSCT, 1 deces autre, 0 pas de deces  # 

greffe$cause_death_c2<-ifelse(grepl(pat="HSCT",greffe$cause_death_c)& !is.na(greffe$cause_death_c),1,0) 
greffe$cause_death_c2<-ifelse(greffe$deces==1 & !greffe$cause_death_c2==1,2,greffe$cause_death_c2) 
greffe$cause_death_c2<-ifelse(greffe$cause_death_c=="Unknown" & !is.na(greffe$cause_death_c),NA,greffe$cause_death_c2) 


table(greffe$cause_death_c2,exclude=NULL)
table(greffe$deces,exclude=NULL)

 ### cause décès pour cause spécifique décès c3 HSCT ###

greffe$cause_death_c3<-ifelse(greffe$cause_death_c2==1 , 1, 0)
table(greffe$cause_death_c2,greffe$cause_death_c3,exclude=NULL)



####


greffe$delai_dia_alloc<-factor(ifelse(greffe$delai_dia_allo<365,1,0),labels=c("NO","Yes"))

greffe$nbr_lignes_avt_alloc<-NA
greffe$nbr_lignes_avt_alloc<-ifelse(greffe$nbr_lignes_avt_allo <=2 & !is.na(greffe$nbr_lignes_avt_allo),
                                    "1 or 2", greffe$nbr_lignes_avt_alloc)
greffe$nbr_lignes_avt_alloc<-ifelse(greffe$nbr_lignes_avt_allo >2 & !is.na(greffe$nbr_lignes_avt_allo),
                                    ">2", greffe$nbr_lignes_avt_alloc)
greffe$nbr_lignes_avt_alloc<-as.factor(greffe$nbr_lignes_avt_alloc)
table(greffe$nbr_lignes_avt_alloc,exclude=NULL)

greffe$nbr_lignes_avt_alloc2<-NA
greffe$nbr_lignes_avt_alloc2<-ifelse(greffe$nbr_lignes_avt_allo <=3 & !is.na(greffe$nbr_lignes_avt_allo),
                                    greffe$nbr_lignes_avt_allo, greffe$nbr_lignes_avt_alloc2)
greffe$nbr_lignes_avt_alloc2<-ifelse(greffe$nbr_lignes_avt_allo >3 & !is.na(greffe$nbr_lignes_avt_allo),
                                   ">=4", greffe$nbr_lignes_avt_alloc2)
greffe$nbr_lignes_avt_alloc2<-as.factor(greffe$nbr_lignes_avt_alloc2)
greffe$nbr_lignes_avt_alloc2<-reorder(greffe$nbr_lignes_avt_alloc2,new.order=c(2,3,4,1))
table(greffe$nbr_lignes_avt_alloc2,exclude=NULL)

table(greffe$nbr_lignes_avt_allo,exclude = NULL)
table(greffe$nbr_lignes_avt_alloc,exclude = NULL)
table(greffe$nbr_lignes_avt_alloc2,exclude = NULL)
table(greffe$intensite_condi,exclude = NULL)

greffe$intensite_condi2<-ifelse(greffe$intensite_condi %in% c("RIC","NMA"),"RIC/NMA",greffe$intensite_condi)
greffe$intensite_condi2<-as.factor(greffe$intensite_condi2)
levels(greffe$intensite_condi2)<-c("MAC","RIC/NMA")

#greffe$intensite_condi2<-as.factor(ifelse(greffe$intensite_condi %in% c("NMA","MAC"),"MAC/NMA","RIC"))

greffe$intensite_condi3<-ifelse(greffe$intensite_condi %in% c("MAC"),"MAC",greffe$intensite_condi)
greffe$intensite_condi3<-ifelse(greffe$intensite_condi %in% c("NMA"),NA,greffe$intensite_condi3)
greffe$intensite_condi3<-as.factor(greffe$intensite_condi3)
levels(greffe$intensite_condi3)<-c("RIC","MAC")

table(greffe$intensite_condi3,exclude=NULL)



greffe$programme_autoalloc<-factor(greffe$programme_autoallo,label=c("No","Yes"))
greffe$previous_autoc<-factor(greffe$previous_auto,label=c("No","Yes"))
greffe$rechute_prem_greffec<-factor(as.character(greffe$rechute_prem_greffe),label=c("No","Yes"))

greffe$rechute_post_allo<-ifelse(greffe$rechute_prem_greffec=="No" & greffe$previous_auto==1,"No","Yes")
greffe$rechute_post_allo<-ifelse(greffe$rechute_prem_greffec=="No" & greffe$previous_auto==0,"No previous graft",greffe$rechute_post_allo)
greffe$rechute_post_allo<-as.factor(greffe$rechute_post_allo)

table(greffe$rechute_post_allo)
table(greffe$rechute_prem_greffec)

table(greffe$nbr_donneur,exclude=NULL)
greffe$nbr_donneurc<-as.factor(as.character(greffe$nbr_donneur))
table(greffe$nbr_donneurc)

greffe$karnofsky_greffec<-as.factor(as.character(greffe$karnofsky_greffe))

greffe$karnofsky_greffec2<-ifelse(greffe$karnofsky_greffec %in% c("100","90","80"),"Normal activities","Non normal activity")
greffe$karnofsky_greffec2<-ifelse(is.na(greffe$karnofsky_greffec),NA,greffe$karnofsky_greffec2)
greffe$karnofsky_greffec2<-as.factor(greffe$karnofsky_greffec2)

greffe$karnofsky_greffec3<-greffe$karnofsky_greffec

table(greffe$karnofsky_greffec,exclude=NULL)
levels(greffe$karnofsky_greffec3)<-c("100", "Unable to carry on normal activity", "Unable to carry on normal activity", "Unable to carry on normal activity", "Unable to carry on normal activity", "90-80", "90-80")
table(greffe$karnofsky_greffec3,exclude=NULL)
table(greffe$karnofsky_greffec2,exclude=NULL)

greffe$karnofsky_greffec4<-greffe$karnofsky_greffec

table(greffe$karnofsky_greffec,exclude=NULL)
levels(greffe$karnofsky_greffec4)<-c("100", "<90", "<90", "<90", "<90", "<90", "90")
table(greffe$karnofsky_greffec4,exclude=NULL)
greffe$karnofsky_greffec4<-reorder(greffe$karnofsky_greffec4, new.order=c(1,3,2))

greffe$stade_diac<-ifelse(greffe$stade_dia %in% c("III","IV"), "III-IV","I-II")
greffe$stade_diac<-ifelse(is.na(greffe$stade_dia),NA,greffe$stade_diac)
greffe$stade_diac<-as.factor(greffe$stade_diac)
  
#greffe$hla_matchc<-relevel(greffe$hla_matchc,ref="Yes")


greffe$sex_dp2<-ifelse(greffe$sex_dp %in% c("F/F", "F/F/F", "M/M", 
                                            "M/M/M"),"sex idem","different sex")
greffe$sex_dp2<-ifelse(is.na(greffe$sex_dp),NA,greffe$sex_dp2)
greffe$sex_dp2<-as.factor(greffe$sex_dp2)
greffe$sex_dp2<-relevel(greffe$sex_dp2,ref="sex idem")

greffe$sex_dp3<-ifelse(greffe$sex_dp %in% c("F/M","M/F/M"),"F/M","Others")
greffe$sex_dp3<-ifelse(is.na(greffe$sex_dp),NA,greffe$sex_dp3)
greffe$sex_dp3<-as.factor(greffe$sex_dp3)
greffe$sex_dp3<-relevel(greffe$sex_dp3,ref="Others")



greffe$cmv_dp2<-ifelse(greffe$cmv_dp %in% c("neg/neg"),"neg/neg", "Others")
greffe$cmv_dp2<-ifelse(is.na(greffe$cmv_dp),NA, greffe$cmv_dp2)
greffe$cmv_dp2<-as.factor(greffe$cmv_dp2)
greffe$cmv_dp2<-relevel(greffe$cmv_dp2,ref="neg/neg")

levels(greffe$sex_donor)<-c("Female", "Female", "Male", "Male", NA)

greffe$depletion<-as.factor(as.character(greffe$depletion))
levels(greffe$depletion)<-c("No","Partial T depletion")



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

table(greffe$rechute_progression,exclude=NULL)

greffe$relapse_progression_transplant_c<-greffe$relapse_progression_transplant_2.
levels(greffe$relapse_progression_transplant_c)<-c("Continuous progression", "Continuous progression", "No", 
  "Non applicable ", "Yes", "Yes")

table(greffe$relapse_progression_transplant_c,exclude=NULL)

table(greffe$relapse_progression_transplant_2.,exclude=NULL)

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


table(greffe$rechute_progression_dc,greffe$rechute_progressionc,exclude=NULL)




#il reste les 2 NA# 

# 2 dc, 1 rechute

greffe$rechute_dc<-ifelse(greffe$rechute==1,1,0)
greffe$rechute_dc<-ifelse(greffe$deces==1 & !greffe$rechute_dc==1,2,
                                      greffe$rechute_dc)                                   


table(greffe$rechute_dc,exclude=NULL)
#il reste les 2 NA# 


# 3 dc, 2 rechute/progression,1 ghvd
greffe$rechute_progression_ghvd_dc<-NULL
greffe$g_p<-as.character(ifelse(difftime(greffe$date_rechutep,greffe$date_gvhd)>=0,"g","r"))



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


#il reste les 2 NA# ancienne version 

greffe$efs_statut<-ifelse(greffe$rechute_progression_ghvd_dc==0,0,1)
table(greffe$efs_statut,exclude=NULL)


#### GRFS
greffe$grfs<-NULL
greffe$grfs<-ifelse(greffe$rechute_progression==1,2,0)
table(greffe$grfs,exclude=NULL)

greffe$agvhd_grave<-as.factor(ifelse(greffe$agvhd_grade %in% c("Grade III","Grade IV"),1,0))


greffe$grfs<-ifelse((greffe$agvhd_grade %in% c("Grade III","Grade IV")|
                      greffe$cgvhd_grade %in% c("Extensive"))
                    & !greffe$rechute_progression==1,1,greffe$grfs)
table(greffe$grfs,exclude=NULL)



greffe$grfs<-ifelse(greffe$deces==1 & !greffe$grfs==2  & !greffe$grfs==1,3,greffe$grfs)                                   
table(greffe$grfs,exclude=NULL)

greffe$grfs<-ifelse((greffe$agvhd_grade %in% c("Grade III","Grade IV")|
                                             greffe$cgvhd_grade %in% c("Extensive")) & 
                      greffe$rechute_progression==1 & greffe$g_p=="g",1,greffe$grfs)

table(greffe$grfs,exclude=NULL)

greffe$grfs<-ifelse((greffe$agvhd_grade %in% c("Present, grade unknown")|
                       greffe$cgvhd_grade %in% c("grade unknown")) ,NA,
                    greffe$grfs)

table(greffe$grfs,exclude=NULL)
table(greffe$rechute_progressionc,exclude=NULL)
table(greffe$rechute_progression_ghvd_dc,greffe$grfs,exclude=NULL)

greffe$efs_statut<-ifelse(greffe$grfs==0,0,1)
table(greffe$efs_statut,exclude=NULL)


# 1 GVHD 2 DC 




greffe$gvhd_dc<-ifelse(greffe$gvhd==1,1,0)
table(greffe$gvhd_dc,exclude=NULL)

greffe$gvhd_dc<-ifelse(greffe$gvhd_dc==0 & greffe$deces==1,2,
                       greffe$gvhd_dc)
table(greffe$gvhd_dc,exclude=NULL)


### Dates et délais###
patients$delai_dc<-difftime(patients$date_fu,patients$j0)/30.25

greffe$delai_dc<-difftime(greffe$date_fu,greffe$j0)/30.25 


greffe$delai_gvhd<-difftime(greffe$date_gvhd,greffe$j0)/30.25



greffe$date_rechute<-ifelse(greffe$rechute==1,greffe$date_rechutep,greffe$date_fu)
greffe$date_rechute<-as.Date(greffe$date_rechute,origin = "1970-01-01")
table(is.na(greffe$rechute), is.na(greffe$date_rechute))


greffe$date_rechute_progression<-ifelse(greffe$rechute_progression==1,greffe$date_rechutep,greffe$date_fu)
table(is.na(greffe$rechute_progression), is.na(greffe$date_rechute_progression))
greffe$date_rechute_progression<-as.Date(greffe$date_rechute_progression,origin = "1970-01-01")


greffe$date_rechute_progression_gvhd<-ifelse(greffe$grfs==2,greffe$date_rechutep,greffe$date_fu)
greffe$date_rechute_progression_gvhd<-ifelse(greffe$grfs==1,greffe$date_gvhd,greffe$date_rechute_progression_gvhd)
table(is.na(greffe$date_rechute_progression_gvhd),is.na(greffe$rechute_progression_ghvd_dc))
greffe$date_rechute_progression_gvhd<-as.Date(greffe$date_rechute_progression_gvhd,origin = "1970-01-01")

greffe$date_gvhd_dc<-ifelse(greffe$gvhd_dc==1,greffe$date_gvhd,greffe$date_fu)
greffe$date_gvhd_dc<-as.Date(greffe$date_gvhd_dc,origin = "1970-01-01")


greffe$delai_pfs<-difftime(greffe$date_rechute_progression,greffe$j0)/30.25
greffe$delai_rechute<-difftime(greffe$date_rechute,greffe$j0)/30.25
greffe$delai_rechutepg<-difftime(greffe$date_rechute_progression_gvhd,greffe$j0)/30.25
greffe$delai_gvhd<-difftime(greffe$date_gvhd_dc,greffe$j0)/30.25

az<-length(unique(greffe$num_id[greffe$best_response_after_allo %in% c("CR")]))




####  limite à 60 mois

greffe$cause_death_c360<-ifelse(greffe$cause_death_c3==1 & greffe$delai_dc>60,0,greffe$cause_death_c3)
greffe$rechute_progression_dc60<-ifelse(greffe$delai_pfs>60,0,greffe$rechute_progression_dc)

### on va exclure la première greffe du patient greffé deux fois car ça première greffe est 
#un échec pour l'analyse 
#quali(x=c("sex_patient"),nomx=c("Sexe"), data=patients,RAPPORT=F,SAVEFILE=T,chemin="C:/Users/Louise/Documents/Desespoir/Bases/resultats/")


### délai gvhd chronique et décès

greffe$delai_gvhdc_deces<-difftime(greffe$date_fu,greffe$cgvhd_date, units="days")/30.25
# ceux qui ont NA c'est ceux qui n'ont pas eu de gvhd chronique

greffe$delai_gvhdc_rechutep<-difftime(greffe$date_rechute_progression,greffe$cgvhd_date, units="days")/30.25
# certains ont des délai négatifs car ils ont la rechute avant la cgvhd 




#res <- compareGroups(anapathc ~ . , data = greffe[,c("anapathc","prise_greffe")])
#restab <- createTable(res, hide.no = "no", type = 1)
#export2latex(restab, loc = "bottom",caption = "Descriptives by year.", size = "small")

c("previous_auto", "previous_allo", 
  "tbi", "sex_dp", "cmv_dp", "best_response_after_allo", "date_rechute.", "X.relapse_progression_after_allo",  "status_FU.", "survival_status_FU.", 
  "cause_deces", "Main.cause.of.death.", "date_fu", "agvhd_date", 
  "cgvhd_date", "agvhd", "cgvhd", "prise_greffe1", "anapath2", 
  "anapathc", "disease_status_at_transplantc", "stem_cell_source", "survival_status_FU", 
  "gvhd", "date_gvhd", "delai_gvhd")
