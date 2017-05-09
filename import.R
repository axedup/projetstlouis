### Import données ###

# 2 bases : une base patient (285)
# une base greffe car un patient a eu deux greffes

greffe<-read.csv2("C:/Users/adupont/Documents/projetstlouis/data/lnhtallogreffe.csv")
patients<-read.csv2("C:/Users/adupont/Documents/projetstlouis/data/lnhtallopatients.csv")

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

table(greffe$prise_greffe,exclude = NULL)

greffe$prise_greffe1<-ifelse(greffe$prise_greffe %in% c("deces avant J30",
                                          "lost graft[3]","Lost graft[3]","no engraftment[1]","No engraftment[1]"),0,1)
levels(greffe$prise_greffe)<-c( "","deces avant J30", "engrafted[2]", "engrafted[2]", "lost graft[3]", 
                              "lost graft[3]", "no engraftment[1]", "no engraftment[1]")

table(greffe$prise_greffe,exclude = NULL)

table(patients$survival_status_FU.)
levels(patients$survival_status_FU)<-c("alive[2]", "alive[2]", "dead[1]", "dead[1]")
table(patients$survival_status_FU)
patients$deces<-ifelse(patients$survival_status_FU %in% c("alive[2]"),0,1)
table(patients$deces)


table(greffe$anapath,exclude = NULL)
greffe$anapath2<-greffe$anapath
greffe$anapathc<-greffe$anapath
levels(greffe$anapath2)<-c("", "AITL", "ALCL", "ALCL", "ALCL", "ATLL", 
                  "EATL", "HS", "LGL", "NK leukemia", "NK/T nasal", "NOS")
levels(greffe$anapathc)<-c("", "AITL", "ALCL", "ALCL", "ALCL", "Autre", 
                           "Autre", "Autre", "Autre", "Autre", "Autre", "NOS")

table(greffe$anapathc,exclude = NULL)

table(greffe$stade_dia,exclude = NULL)
table(greffe$disease_status_at_transplant,exclude = NULL)
greffe$disease_status_at_transplantc<-greffe$disease_status_at_transplant
levels(greffe$disease_status_at_transplantc)<-c(NA, "CR", "CR", "CR", "CR", "PD", "PR", 
                                                "PR", "PR", 
                                        "PR", "PR", NA)
table(greffe$disease_status_at_transplantc,exclude = NULL)

table(greffe$stem_cell_.source)
greffe$stem_cell_source<-greffe$stem_cell_.source
levels(greffe$stem_cell_source)<-c("", "BM[1]", "CB[3]", "PB[2]", "PB[2]")



levels(patients$sex_patient)<-c("Female[2]", "Male[1]", "Male[1]")
table(patients$sex_patient)

levels(greffe$tbi)<-c("","No[1]", "No[1]", "Yes[2]")


### pb relapse, progression###
table(greffe$best_response_after_allo)
table(greffe$X.relapse_progression_after_allo)
table(greffe$relapse_progression_transplant_2.)
table(greffe$relapse_progression_transplant_2.,greffe$X.relapse_progression_after_allo)
table(greffe$best_response_after_allo,greffe$relapse_progression_transplant_2)

greffe$rechute_progression<-ifelse(greffe$relapse_progression_transplant_2.
                                   %in% c("No[1]","Non applicable","unknown"),0,1)

table(greffe$rechute_progression)

### Dates et délais###

patients$delai_dc<-difftime(patients$date_fu,patients$j0)
