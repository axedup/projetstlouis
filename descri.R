correspondance<-data.frame(var=c("delai_dia_alloc","anapath","stade_dia","disease_status_at_transplantc","disease_status_at_transplant",
                                 "karnofsky_greffec", "previous_autoc","programme_autoalloc","rechute_prem_greffec",
                                 "nbr_lignes_avt_alloc",
                                 "donnor","hla_matchc","hla_match",
                                 "sex_dp3","sex_dp2","sex_dp","cmv_dp","cmv_dp3","stem_cell_source","tbi","intensite_condi","condit_details","manipu_cells","nbr_donneurc","agvhd","agvhd_grade","cgvhd",
                                 "cgvhd_grade","cause_death_c","best_response_after_allo","relapse_progression_transplant_c"),
                           cor=c(">12 months delay","Histopathologic subtypes","Stage at diagnostic","disease status at transplant","disease status at transplant",
                           "Karnofsky", "previous autoSCT","program autoallo","relapse first graft",
                           "No of lines before alloSCT",
                           "Donnor related","HLA match","HLA match","Sex of patient/donnor",
                           "Sex of patient/donnor", "Sex of patient/donnor","CMV serostatus of patient/donnor",
                           "CMV serostatus of patient/donnor", "Source of stem cells","tbi","conditioning intensity","conditioning","cells manipulation",
                           "no of donnors","agvhd","agvhd grade","cgvhd",
                           "cgvhd grade","Cause of death","best reponse after SCT","Relapse/progression"))





w<-quali(x=c("delai_dia_alloc","anapath","stade_dia","disease_status_at_transplantc","disease_status_at_transplant",
             "karnofsky_greffec", "previous_autoc","programme_autoalloc","rechute_prem_greffec",
             "nbr_lignes_avt_alloc",
             "donnor","hla_matchc","hla_match",
             "sex_dp","cmv_dp","stem_cell_source","tbi","intensite_condi","condit_details","manipu_cells","nbr_donneurc","agvhd","agvhd_grade","cgvhd",
             "cgvhd_grade","cause_death_c","best_response_after_allo","relapse_progression_transplant_c"),
         nomx=c(" $\\leq 12 $ months betwenn diagnosis and allo SCT","Histopathologic subtypes","Stage at diagnostic","disease status at transplant","disease status at transplant",
                "Karnofsky", "previous autoSCT","program autoallo","relapse first graft",
                "No of lines before alloSCT",
                "Donnor related","HLA match","HLA match",
                "sex of patient/donnor","CMV serostatus of patient/donnor",
                "Source of stem cells","tbi","conditioning intensity","conditioning","cells manipulation",
                "no of donnors","agvhd","agvhd grade","cgvhd",
                "cgvhd grade","Cause of death","Best reponse after SCT","Relapse/progression"),
         data=greffe,RAPPORT=F,SAVEFILE=T,chemin="C:/Users/Louise/Documents/Desespoir/Bases/resultats/")


#write.csv2(w,file="C:/Users/adupont/Documents/projetstlouis/resultats/descri_uni_quali.csv")


# #res <- compareGroups(anapathc2 ~ ., data = greffe[greffe$anapathc2 %in% c("AITL","ALCL","NOS","ATLL")
#                                                  ,c("anapathc2","delai_dia_alloc","stade_diac","disease_status_at_transplantc",
#                                                      "karnofsky_greffec2", "previous_autoc","programme_autoalloc","rechute_prem_greffec",
#                                                      "nbr_lignes_avt_alloc")])


#restab <- createTable(res, hide.no = "no", type = 2,show.all = TRUE,)
#restab

WD<-NA

for (i in c("delai_dia_alloc","stade_diac","disease_status_at_transplantc","disease_status_at_transplant",
            "karnofsky_greffec", "previous_autoc","programme_autoalloc","rechute_prem_greffec",
            "nbr_lignes_avt_alloc",
            "donnor","hla_matchc","hla_match",
            "sex_dp","cmv_dp","stem_cell_source","tbi","intensite_condi","condit_details","manipu_cells","nbr_donneurc","agvhd","agvhd_grade","cgvhd",
            "cgvhd_grade","cause_death_c","best_response_after_allo","relapse_progression_transplant_c")){

histo<- greffe%>%
  group_by(anapathc2) %>%
  do(data.frame(n=table(.[,i],exclude=NULL),pour=c(round(prop.table(table(.[,i]))*100,1),"")))



histo<-as.data.frame(histo)
histo$anapathc2<-as.factor(histo$anapathc2)
wu <- reshape(histo, 
             timevar = "anapathc2",
             idvar = c("n.Var1"),
             direction = "wide")

ws<-c(i,rep("",nrow(wu)-1))
wu<-cbind(ws,wu)

WD<-rbind(WD,wu)

}

WD$ro<-1:nrow(WD)
WD<-merge(correspondance,WD,by.x="var",by.y="ws",all.y=TRUE)
WD<-WD[order(WD$ro), ]

###
summary(as.numeric(greffe$delai_dc)*30.25)



patientg<-TABKRIS(baz=greffe,vect.var = c("age_dia","sex_patient","age_dia","stade_dia","stade_diac","anapath","anapathc2","centre"),
                        vect.quali = c(0,1,0,1,1,1,1,1),
                        varint=NULL,valvarint = NULL,
                        nomvarint = NULL,
                        test=NULL,
                        vecnoms=c("Age at diagnostic","Patient sex","Age at diagnostic","Stage at diagnostic","Stage at diagnostic",
                                  
                                  "Subtypes","Subtypes","Centres"),valeurs=NULL,
                        vecrefs=NULL,varassoc=NULL,
                        codassoc=NULL,pres=NULL,langue="en",digits=2)
avt_greffe<-TABKRIS(baz=greffe,vect.var = c("previous_autoc","programme_autoalloc" ,"rechute_prem_greffec" 
                    ),
                    vect.quali = c(1,1,1,1),
                    varint=NULL,valvarint = NULL,
                    nomvarint = NULL,
                    test=NULL,
                    vecnoms=c("Previous auto"," Programme auto allo","First graft relapse"),valeurs=NULL,
                    vecrefs=NULL,varassoc=NULL,
                    codassoc=NULL,pres=NULL,langue="en",digits=2)


greffed<-TABKRIS(baz=greffe,vect.var = c("age_greffe","age_greffec","age_donor","sex_donor",
                                         "delai_dia_allo",
                                        "delai_dia_alloc","stade_dia",
                                        "disease_status_at_transplantc2",
                                        "disease_status_at_transplantc",
                                        "disease_status_at_transplant","karnofsky_greffe",
                                        "karnofsky_greffec", "nbr_lignes_avt_alloc",
                                        "nbr_lignes_avt_alloc2",
                                        "donnor","hla_matchc","hla_match",
                                        "sex_dp3","cmv_dp2","stem_cell_source","tbi","intensite_condi",
                                        "condit_details","manipu_cells","nbr_donneurc"),
                vect.quali = c(0,1,0,1,0,1,1,1,1,1,0,1,0,1,1,1,1,1,1,1,1,1,1,1,1),
                varint=NULL,valvarint = NULL,
                nomvarint =NULL,
                test=NULL,
                vecnoms=c("Age at graft","Age at graft","Donor age","Donor sex","Delay diagnosis and allo SCT",">12 months delay","Stage at diagnostic",
                          "Disease status at transplant","Disease status at transplant","Disease status at transplant",
                          "Karnofsky","Karnofsky","No of lines before alloSCT",
                          "No of lines before alloSCT",
                          "Donnor related","HLA match","HLA match",
                          "sex of patient/donnor","CMV serostatus of patient/donnor",
                          "Source of stem cells","TBI","conditioning Intensity","Conditioning","Cells manipulation","No of donnors"),valeurs=NULL,
                vecrefs=NULL,varassoc=NULL,
                codassoc=NULL,pres=NULL,langue="en",digits=2)


post_greffe<-TABKRIS(baz=greffe,vect.var = c(  "agvhd","agvhd_grade","cgvhd",
                                              "cgvhd_grade","prise_greffe","cause_death_c","best_response_after_allo",
                                         "relapse_progression_transplant_c","survival_status_FU"),
                                              vect.quali = c(rep(1,7),1,1),
                                              varint=NULL,valvarint = NULL,
                                              nomvarint = NULL,
                                              test=NULL,
                                              vecnoms=c("Agvhd","Agvhd grade","Cgvhd",
                                                        "Cgvhd grade",'Engrafted',"Cause of death","Best reponse after SCT","Relapse/progression","Death"),valeurs=NULL,
                                              vecrefs=NULL,varassoc=NULL,
                                              codassoc=NULL,pres=NULL,langue="en",digits=2)



### Bivariés anapath

# patient_greffe<-TABKRIS(baz=greffe,vect.var = c("age_dia","sex_patient"),
#                         vect.quali = c(0,1),
#                         varint="anapathc2",valvarint = c("NOS","AITL", "ALCL", "ATLL","NK/T nasal","Others"),
#                         nomvarint = "Subtypes",
#                         test=c("aov","fisher"),
#                         vecnoms=c("Age at diagnostic","Patient sex"),valeurs=NULL,
#                         vecrefs=NULL,varassoc=NULL,
#                         codassoc=NULL,pres=NULL,langue="en",digits=2)
# 
# 
# 
bivarie_anapath_greffe<-TABKRIS(baz=greffe,vect.var = c("age_greffe","age_greffec","age_donor","sex_donor",
                                                        "delai_dia_alloc","stade_dia","stade_diac",
                                                        "disease_status_at_transplantc2",
                                                        "disease_status_at_transplantc",
                                                        "disease_status_at_transplant",
                                "karnofsky_greffec","karnofsky_greffec2", "previous_autoc",
                                "programme_autoalloc","rechute_prem_greffec",
                                "nbr_lignes_avt_alloc2",
                                "donnor","hla_matchc","hla_match",
                                "sex_dp3","cmv_dp2","stem_cell_source","tbi"),
        vect.quali = c(0,1,0,rep(1,20)),
        varint="anapathc2",valvarint = c("NOS","AITL", "ALCL", "ATLL","NK/T nasal","Others"),
        nomvarint = "Subtypes",
        test=c("aov","chisq","aov","fisher",
               "chisq","","chisq",
               "",
               "",
               "",
               "","fisher","fisher",
               "fisher","",
               "",
               "chisq","fisher","",
               "","chisq","",""),
        vecnoms=c("Age at graft","Age at graft","Donor age","Donor sex",">12 months delay","Stage at diagnostic",
                  "Stage at diagnostic","Disease status at transplant","Disease status at transplant","Disease status at transplant",
                  "Karnofsky","Karnofsky","previous autoSCT","program autoallo","relapse first graft",
                  "No of lines before alloSCT",
                  "Donnor related","HLA match","HLA match",
                  "sex of p/d","CMV serostatus of p/d",
                  "Source of stem cells","TBI"),valeurs=NULL,
        vecrefs=NULL,varassoc=NULL,
        codassoc=NULL,pres=NULL,langue="en",digits=2)
# 
# 
# #write.csv2(bivarie_anapath_greffe,file="C:/Users/adupont/Documents/projetstlouis/resultats/bivarie_greffe_anapath.csv")
# 
# 
# bivarie_anapath_greffe2<-TABKRIS(baz=greffe,vect.var = c( "intensite_condi","condit_details","manipu_cells","nbr_donneurc",
#                                                         "agvhd","agvhd_grade","cgvhd",
#                                                         "cgvhd_grade","prise_greffe","cause_death_c","best_response_after_allo",
#                                                         "relapse_progression_transplant_c"),
#                                 vect.quali = c(rep(1,12)),
#                                 varint="anapathc2",valvarint = c("NOS","AITL", "ALCL", "ATLL","NK/T nasal","Others"),
#                                 nomvarint = "Subtypes",
#                                 test=c( "","","",
#                                         "fisher","chisq",""
#                                         ,"chisq","","","",
#                                         "")
#                                 
#                                 ,
#                                 vecnoms=c("conditioning intensity","conditioning","cells manipulation",
#                                           "no of donnors","agvhd","agvhd grade","cgvhd",
#                                           "cgvhd grade",'Engrafted',"Cause of death","best reponse after SCT","Relapse/progression"),valeurs=NULL,
#                                 vecrefs=NULL,varassoc=NULL,
#                                 codassoc=NULL,pres=NULL,langue="en",digits=2)
# 

#write.csv2(bivarie_anapath_greffe,file="C:/Users/adupont/Documents/projetstlouis/resultats/bivarie_greffe_anapath2.csv")

c("aov","aov","fisher",
  "chisq","","chisq",
  "",
  "",
  "",
  "","fisher","fisher",
  "fisher","",
  "",
  "fisher","chisq","fisher",
  "","chisq","fisher","",
  "chisq","","","",
  "","chisq",""
  ,"","","","",
  "")



c( "fisher","","",
   "fisher","chisq",""
   ,"fisher","","","",
   "")