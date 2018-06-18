################  Liste des patients qui ont une maputation avant à exclure des analyses 
################


patientvireramputations<-berger$nip[difftime(berger$date_diag,berger$date_amput1)>0|
                                      difftime(berger$date_diag,berger$date_amput2)>0|
                                      difftime(berger$date_diag,berger$date_amput3)>0 
                                      ]
