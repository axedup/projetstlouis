
###############################################################################################################
###############################################################################################################
format.pv <- function(p, text=F)
##
## arrondir la p-value pour la construction des tableaux de résultats
##
{
 if(p<0.0001) return("<0.0001")
 if(p>=0.0001&p<0.00095) ifelse(text==F,return(sprintf("%.4f", p)),return(paste("=",sprintf("%.4f", p),sep="")))
 if(p>=0.00095&p<=0.0095) ifelse(text==F,return(as.character(signif(p,1))),return(paste("=",as.character(signif(p,1)),sep="")))
 if(p>0.0095&p<0.0995) ifelse(text==F,return(sprintf("%.3f", signif(p,2))),return(paste("=",sprintf("%.3f", signif(p,2)),sep="")))
 if(p>=0.0995) ifelse(text==F,return(sprintf("%.2f", signif(p,2))),return(paste("=",sprintf("%.2f", signif(p,2)),sep="")))
}
###############################################################################################################
###############################################################################################################
format.hr <- function(z)
##
## arrondir la valeur du hr pour la construction des tableaux de résultats
##
{
  if(z<0.05) return(sprintf("%.3f",z))
  if(z<=9.95&z>=0.05) return(sprintf("%.2f",z))
  if(z>9.95) return(sprintf("%.1f",z))
}
###############################################################################################################
###############################################################################################################
or.calc <- function(obj, alpha=0.05)
##
## calculer un OR à partir d'un objet (résultat d'un certain modèle)
##
{
    se <- sqrt(diag(obj$var))
    res <- data.frame(OR=exp(obj$coef), lower=exp(obj$coef-qnorm(1-alpha/2)*se), upper=exp(obj$coef+qnorm(1-alpha/2)*se), p.value=(1-pchisq((obj$coef/se)^2,1)))
    return(res)
}
###############################################################################################################
###############################################################################################################

###############################################################################################################
###############################################################################################################
selectNOMS <- function(fichier)
##
## sélectionner par un menu les noms des variables que l'on souhaite décrire
## fichier : nom de la table R 
{
hop <- select.list(names(fichier),multiple=1)
print(paste("c(\'",paste(hop,collapse="\',\'"),"\')",sep=""))
return(hop)
}
###############################################################################################################
###############################################################################################################
selectQUALI <- function(vect.var)
##
## sélectionner par un menu les types (quali ou quanti) des variables que l'on souhaite décrire
## vect.var  : vecteur des noms de variables concernés 
{
j<-1
vect.quali<-rep(0,length(vect.var))
for (i in vect.var)
{
vect.quali[j]<-menu(c("quantitatif","qualitatif"),TRUE,title=i)-1
j<-j+1
}
print(paste("c(",paste(vect.quali,collapse=","),")",sep=""))
return(vect.quali)
}
###############################################################################################################
###############################################################################################################
selectTESTS <- function(vect.var)
##
## sélectionner par un menu les tests à effectuer sur les variables que l'on souhaite décrire
## vect.var  : vecteur des noms de variables concernés 
{
j<-1
test<-rep("",length(vect.var))
for (i in vect.var)
{
if(vect.var[j]==1)
{test[j]<-menu(c("fisher","chisq","mcnemar","pas de test"),TRUE,title=i)}
else
{test[j]<-menu(c("t","wilcox","aov","kruskal","pas de test"),TRUE,title=i)+4}
j<-j+1
}
test[test=="1"]<-"fisher"
test[test=="2"]<-"chisq"
test[test=="3"]<-"mcnemar"
test[test=="4"]<-""
test[test=="5"]<-"t"
test[test=="6"]<-"wilcox"
test[test=="7"]<-"aov"
test[test=="8"]<-"kruskal"
test[test=="9"]<-""
print(paste("c(\'",paste(test,collapse="\',\'"),"\')",sep=""))
return(test)
}

###############################################################################################################
###############################################################################################################
##
## création du tableau d'un modèle de cox multivarié
## 
result.cox <- function (modele)
{
  summary(modele)$conf.int ->  modele.detail
  res <- data.frame (Variable = names(modele$coef))
  res$HR <- round(modele.detail[,1],2)
  res$IC <- paste("[" , round(modele.detail[,3],2) , " - " , round(modele.detail[,4],2) ,"]",sep="")
  for (j in 1:length(res$IC))
  {
    res$pval[j] <- as.character(format.pv(summary(modele)$coef[j,5]))
  }
  res$Variable<- gsub("greffe\\[, i\\]","",res$Variable)
  titre<-data.frame(Variable=i,HR=NA,IC=NA,pval=NA)
  res<-rbind(titre,res)
  return(res)
}

###############################################################################################################
###############################################################################################################
###############################################################################################################
###############################################################################################################

tab.cont<- function(toto,varint,cont)
##
## descriptif variables continues selon variable stratification (varint)
## toto   : nom de table R
## varint : nom de la variable de stratification entre ""
## cont   : liste des noms de variables entre "" séparées par des virgules
##
{
noms<- names(toto)
nomscl<-  noms
varintind<-match(varint,noms)
nlev<-nlevels(as.factor(paste(toto[,varintind])))
lev<-levels(as.factor(toto[,varintind]))
lev<- lev[lev !=""]
ind<-match(cont,noms)
cont.2<-nomscl[ind]
descont.fac<-matrix(nrow=(length(ind)+1),ncol=(1+2*nlev))
descont.fac<-as.data.frame(descont.fac)
names(descont.fac)<-c("Parameter",rep(c("N","Median [Q1-Q3]"),nlev))
for (i in 1:length(ind))
{
descont.fac[i+1,1]<-cont.2[i]
}
descont.fac[1,1]<-""
for (j in 1:nlev)
{
descont.fac[1,(3+2*(j-1))]<-paste(nomscl[varintind],"=",lev[j])
#descont.fac[1,(2+4*(j-1))]<-""
#descont.fac[1,(4+4*(j-1))]<-""
#descont.fac[1,(2+4*(j-1))]<-""
}

for (j in 1:nlev)
{
toto1<-toto[as.factor(toto[,varintind])==lev[j],]
ll1<-dim(toto1)[1]
descont.fac[1,(2+2*(j-1))]<-ll1
for (i in 1:length(ind))
{
xx<-summary(toto1[,ind[i]])
descont.fac[i+1,(2+2*(j-1))]<-ll1-sum(is.na(toto1[,ind[i]]))
#descont.fac[i+1,(3+4*(j-1))]<-paste(round(xx[4],2)," +/-",round(sd(toto1[,ind[i]],na.rm=1),2),sep="")
#descont.fac[i+1,(4+4*(j-1))]<-paste(xx[1],";",xx[6],sep="")
descont.fac[i+1,(3+2*(j-1))]<-paste(round(xx[3],2)," [",round(xx[2],2),"-",round(xx[5],2),"]",sep="")
}
}

return(descont.fac)
}

###############################################################################################################
###############################################################################################################
###############################################################################################################
###############################################################################################################

tab.qual <- function(toto,varint,qualit)
##
## descriptif variables qualitatives selon variable stratification (varint)
## toto   : nom de table R
## varint : nom de la variable de stratification entre ""
## qual   : liste des noms de variables entre "" séparées par des virgules
##
{
noms<- names(toto)
nomscl<- noms
varintind<-match(varint,noms)
nlev<-nlevels(as.factor(paste(toto[,varintind])))
lev<-levels(as.factor(toto[,varintind]))
ind<-match(qualit,noms)
qualit.2<-nomscl[ind]
desqualit.fac<-matrix(nrow=1,ncol=4)
desqualit.fac<-as.data.frame(desqualit.fac)
desqualit.fac[1,1:4]<-c("","",paste(nomscl[varintind],"=",lev[1]),"")
temp_lev<-list(NULL)

for (i in 1:length(ind))
{
#toto[,ind[i]]<-as.factor(toto[,ind[i]])
tttemp<-factor(toto[,ind[i]],exclude=NULL)
#tttemp<-addNA(tttemp)
temp_lev[[i]]<-levels(tttemp)
}

toto1<-toto[as.factor(toto[,varintind])==lev[1],]
#print(dim(toto1))
for (i in 1:length(ind))
{
xx<-summary(factor(toto1[,ind[i]],levels=temp_lev[[i]],exclude=NULL))
nona<-sum(1-is.na(toto1[,ind[i]]))
nn<-names(xx)
l<-length(nn)
#print(xx)
#print(sum(xx))
#print(nona)
##if (nn[l]=="NA's")
#{l<-l-1}
yy<-matrix(nrow=l,ncol=4,data="")
yy[1,1]<-qualit.2[i]
for (j in 1:l)
{
yy[j,2]<-nn[j]
if(is.na(nn[j]))
{
yy[j,2]<-"NA"
}
yy[j,3]<-xx[j]
if(!is.na(nn[j]))
{
if(!is.na(xx[j]/nona))
{
yy[j,4]<-paste(round(xx[j]/nona,3)*100,"\\%",sep="")
}
}
}
desqualit.fac<-rbind(desqualit.fac,yy)
yy
}

for (j in 2:nlev)
{
toto1<-toto[as.factor(toto[,varintind])==lev[j],]
desqualit.fac1<-matrix(nrow=1,ncol=4)
desqualit.fac1<-as.data.frame(desqualit.fac1)
desqualit.fac1[1,1:4]<-c("","",paste(nomscl[varintind],"=",lev[j]),"")

for (i in 1:length(ind))
{
xx<-summary(factor(toto1[,ind[i]],levels=temp_lev[[i]],exclude=NULL))
nona<-sum(1-is.na(toto1[,ind[i]]))
nn<-names(xx)
l<-length(nn)
#if (nn[l]=="NA's")
#{l<-l-1}
yy<-matrix(nrow=l,ncol=4,data="")
yy[1,1]<-qualit.2[i]
for (j in 1:l)
{
yy[j,2]<-nn[j]
if(is.na(nn[j]))
{
yy[j,2]<-"NA"
}
yy[j,3]<-xx[j]
if(!is.na(nn[j]))
{
if(!is.na(xx[j]/nona))
{
yy[j,4]<-paste(round(xx[j]/nona,3)*100,"\\%",sep="")
}
}
}
desqualit.fac1<-rbind(desqualit.fac1,yy)
yy
}
desqualit.fac<-cbind(desqualit.fac,desqualit.fac1[,c(-1,-2)])
}

names(desqualit.fac)<-c("parametre","valeurs",rep(c("n","\\%"),nlev))
return(desqualit.fac)
}

###############################################################################################################
###############################################################################################################
###############################################################################################################
###############################################################################################################
###############################################################################################################
#Tableau global (quanti+quali) avec ou sans variable d'intérêt
#plusieurs options disponibles ici
TABKRIS <- function(baz,vect.var,vect.quali,varint=NULL,valvarint=NULL,nomvarint=NULL,
test=NULL,vecnoms=NULL,valeurs=NULL,vecrefs=NULL,varassoc=NULL,codassoc=NULL,pres=NULL,langue="fr",digits=2)
##
## descriptif variables qualitatives+quantitatives
## baz   : nom de table R
## vect.var   : liste des noms de variables entre "" séparées par des virgules
## vect.quali   : liste des types de variables séparées par des virgules
##                      type=1 si     variable qualitative
##                      type=0 sinon  (variable quantitative)
## varint : nom de la variable de stratification entre ""
## valvarint : liste des valeurs que l'on souhaite afficher dans le tableau concernant la variable d'intérêt
## nomvarint : nom de la variable d'intérêt que l'on souhaite utiliser pour l'affichage 
##              (bras de randomisation, sachant qu'on utilise ensuite valvarint pour nommer les groupes de randomisation)
## test : liste des tests que l'on souhaite effectuer entre ""  séparés par des virgules
##                       variables quali :  "fisher", "chisq" ou "mcnemar"
##                       variables quanti : "t", "wilcox","aov","kruskal"
## !!! on peut décider d'effectuer uniquement certains tests il faut alors songer à mettre "" pour les tests que l'on ne souhaite pas réaliser
## vecnoms : liste des noms que l'on souhaite utiliser pour la sortie tableau, entre "" séparés par des virgules
## valeurs (utiles pour les quali et les quanti) :
## liste des valeurs que l'on souhaite utiliser pour la sortie tableau, entre "" séparées par des virgules
##                      exemple : si la variable est codée 1/2 on veut afficher Masculin/Féminin
##                      permet aussi d'afficher les unités pour les avriables quantitatives
## vecrefs (utiles pour les quali) : liste des références que l'on souhaite utiliser pour la sortie tableau
##                      exemple "Masculin" pour le sexe
## !!!! de bien définir une référence qui existe dans le vecteur des valeurs de la variable
## !!!! il faut songer à définir une référence vide pour les variables quantitatives
## varassoc : liste des variables "mères" qui impliquent des réponses imbriquées 
##             exemple : TABAC (oui/non) est une variable de ce type pour la variable type de tabagisme (sevré/actif)
##              mettre "" si pas de variable "mère" pour la variable de vect.var
## codassoc : code qui devait conduire à la réponse d'une variable dite imbriquée
##              exemple : codassoc pour TABAC == "Oui"
## pres : liste des précisions que l'on souhaite utiliser pour les variables quantitatives
##                     "med" (median [Q1;Q3]), "med_mm" (median [Q1;Q3] (min;max)),"mean" (mean (sd)), "mean_mm" (mean(min;max))
## !!!! le vecteur entré ici doit avoir la taille du vecteur de variables vect.var
## !!!! il faut donc penser à mettre "" pour les variables qualitatives
## langue : permet de choisir la langue de sortie du tableau
##                    "fr" = FRANCAIS; "an" ou "en" = ANGLAIS
##
## utilise les fonctions qualitbis et quantitbis
##
####NB : on peut entrer une variable d'intérêt qui contiendrait des NA
#### seulement dans le tableau de sortie on ne retrouve pas les infos de cette colonne ...
{
print("Veillez à bien vérifier que les valeurs que vous définissez pour vos variables qualitatives sont bien représentées")
if (!is.null(varassoc))
{
  print("Vous avez annoncé des variables dites -mères-, pensez à vérifier au préalable la cohérence de vos imbrications, si des éléments sont discordants cela risque de faire planter la fonction TABKRIS")
  print("par exemple, si vous avez variable-mère- = non Remplie ou Non mais variable-imbriquée- remplie ...")  
}

if (!is.null(test) & is.null(varint))
{
  print("Vous tentez d'effectuer un test sur une population Single Arm.")
  return()
}
if (langue %in% c("fr","FR","Fr","fR"))
{
  langue="fr"
}
if (langue %in% c("en","EN","an","AN","An","En","eN","aN"))
{
  langue="en"
}
if (!(langue %in% c("fr","an","en")))
{
  cat("Vous avez choisi une langue que je ne connais pas !! \n Le français sera pris par défaut \n")
  langue="fr"
}

noms<-names(baz)
if (is.null(pres))
{
  pres<-rep("med",length(vect.var))
}
if (length(pres)==1)
{
  pres2<-rep(pres,length(vect.var))
  pres <- pres2
}
if (!is.null(vecrefs)) {vecrefs[vecrefs==""]<-NA}
if (!is.null(test)) {test[test==""]<-NA}
if (!is.null(pres)) {pres[pres==""]<-NA}
if (!is.null(valeurs)) {valeurs[valeurs==""]<-NA}
if (!is.null(varassoc)) {varassoc[varassoc==""]<-NA}
if (!is.null(codassoc)) {codassoc[codassoc==""]<-NA}

if (is.null(varassoc)) {varassoc<-rep(NA,length(vect.var))}

vecnoms[vecnoms==""]<-vect.var[vecnoms==""]

lgtest<-ifelse(is.null(test),0,1)

if (is.null(varint))
{
  effect <- dim(baz)[1]
  i <- 1
  if (is.na(varassoc[i]))
  {
      if (vect.quali[i]==1)
      {
        #digits <- 1
        levvar<-levels(as.factor(baz[,vect.var[i]]))
        if (!is.null(vecrefs))
        {
          if (is.null(valeurs))
          {
            if (!is.element(vecrefs[i],levvar) & !is.na(vecrefs[i]))
            {
               cat("Vous essayez de réaliser un tableau, avec une référence * ",vecrefs[i]," * que vous ne définissez pas dans le vecteur de valeurs possibles pour la variable")
               print(vect.var[i])
               return()
            } else {
                    if (is.na(vecrefs[i]))
                    {
                        ref<-NULL
                    } else {
                            ref <- vecrefs[i]
                            }
                    descript<-qualitbis(x=vect.var[i],baz=baz,ref=ref,nom=vecnoms[i],digits=digits)
                  }
          } else {
                if (!is.element(vecrefs[i],valeurs[i:length(levvar)]) & !is.na(vecrefs[i]))
                {
                   print(paste("Vous essayez de réaliser un tableau, avec une référence * ",vecrefs[i]," * que vous ne définissez pas dans le vecteur de valeurs possibles pour la variable"))
                   print(vect.var[i])
                   return()
                } else {
                        if (is.na(vecrefs[i]))
                        {
                            ref<-NULL
                        } else {
                                ref <- vecrefs[i]
                                }
                        descript<-qualitbis(x=vect.var[i],baz=baz,ref=ref,nom=vecnoms[i],valeurs=valeurs[i:length(levvar)],digits=digits)
                        }
                  }
        } else {
                descript<-qualitbis(x=vect.var[i],baz=baz,nom=vecnoms[i],valeurs=valeurs[i:length(levvar)],digits=digits)
                }
      } else {
              if (is.na(pres[i]))
              {
                presi="med"
              } else {
                        presi=pres[i]
                      }
              #digits=max(3, getOption("digits")-3)
              descript<-quantitbis(x=vect.var[i],baz=baz,nom=vecnoms[i],pres=presi,val=valeurs[i])
              #descript<-matrix(descript,ncol=4,byrow=F)
             }
  } else {
              indV <- match(noms,varassoc[i])
              Vassoc <- baz[,varassoc[i]]
              Cassoc <- codassoc[i]
              soustable <- baz[Vassoc==as.character(Cassoc) &  is.na(Vassoc)==F,]
              if (vect.quali[i]==1)
              {
                  levvar<-levels(as.factor(soustable[,vect.var[i]]))
                  if (!is.null(vecrefs))
                  {
                    if (is.null(valeurs))
                    {
                      if (!is.element(vecrefs[i],levvar) & !is.na(vecrefs[i]))
                      {
                         cat("Vous essayez de réaliser un tableau, avec une référence * ",vecrefs[i]," * que vous ne définissez pas dans le vecteur de valeurs possibles pour la variable")
                         print(vect.var[i])
                         return()
                      } else {
                              if (is.na(vecrefs[i]))
                              {
                                  ref<-NULL
                              } else {
                                      ref <- vecrefs[i]
                                      }
                              descript<-qualitbis(x=vect.var[i],digits=digits,baz=soustable,ref=ref,nom=vecnoms[i])
                            }
                    } else {
                          if (!is.element(vecrefs[i],valeurs[i:length(levvar)]) & !is.na(vecrefs[i]))
                          {
                             print(paste("Vous essayez de réaliser un tableau, avec une référence * ",vecrefs[i]," * que vous ne définissez pas dans le vecteur de valeurs possibles pour la variable"))
                             print(vect.var[i])
                             return()
                          } else {
                                  if (is.na(vecrefs[i]))
                                  {
                                      ref<-NULL
                                  } else {
                                          ref <- vecrefs[i]
                                          }
                                  descript<-qualitbis(x=vect.var[i],digits=digits,baz=soustable,ref=ref,nom=vecnoms[i],valeurs=valeurs[i:length(levvar)])
                                  }
                            }
                  } else {
                          descript<-qualitbis(x=vect.var[i],digits=digits,baz=soustable,nom=vecnoms[i],valeurs=valeurs[i:length(levvar)])
                          }
                } else {
                        if (is.na(pres[i]))
                        {
                          presi="med"
                        } else {
                                  presi=pres[i]
                                }
                        descript<-quantitbis(x=vect.var[i],baz=soustable,nom=vecnoms[i],pres=presi,val=valeurs[i])
                       }
          }
  #descript<-matrix(descript,ncol=4,byrow=F)
  if (length(vect.var)>=2)
  {
    for (i in 2:length(vect.var))
    {
        deb=0
        for (j in 1:(i-1))
        {
            if (vect.quali[j]==1)
            {
              deb=deb+length(levels(as.factor(baz[,vect.var[j]])))
            } else {
                    deb=deb+1
                    }
        }
        deb=deb+1
        if (is.na(varassoc[i]))
        {
            if (vect.quali[i]==1)
              {
                levvar<-levels(as.factor(baz[,vect.var[i]]))
      
                #digits=1
                if (!is.null(vecrefs))
                {
                  if (is.null(valeurs))
                  {
                    if (!is.element(vecrefs[i],levvar) & !is.na(vecrefs[i]))
                    {
                       print(paste("Vous essayez de réaliser un tableau, avec une référence * ",vecrefs[i]," * que vous ne définissez pas dans le vecteur de valeurs possibles pour la variable"))
                       print(vect.var[i])
                       return()
                    } else {
                            if(is.na(vecrefs[i])) {ref<-NULL} else {ref<-vecrefs[i]}
                            descript<-rbind(descript,qualitbis(x=vect.var[i],digits=digits,baz=baz,ref=ref,nom=vecnoms[i]))
                            }
                  } else {
                        if (!is.element(vecrefs[i],valeurs[deb:(deb+length(levvar)-1)]) & !is.na(vecrefs[i]))
                        {
      
                          print(paste("Vous essayez de réaliser un tableau, avec une référence * ",vecrefs[i]," * que vous ne définissez pas dans le vecteur de valeurs possibles pour la variable"))
                           print(vect.var[i])
                           return()
                        } else {
                                if(is.na(vecrefs[i])) {ref<-NULL} else {ref<-vecrefs[i]}
                                descript<-rbind(descript,qualitbis(x=vect.var[i],digits=digits,baz=baz,ref=ref,nom=vecnoms[i],valeurs=valeurs[deb:(deb+length(levvar)-1)]))
                                }
                        }
                } else {
                        descript<-rbind(descript,qualitbis(x=vect.var[i],digits=digits,baz=baz,nom=vecnoms[i],valeurs=valeurs[deb:(deb+length(levvar)-1)]))
                        }
              } else {
                      if (is.na(pres[i]))
                      {
                        presi="med"
                      } else {
                                presi=pres[i]
                              }
                      #digits=max(3, getOption("digits")-3)
                      descript<-rbind(descript,quantitbis(x=vect.var[i],baz=baz,nom=vecnoms[i],pres=presi,val=valeurs[deb]))
                      }
                      
          } else {
                      indV <- match(noms,varassoc[i])
                      Vassoc <- baz[,varassoc[i]]
                      Cassoc <- codassoc[i]
                      soustable <- baz[Vassoc==as.character(Cassoc) &  is.na(Vassoc)==F,]
                      if (vect.quali[i]==1)
                      {
                          levvar<-levels(as.factor(soustable[,vect.var[i]]))
      
                          if (!is.null(vecrefs))
                          {
                              if (is.null(valeurs))
                              {
                                  if (!is.element(vecrefs[i],levvar) & !is.na(vecrefs[i]))
                                  {
                                       print(paste("Vous essayez de réaliser un tableau, avec une référence * ",vecrefs[i]," * que vous ne définissez pas dans le vecteur de valeurs possibles pour la variable"))
                                       print(vect.var[i])
                                       return()
                                  } else {
                                              if(is.na(vecrefs[i])) {ref<-NULL} else {ref<-vecrefs[i]}
                                              descript<-rbind(descript,qualitbis(x=vect.var[i],digits=digits,baz=soustable,ref=ref,nom=vecnoms[i]))
                                          }
                                  } else {
                                                if (!is.element(vecrefs[i],valeurs[deb:(deb+length(levvar)-1)]) & !is.na(vecrefs[i]))
                                                {
                                                        print(paste("Vous essayez de réaliser un tableau, avec une référence * ",vecrefs[i]," * que vous ne définissez pas dans le vecteur de valeurs possibles pour la variable"))
                                                       print(vect.var[i])
                                                       return()
                                                } else {
                                                              if(is.na(vecrefs[i])) {ref<-NULL} else {ref<-vecrefs[i]}
                                                              descript<-rbind(descript,qualitbis(x=vect.var[i],digits=digits,baz=soustable,ref=ref,nom=vecnoms[i],valeurs=valeurs[deb:(deb+length(levvar)-1)]))
                                                        }
                                          }
                            } else {
                                      descript<-rbind(descript,qualitbis(x=vect.var[i],digits=digits,baz=soustable,nom=vecnoms[i],valeurs=valeurs[deb:(deb+length(levvar)-1)]))
                                    }
                        } else {
                                    if (is.na(pres[i]))
                                    {
                                      presi="med"
                                    } else {
                                              presi=pres[i]
                                            }
                                    #digits=max(3, getOption("digits")-3)
                                    descript<-rbind(descript,quantitbis(x=vect.var[i],baz=soustable,nom=vecnoms[i],pres=presi,val=valeurs[deb]))
                              }
                    }
      } #fin du for
    #descript<-matrix(descript,ncol=4,byrow=F)
    descript<-rbind(c("","",effect,""),descript)
    descript<-suppressWarnings(data.frame(descript,stringsAsFactors=FALSE))
    if (langue=="fr")
    {
      names(descript) <- c("Paramètres","Valeurs",c("N","Statistiques*"))
      #names(descript)<-descript[1,]
      #descript[1,] <- c("Paramètres","Valeurs",c("N","Statistiques*"))
    }
    if (langue=="en")
    {
      names(descript) <- c("Parameters","Values",c("N","Statistics*"))
      #names(descript)<-descript[1,]
      #descript[1,] <- c("Parameters","Values",c("N","Statistics*"))
    }

  } else {
            descript<-rbind(c("","",effect,""),descript)
            descript<-suppressWarnings(data.frame(descript,stringsAsFactors=FALSE))
            if (langue=="fr")
            {
              names(descript) <- c("Paramètres","Valeurs",c("N","Statistiques*"))
              #names(descript)<-descript[1,]
              #descript[1,] <- c("Paramètres","Valeurs",c("N","Statistiques*"))
            }
            if (langue=="en")
            {
              names(descript) <- c("Parameters","Values",c("N","Statistics*"))
              #names(descript)<-descript[1,]
              #descript[1,] <- c("Parameters","Values",c("N","Statistics*"))
            }
          }

return(descript)
}

########################
### avec variable d'intérêt
if (!is.null(varint))
{
  varintind<-match(varint,noms)
  nlev<-length(levels(as.factor(baz[,varintind])))
  lev<-levels(as.factor(baz[,varintind]))
  lev<- lev[lev !=""]
  effect <-table(baz[,varintind])
  i=1
  if (lgtest==1)
  {
    if (is.na(test[i]))
    {
      testi<-NULL
    } else {
            testi <- test[i]
            }
  } else{
        testi<-NULL
        }
  if (is.na(varassoc[i]))
  {
      if (vect.quali[i]==1)
      {
        #digits=1
        levvar<-levels(as.factor(baz[,vect.var[i]]))
        if (!is.null(vecrefs))
        {
          if (is.null(valeurs))
          {
            if (!is.element(vecrefs[i],levvar) & !is.na(vecrefs[i]))
            {
               print(paste("Vous essayez de réaliser un tableau, avec une référence * ",vecrefs[i]," * que vous ne définissez pas dans le vecteur de valeurs possibles pour la variable"))
               print(vect.var[i])
               return()
            } else {
                    if (is.na(vecrefs[i]))
                    {
                      ref<-NULL
                    } else {
                            ref <- vecrefs[i]
                            }
                    descript<-qualitbis(x=vect.var[i],digits=digits,y=varint,baz=baz,ref=ref,test=testi,nom=vecnoms[i])
                    if (lgtest==1 & is.null(testi))
                    {
                      if (class(descript)=="matrix")
                      {
                      descript<-cbind(descript,rep("",dim(descript)[1]))
                      } else {
                                descript<-cbind(descript,rep("",length(descript)))
                              }
                    }
                    }
          } else {
                  if (!is.element(vecrefs[i],valeurs[i:length(levvar)]) & !is.na(vecrefs[i]))
                  {
                   print(paste("Vous essayez de réaliser un tableau, avec une référence * ",vecrefs[i]," * que vous ne définissez pas dans le vecteur de valeurs possibles pour la variable"))
                   print(vect.var[i])
                   return()
                  } else {
                            if (is.na(vecrefs[i]))
                            {
                                ref<-NULL
                            } else {
                                    ref <- vecrefs[i]
                                    }
                            descript<-qualitbis(x=vect.var[i],digits=digits,y=varint,baz=baz,ref=ref,test=testi,nom=vecnoms[i],valeurs=valeurs[i:length(levvar)])
                            if (lgtest==1 & is.null(testi))
                            {
                                 if (class(descript)=="matrix")
                                  {
                                    descript<-cbind(descript,rep("",dim(descript)[1]))
                                  } else {
                                            descript<-cbind(descript,rep("",length(descript)))
                                          }
                            }
                          }
                  }
        } else {
                descript<-qualitbis(x=vect.var[i],digits=digits,y=varint,baz=baz,test=testi,nom=vecnoms[i],valeurs=valeurs[i:length(levvar)])
                if (lgtest==1 & is.null(testi))
                {
                  if (class(descript)=="matrix")
                  {
                  descript<-cbind(descript,rep("",dim(descript)[1]))
                  } else {
                            descript<-cbind(descript,rep("",length(descript)))
                          }
                }
                }
      } else {
                  if (is.na(pres[i]))
                  {
                    presi="med"
                  } else {
                            presi=pres[i]
                          }
                  #digits=max(3, getOption("digits")-3)
                  descript<-quantitbis(x=vect.var[i],y=varint,baz=baz,test=testi,nom=vecnoms[i],pres=presi,val=valeurs[i])
                  #descript<-matrix(descript,ncol=2+2*nlev+lgtest,byrow=F)
                  if (lgtest==1 & is.null(testi))
                  {
                    descript<-c(descript,"")
                  }
              }
  } else {
              indV <- match(noms,varassoc[i])
              Vassoc <- baz[,varassoc[i]]
              Cassoc <- codassoc[i]
              soustable <- baz[Vassoc==as.character(Cassoc) &  is.na(Vassoc)==F,]
              if (vect.quali[i]==1)
              {
              #digits=1
                  levvar<-levels(as.factor(soustable[,vect.var[i]]))
                  if (!is.null(vecrefs))
                  {
                    if (is.null(valeurs))
                    {
                      if (!is.element(vecrefs[i],levvar) & !is.na(vecrefs[i]))
                      {
                         print(paste("Vous essayez de réaliser un tableau, avec une référence * ",vecrefs[i]," * que vous ne définissez pas dans le vecteur de valeurs possibles pour la variable"))
                         print(vect.var[i])
                         return()
                      } else {
                                  if (is.na(vecrefs[i]))
                                  {
                                    ref<-NULL
                                  } else {
                                          ref <- vecrefs[i]
                                          }
                                  descript<-qualitbis(x=vect.var[i],digits=digits,y=varint,baz=soustable,ref=ref,test=testi,nom=vecnoms[i])
                                  if (lgtest==1 & is.null(testi))
                                  {
                                    if (class(descript)=="matrix")
                                    {
                                    descript<-cbind(descript,rep("",dim(descript)[1]))
                                    } else {
                                              descript<-cbind(descript,rep("",length(descript)))
                                            }
                                  }
                              }
                    } else {
                                if (!is.element(vecrefs[i],valeurs[i:length(levvar)]) & !is.na(vecrefs[i]))
                                {
                                 print(paste("Vous essayez de réaliser un tableau, avec une référence * ",vecrefs[i]," * que vous ne définissez pas dans le vecteur de valeurs possibles pour la variable"))
                                 print(vect.var[i])
                                 return()
                                } else {
                                          if (is.na(vecrefs[i]))
                                          {
                                              ref<-NULL
                                          } else {
                                                  ref <- vecrefs[i]
                                                  }
                                          descript<-qualitbis(x=vect.var[i],digits=digits,y=varint,baz=soustable,ref=ref,test=testi,nom=vecnoms[i],valeurs=valeurs[i:length(levvar)])
                                          if (lgtest==1 & is.null(testi))
                                          {
                                               if (class(descript)=="matrix")
                                                {
                                                  descript<-cbind(descript,rep("",dim(descript)[1]))
                                                } else {
                                                          descript<-cbind(descript,rep("",length(descript)))
                                                        }
                                          }
                                        }
                            }
                  } else {
                            descript<-qualitbis(x=vect.var[i],digits=digits,y=varint,baz=soustable,test=testi,nom=vecnoms[i],valeurs=valeurs[i:length(levvar)])
                            if (lgtest==1 & is.null(testi))
                            {
                              if (class(descript)=="matrix")
                              {
                              descript<-cbind(descript,rep("",dim(descript)[1]))
                              } else {
                                        descript<-cbind(descript,rep("",length(descript)))
                                      }
                            }
                      }
                } else {
                            if (is.na(pres[i]))
                            {
                              presi="med"
                            } else {
                                      presi=pres[i]
                                    }
                            #digits=max(3, getOption("digits")-3)
                            descript<-quantitbis(x=vect.var[i],y=varint,baz=soustable,test=testi,nom=vecnoms[i],pres=presi,val=valeurs[i])
                            #descript<-matrix(descript,ncol=2+2*nlev+lgtest,byrow=F)
                            if (lgtest==1 & is.null(testi))
                            {
                              descript<-c(descript,"")
                            }
                        }
        }

  #descript<-matrix(descript,ncol=2+2*nlev+lgtest,byrow=F)
  if (length(vect.var)>=2)
  {
    for (i in 2:length(vect.var))
    {
        #print(vect.var[i])
        if (lgtest==1)
        {
            if (is.na(test[i]))
            {
              testi<-NULL
            } else {
                    testi <- test[i]
                    }
        }
        deb=0
        for (j in 1:(i-1))
        {
          if (vect.quali[j]==1)
          {
            deb=deb+length(levels(as.factor(baz[,vect.var[j]])))
          } else {
                deb=deb+1
                }
        }
        deb=deb+1
        if (is.na(varassoc[i]))
        {
            if (vect.quali[i]==1)
            {
                  levvar<-levels(as.factor(baz[,vect.var[i]]))
                  #digits=1
                  if (!is.null(vecrefs))
                  {
                    if (is.null(valeurs))
                    {
                      if (!is.element(vecrefs[i],levvar) & !is.na(vecrefs[i]))
                      {
                         print(paste("Vous essayez de réaliser un tableau, avec une référence * ",vecrefs[i]," * que vous ne définissez pas dans le vecteur de valeurs possibles pour la variable"))
                         print(vect.var[i])
                         return()
                      } else {
                               if (is.na(vecrefs[i]))
                                {
                                  ref<-NULL
                                } else {
                                        ref <- vecrefs[i]
                                        }
                              if (lgtest==1 & is.null(testi))
                              {
                                  tmp<- qualitbis(x=vect.var[i],digits=digits,y=varint,baz=baz,ref=ref,test=testi,nom=vecnoms[i])
                                  if (class(tmp)=="matrix")
                                  {
                                  tmp<-cbind(tmp,rep("",dim(tmp)[1]))
                                  } else {
                                            tmp<-c(tmp,"")
                                          }
                              } else {
                                        tmp<- qualitbis(x=vect.var[i],digits=digits,y=varint,baz=baz,ref=ref,test=testi,nom=vecnoms[i])
                                      }
                              descript<-rbind(descript,tmp)
                              }
                    } else {
                          if (!is.element(vecrefs[i],valeurs[deb:(deb+length(levvar)-1)]) & !is.na(vecrefs[i]))
                          {
                             print(paste("Vous essayez de réaliser un tableau, avec une référence * ",vecrefs[i]," * que vous ne définissez pas dans le vecteur de valeurs possibles pour la variable"))
                             print(vect.var[i])
                             return()
                          } else {
                                      if(is.na(vecrefs[i]))
                                      {
                                        ref<-NULL
                                      } else {
                                                ref<-vecrefs[i]
                                              }
                                      if (lgtest==1 & is.null(testi))
                                      {
                                          tmp<- qualitbis(x=vect.var[i],digits=digits,y=varint,baz=baz,ref=ref,test=testi,nom=vecnoms[i],valeurs=valeurs[deb:(deb+length(levvar)-1)])
                                          if (class(tmp)=="matrix")
                                          {
                                            tmp<-cbind(tmp,rep("",dim(tmp)[1]))
                                          } else {
                                                    tmp<-cbind(tmp,rep("",length(tmp)))
                                                  }
                                      } else {
                                              tmp<- qualitbis(x=vect.var[i],digits=digits,y=varint,baz=baz,ref=ref,test=testi,nom=vecnoms[i],valeurs=valeurs[deb:(deb+length(levvar)-1)])
                                              }
                                      descript<-rbind(descript,tmp)
                                  }
                          }
                  } else {
                              if (lgtest==1 & is.null(testi))
                              {
                                  tmp<- qualitbis(x=vect.var[i],digits=digits,y=varint,baz=baz,test=testi,nom=vecnoms[i],valeurs=valeurs[deb:(deb+length(levvar)-1)])
                                  if (class(tmp)=="matrix")
                                  {
                                    tmp<-cbind(tmp,rep("",dim(tmp)[1]))
                                  } else {
                                            tmp<-cbind(tmp,rep("",length(tmp)))
                                          }
                              } else {
                                        tmp<- qualitbis(x=vect.var[i],digits=digits,y=varint,baz=baz,test=testi,nom=vecnoms[i],valeurs=valeurs[deb:(deb+length(levvar)-1)])
                                      }
                              descript<-rbind(descript,tmp)
                          }
               } else {
                  #digits=max(3, getOption("digits")-3)
                      if (is.na(pres[i]))
                      {
                        presi="med"
                      } else {
                                presi=pres[i]
                              }

                      if (lgtest==1 & is.null(testi))
                      {
                          tmp<- quantitbis(x=vect.var[i],y=varint,baz=baz,test=testi,nom=vecnoms[i],pres=presi,val=valeurs[deb])
                          tmp<-c(tmp,"")

                      } else {
                                tmp<- quantitbis(x=vect.var[i],y=varint,baz=baz,test=testi,nom=vecnoms[i],pres=presi,val=valeurs[deb])
                              }

                      descript<-rbind(descript,tmp)
                       }
            } else {
                      indV <- match(noms,varassoc[i])
                      Vassoc <- baz[,varassoc[i]]
                      Cassoc <- codassoc[i]
                      soustable <- baz[Vassoc==as.character(Cassoc) &  is.na(Vassoc)==F,]
                      if (vect.quali[i]==1)
                      {
                          levvar<-levels(as.factor(soustable[,vect.var[i]]))
                          #digits=1
                          if (!is.null(vecrefs))
                          {
                            if (is.null(valeurs))
                            {
                              if (!is.element(vecrefs[i],levvar) & !is.na(vecrefs[i]))
                              {
                                 print(paste("Vous essayez de réaliser un tableau, avec une référence * ",vecrefs[i]," * que vous ne définissez pas dans le vecteur de valeurs possibles pour la variable"))
                                 print(vect.var[i])
                                 return()
                              } else {
                                       if (is.na(vecrefs[i]))
                                        {
                                          ref<-NULL
                                        } else {
                                                ref <- vecrefs[i]
                                                }
                                      if (lgtest==1 & is.null(testi))
                                      {
                                          tmp<- qualitbis(x=vect.var[i],digits=digits,y=varint,baz=soustable,ref=ref,test=testi,nom=vecnoms[i])
                                          if (class(tmp)=="matrix")
                                          {
                                          tmp<-cbind(tmp,rep("",dim(tmp)[1]))
                                          } else {
                                                    tmp<-c(tmp,"")
                                                  }
                                      } else {
                                                tmp<- qualitbis(x=vect.var[i],digits=digits,y=varint,v=soustable,ref=ref,test=testi,nom=vecnoms[i])
                                              }
                                      descript<-rbind(descript,tmp)
                                      }
                            } else {
                                  if (!is.element(vecrefs[i],valeurs[deb:(deb+length(levvar)-1)]) & !is.na(vecrefs[i]))
                                  {
                                     print(paste("Vous essayez de réaliser un tableau, avec une référence * ",vecrefs[i]," * que vous ne définissez pas dans le vecteur de valeurs possibles pour la variable"))
                                     print(vect.var[i])
                                     return()
                                  } else {
                                            if(is.na(vecrefs[i]))
                                            {
                                              ref<-NULL
                                            } else {
                                                      ref<-vecrefs[i]
                                                    }
                                            if (lgtest==1 & is.null(testi))
                                            {
                                                tmp<- qualitbis(x=vect.var[i],digits=digits,y=varint,baz=soustable,ref=ref,test=testi,nom=vecnoms[i],valeurs=valeurs[deb:(deb+length(levvar)-1)])
                                                if (class(tmp)=="matrix")
                                                {
                                                  tmp<-cbind(tmp,rep("",dim(tmp)[1]))
                                                } else {
                                                          tmp<-cbind(tmp,rep("",length(tmp)))
                                                        }
                                            } else {
                                                    tmp<- qualitbis(x=vect.var[i],y=varint,digits=digits,baz=soustable,ref=ref,test=testi,nom=vecnoms[i],valeurs=valeurs[deb:(deb+length(levvar)-1)])
                                                    }
                                            descript<-rbind(descript,tmp)
                                        }
                                }
                        } else {
                                    if (lgtest==1 & is.null(testi))
                                    {
                                        tmp<- qualitbis(x=vect.var[i],y=varint,baz=soustable,test=testi,nom=vecnoms[i],valeurs=valeurs[deb:(deb+length(levvar)-1)],digits=digits)
                                        if (class(tmp)=="matrix")
                                        {
                                          tmp<-cbind(tmp,rep("",dim(tmp)[1]))
                                        } else {
                                                  tmp<-cbind(tmp,rep("",length(tmp)))
                                                }
                                    } else {
                                              tmp<- qualitbis(x=vect.var[i],y=varint,baz=soustable,test=testi,nom=vecnoms[i],valeurs=valeurs[deb:(deb+length(levvar)-1)],digits=digits)
                                            }
                                    descript<-rbind(descript,tmp)
                                }
                     } else {
                        #digits=max(3, getOption("digits")-3)
                                if (is.na(pres[i]))
                                {
                                  presi="med"
                                } else {
                                          presi=pres[i]
                                        }

                                if (lgtest==1 & is.null(testi))
                                {
                                    tmp<- quantitbis(x=vect.var[i],y=varint,baz=soustable,test=testi,nom=vecnoms[i],pres=presi,val=valeurs[deb])
                                    tmp<-c(tmp,"")

                                } else {
                                          tmp<- quantitbis(x=vect.var[i],y=varint,baz=soustable,test=testi,nom=vecnoms[i],pres=presi,val=valeurs[deb])
                                        }
                                descript<-rbind(descript,tmp)
                             }
            }
          }
  } # fin du for
  templev<-NULL
  for (i in 1:nlev)
  {
    if (!is.null(valvarint))
    {
       templev<-c(templev,paste(effect[i]),paste(valvarint[i]))
    } else {
            templev<-c(templev,paste(effect[i]),paste(lev[i]))
            }
  }

 #descript<-matrix(descript,ncol=2+2*nlev+lgtest,byrow=F)
  descript<-rbind(c("","",templev,rep("",lgtest)),descript)
  descript<-suppressWarnings(data.frame(descript,stringsAsFactors=FALSE))
  if (langue=="fr")
  {
      if (is.null(test))
      {
          if (is.null(nomvarint))
          {
            nomstable <- c("Paramètres","Valeurs",rep(c("N","Statistiques*"),nlev))
            names(descript) <- nomstable
          } else {
                      ligne1 <- c("Paramètres","Valeurs",rep("",nlev),nomvarint,rep("",nlev*2-(nlev+1)))
                      ligne2 <- c("","",rep(c("N","Statistiques*"),nlev))
                      descript <- rbind(ligne2,descript)
                      names(descript) <- ligne1
                  }
      } else {
                  if (is.null(nomvarint))
                  {
                      nomstable <- c("Paramètres","Valeurs",rep(c("N","Statistiques*"),nlev),"p-value")
                      names(descript) <- nomstable
                  } else {
                              ligne1 <- c("Paramètres","Valeurs",rep("",nlev),nomvarint,rep("",nlev*2-nlev))
                              ligne2 <- c("","",rep(c("N","Statistiques*"),nlev),"p-value")
                              descript <- rbind(ligne2,descript)
                              names(descript) <- ligne1
                          }
              }
  }
  if (langue=="en")
  {
        if (is.null(test))
        {
              if (is.null(nomvarint))
              {
                    nomstable <- c("Parameters","Values",rep(c("N","Statistics*"),nlev))
                    names(descript) <- nomstable
              } else {
                          ligne1 <- c("Parameters","Values",rep("",nlev),nomvarint,rep("",nlev*2-(nlev+1)))
                          ligne2 <- c("","",rep(c("N","Statistics*"),nlev))
                          descript <- rbind(ligne2,descript)
                          names(descript) <- ligne1
                      }
              } else {
                          if (is.null(nomvarint))
                          {
                                nomstable <- c("Parameters","Values",rep(c("N","Statistics*"),nlev),"p-value")
                                names(descript) <- nomstable
                          } else {
                                    ligne1 <- c("Parameters","Values",rep("",nlev),nomvarint,rep("",nlev*2-(nlev+1)))
                                    ligne2 <- c("","",rep(c("N","Statistics*"),nlev),"p-value")
                                    descript <- rbind(ligne2,descript)
                                    names(descript) <- ligne1
                                  }
                      }
  }
  return(descript)
} #fin avec variable d'intérêt
}
###############################################################################################################
###############################################################################################################
###############################################################################################################
###############################################################################################################
#tableau qualitatif avec ou sans variable d'intérêt
qualitbis<-function(x,y=NULL,test=NULL,ref=NULL,digits=2,baz=NULL,nom=NULL,
valeurs=NULL)
## utilisée dans completFRbis
##
## descriptif variables qualitatives
## x : variable d'intérêt ou "nom" de la variable
## y : groupe à comparer ou "nom" de la variable
## test : "fisher", "chisq" ou "mcnemar"
## ref : valeur de référence de la variable x
## digits : précision du pourcentage de sortie
## baz : data
## nom : nom devant apparaitre dans le tableau si différent de x
## valeurs : valeurs à faire apparaître dans le tableau
{
if (!is.null(baz))
{
  if (is.null(nom))
  {
    nom<-x
  }
  x<-baz[,x]
}
if (is.null(baz) & is.null(nom))
{
  nom<-"x"
}
x<-as.factor(x)
if (!is.null(valeurs))
{
  levels(x)<-valeurs
}
if (is.null(y))
{
  nb.na<-sum(is.na(x))
  if (nb.na==dim(baz)[1])
  {
    tmp<-cbind(nom,"NA",nb.na,"")
    return(tmp)
  }
  if (is.null(ref))
  {
    tab<-table(x)
    pourc<-round(tab/sum(tab)*100,digits=digits)
    if (length(tab)>=2)
    {
      tmp<-cbind(c(nom,rep("",length(tab)-1)),names(tab),tab,paste(pourc,"\\%"))
    } else {
              tmp<-cbind(nom,names(tab),tab,paste(pourc,"\\%"))
           }
    if (nb.na !=0) {tmp<-rbind(tmp,c("","NA",nb.na,""))}
    #tab<-table(x,exclude=NULL)
    #pourc<-round(prop.table(tab)*100,digits=digits)
    #tmp<-cbind(c(nom,rep("",(length(tab)-1))),names(tab),paste(tab),paste(pourc,"%"))
    #if ( nb.na==0) {tmp<-tmp[-dim(tmp)[1],]}
    return(tmp)
  }
  if (!is.null(ref))
  {
    tab<-table(x==ref)
    pourc<-round(tab/sum(tab)*100,digits=digits)
    tmp<-cbind(c(nom,rep("",length(ref)-1)),ref,tab,paste(pourc,"\\%"))
    if (dim(tmp)[1]==2)
    {
      tmp<-tmp[2,]
    } 
    if (nb.na!=0)
    {
    tmpna<-c("","NA",nb.na,"")
    tmp<-rbind(tmp,paste(tmpna))
    #tab<-table(x==ref,exclude=NULL)
    #pourc<-round(prop.table(tab)*100,digits=digits)
    #if (nb.na != 0) {nbna <- c(2:3);ref <- c(ref,NA)} else {nbna <- 2}
    #tmp<-cbind(c(nom,rep("",length(ref)-1)),ref,tab[nbna],paste(pourc[nbna],"%"))
    }
    return(tmp)
  }
}

if (!is.null(y))
{
    if (!is.null(baz))
    {
      y<-baz[,y]
    }
    y <- as.factor(y)
    nb.na<-tapply(is.na(x[!is.na(y)]),y[!is.na(y)],sum)
    tab<-table(x,y)
    if (sum(nb.na) ==dim(baz)[1])
    {
      tmpna <- NULL
      for (i in 1:length(levels(y)))
      {
          tmpna <- c(tmpna,nb.na[i],"")
      }
      tmpna<-c(nom,"NA",tmpna)
      return(tmpna)
    }

    if (!is.null(test))
    {
      if (test %in%c("wilcox","t","kruskal","aov")) 
      {
          restest<-suppressWarnings(eval(call(paste(test,".test",sep=""),as.numeric(x)~y)))
      }
      else {
                restest<-suppressWarnings(eval(call(paste(test,".test",sep=""),tab)))
            }
      p.value <- format.pv(restest$p.value)
      #p.value <- ifelse (p.value>0.055,signif(p.value,1),signif(p.value,2))
      #p.value <- ifelse (p.value<0.0001,"<0.0001",p.value)
      #p.value <- ifelse(nchar(as.character(p.value))==3,paste(p.value,"0",sep=""),p.value)
    }
   if (is.null(ref))
   {
      som<-matrix(colSums(tab),nrow=dim(tab)[1],ncol=dim(tab)[2],byrow=TRUE)
      #effect <- rbind((som[1,]+nb.na),(som[1,]+nb.na))
      #pourc<-round(tab/effect*100,digits=digits)
      pourc<-round(tab/som*100,digits=digits)
      tmp <- NULL
      for (i in 1:dim(tab)[2])
      {
        tmp <- cbind(tmp,paste(tab[,i]),paste(pourc[,i],"\\%") )
      }
      tmp[tmp=="NaN \\%"]<-""      
      tmp<-cbind(paste(nom),paste(rownames(tab)),tmp)
      if (dim(tab)[1]>=2)
      {
        tmp[2:dim(tab)[1],1]<- ""
      }
      if (sum(nb.na) !=0)
      {
        tmpna <- NULL
        for (i in 1:dim(tab)[2])
        {
          #tmpna <- c(tmpna,nb.na[i],paste(round(nb.na[i]/(som[i]+nb.na[i])*100,digits=digits),"%"))
          tmpna <- c(tmpna,nb.na[i],"")
        }
        tmpna<-c("","NA",tmpna)
        tmp<-rbind(tmp,paste(tmpna))
      }
      if (!is.null(test))
      {
        if (sum(nb.na) ==0)
        {
            tmp<-cbind(tmp,c(p.value,rep("",(dim(tab)[1]-1))))
        } else {
                tmp<-cbind(tmp,c(p.value,rep("",(dim(tab)[1]-1)),""))
                }
      }
      return(tmp)
    }
    if (!is.null(ref))
    {
      nbligne=1
      tab<-table(x==ref,y)
      som<-matrix(colSums(tab),nrow=dim(tab)[1],ncol=dim(tab)[2],byrow=TRUE)
      pourc<-round(tab/som*100,digits=digits)
      tmp <- NULL
      for (i in 1:dim(tab)[2])
      {
        tmp <- cbind(tmp,paste(tab[,i]),paste(pourc[,i],"\\%") )
      }
      tmp[tmp=="NaN \\%"]<-""      
      tmp<-cbind(paste(nom),paste(rownames(tab)),tmp)
      if (dim(tab)[1]>=2)
      {
        tmp[2,2]<- ref
        tmp<-tmp[2,]
      } else {tmp[,2]<- ref}
      if (sum(nb.na) !=0)
      {
        tmpna <- NULL
        for (i in 1:dim(tab)[2])
        {
          #tmpna <- c(tmpna,nb.na[i],paste(round(nb.na[i]/(som[i]+nb.na[i])*100,digits=digits),"%"))
          tmpna <- c(tmpna,nb.na[i],"")
        }
        tmpna<-c("","NA",tmpna)
        tmp<-rbind(paste(tmp),paste(tmpna))
        nbligne=nbligne+1
    }
      #tmp<-matrix(tmp,nrow=nbligne,ncol=2+2*nlev,byrow=F)
      if (!is.null(test))
      {
       if (sum(nb.na) ==0)
        {
            tmp<-c(tmp,p.value)
        } else {
                tmp<-cbind(tmp,c(p.value,""))
                }
      }
      return(tmp)
    }
}
}

################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
#tableau quantitatif avec ou sans variable d'intérêt
quantitbis<-function(x,y=NULL,test=NULL,digits=max(3, getOption("digits")-3),
baz=NULL,nom=NULL,pres="med",val=NULL)
## utilisée dans completFRbis
##
## descriptif variables qualitatives
## x : variable d'intérêt ou "nom" de la variable
## y : groupe à comparer ou "nom" de la variable
## test : "t", "wilcox","aov","kruskal"
## digits : précision du pourcentage de sortie
## baz : data
## nom : nom devant apparaitre dans le tableau si différent de x
##  pres : "med" (median [Q1;Q3]), "med_mm" (median [Q1;Q3] (min;max)),"mean" (mean +/-sd)
##  !!!! par défaut sera représentée dans le tableau la médiane [q1-q3]
{
if (!is.null(baz))
{
  if (is.null(nom))
  {
    nom<-x
  }
  x<-baz[,x]
}
if (is.null(baz) & is.null(nom))
{
  nom<-"x"
}
if (is.null(val))
{
  val <- ""
} else if (is.na(val)) { val <- ""}

if (is.null(y))
{
  nb.na<-sum(is.na(x))
  if (nb.na != length(x))
  {
    tmp<-summary(x,digits=digits)
    tmpna<-ifelse(!is.na(tmp[7]),tmp[7],0)
    if (pres=="med")
    {
      tmp<-paste(tmp[3]," [",tmp[2],";",tmp[5],"]",sep="")
      tmp<-c(nom,val,paste(length(x)-tmpna),tmp)
      return(tmp)
    }
    if (pres=="med_mm")
    {
      tmp<-paste(tmp[3]," [",tmp[2],";",tmp[5],"] (",tmp[1],";",tmp[6],")",sep="")
      tmp<-c(nom,val,length(x)-tmpna,tmp)
      return(tmp)
    }
    if (pres=="mean")
    {
      tmp<-paste(tmp[4],"(",signif(sd(x,na.rm=TRUE),digits=digits),")",sep="")
      tmp<-c(nom,val,length(x)-tmpna,tmp)
      return(tmp)
    }
    if (pres=="mean_mm")
    {
      tmp<-paste(tmp[4],"(",tmp[1],";",tmp[6],")",sep="")
      tmp<-c(nom,val,paste(length(x)-tmpna),tmp)
      return(tmp)
    }
  } else {return(c(nom,val,nb.na-length(x),NA))}
}

if (!is.null(y))
{
  if (!is.null(baz))
  {
    y<-baz[,y]
  }
  y<-as.factor(y)

  nb.na<-tapply(is.na(x[!is.na(y)]),y[!is.na(y)],sum)
  eff <-tapply(x[!is.na(y)],y[!is.na(y)],length)
  if (!is.null(test))
  {
    aov.test<-function(formula)
    {
      p.value<-unlist(summary(aov(x~y)))[9]
      p.value<-as.numeric(p.value)
      return(list(p.value=p.value))
    }
    
    if (test=="wilcox" & length(levels(as.factor(y)))>2)
    {
      print(nom)
      print("Vous avez demandé à effectuer un test de wilcoxon alors que votre variable d'intérêt comporte plus de 2 classes.")
      print("Un test de kruskal wallis a été effectué à la place.")
      test="kruskal"
    }
    restest<-suppressWarnings(eval(call(paste(test,".test",sep=""),x~y)))
    p.value <- format.pv(restest$p.value)
    #p.value <- ifelse (p.value>0.055,signif(p.value,1),signif(p.value,2))
    #p.value <- ifelse (p.value<0.0001,"<0.0001",p.value)
    #p.value <- ifelse(nchar(as.character(p.value))==3,paste(p.value,"0",sep=""),p.value)
  }
  tempo<-tapply(x,y,summary,digits=digits)
  for (ii in names(tempo))
  {
      tempo[[ii]]<-tempo[[ii]][1:6]
  }
  tempo<-matrix(unlist(tempo),nrow=dim(tempo),byrow=TRUE)

  if (pres=="med")
  {
    truc <- paste(tempo[,3]," [",tempo[,2],";",tempo[,5],"]",sep="")
    tmp <- NULL
    for (i in 1:length(levels(y)))
    {
      tmp <- cbind(tmp,paste(eff[i]-nb.na[i]),paste(truc[i]) )
    }
    for (i in 1:length(tmp))
    {
        if (tmp[i]=="NA [NA;NA]") tmp[i]<-""
    }
    tmp<-c(nom,val,tmp)
    if (!is.null(test))
    {
      tmp<-c(tmp,p.value)
    }
    return(tmp)
  }
  if (pres=="med_mm")
  {
    truc <- paste(tempo[,3]," [",tempo[,2],";",tempo[,5],"] (",tempo[,1],";",tempo[,6],")",sep="")
    tmp <- NULL
    for (i in 1:length(levels(y)))
    {
      tmp <- cbind(tmp,paste(eff[i]-nb.na[i]),paste(truc[i]) )
    }
    for (i in 1:length(tmp))
    {
      if (tmp[i]=="NA [NA;NA] (NA;NA)") tmp[i]<-""
    }
    tmp<-c(nom,val,tmp)
    if (!is.null(test))
    {
      tmp<-c(tmp,p.value)
    }
    return(tmp)
  }
    if (pres=="mean")
  {
    tmp2<-tapply(x,y,sd,na.rm=TRUE)
    tmp2<-signif(tmp2,digits=digits)
    truc <-paste(tempo[,4],"(",tmp2,")",sep="")
    tmp <- NULL
    for (i in 1:length(levels(y)))
    {
      tmp <- cbind(tmp,paste(eff[i]-nb.na[i]),paste(truc[i]) )
    }
    for (i in 1:length(tmp))
    {
       if (tmp[i]=="NaN (NA)") tmp[i]<-""
    }
    tmp<-c(nom,val,tmp)
    if (!is.null(test))
    {
      tmp<-c(tmp,p.value)
    }
    return(tmp)
  }
  if (pres=="mean_mm")
  {
    truc <-paste(tempo[,4],"(",tempo[,1],";",tempo[,6],")",sep="")
    tmp <- NULL
    for (i in 1:length(levels(y)))
    {
      tmp <- cbind(tmp,paste(eff[i]-nb.na[i]),paste(truc[i]) )
    }
    for (i in 1:length(tmp))
    {
       if (tmp[i]=="NaN (NA;NA)") tmp[i]<-""
    }
    tmp<-c(nom,val,tmp)
    if (!is.null(test))
    {
      tmp<-c(tmp,p.value)
    }
    return(tmp)
  }
}
}
###############################################################################################################
###############################################################################################################
###############################################################################################################
###############################################################################################################

qualit<-function(x,y=NULL,test=NULL,ref=NULL,digits=1,baz=NULL,nom=NULL,
valeurs=NULL)
{
# x : variable d'intérêt ou "nom" de la variable
# y : groupe à comparer ou "nom" de la variable
# test : "fisher", "chisq" ou "mcnemar"
# ref : valeur de référence de la variable x
# digits : précision du pourcentage de sortie
# baz : data
# nom : nom devant apparaitre dans le tableau si différent de x
if (!is.null(baz))
  {
  if (is.null(nom))
    {
    nom<-x
    }
  x<-baz[,x]
  }

if (is.null(baz) & is.null(nom))
  {
  nom<-"x"
  }

x<-as.factor(x)
if (!is.null(valeurs))
{
levels(x)<-valeurs
}

if (is.null(y))
  {
  nb.na<-sum(is.na(x))
  if (nb.na!=0)
    {
    nom<-paste(nom," (NA=",nb.na,")",sep="")
    }
  if (is.null(ref))
    {
    tab<-table(x)
    tmp<-paste(tab," (",round(tab/sum(tab)*100,digits=digits),")",sep="")
    tmp<-cbind(c(nom,rep("",(length(tab)-1))),names(tab),tmp)
    return(tmp)
    }
  if (!is.null(ref))
    {
    tab<-table(x==ref)
    tmp<-paste(tab," (",round(tab/sum(tab)*100,digits=digits),")",sep="")
    return(c(nom,ref,tmp[2]))
    }
  }

if (!is.null(y))
  {
  if (!is.null(baz))
    {
    y<-baz[,y]
    }
  nb.na<-tapply(is.na(x[!is.na(y)]),y[!is.na(y)],sum)
  if (sum(nb.na)!=0)
    {
    nb.na<-paste(nb.na,sep="",collapse="/")
    nom<-paste(nom," (NA=",nb.na,")",sep="")
    }
  tab<-table(x,y)
  if (!is.null(test))
    {
    p.value<-suppressWarnings(eval(call(paste(test,".test",sep=""),tab)))
    p.value<-signif(p.value$p.value,2)
    p.value[p.value<0.0001]<-"<0.0001"
    }
  if (is.null(ref))
    {
    som<-matrix(colSums(tab),nrow=dim(tab)[1],ncol=dim(tab)[2],byrow=TRUE)
    tmp<-paste(tab," (",round(tab/som*100,digits=digits),")",sep="")
    tmp<-matrix(tmp,nrow=dim(tab)[1])
    tmp<-cbind(c(nom,rep("",(dim(tab)[1]-1))),rownames(tab),tmp)
    if (!is.null(test))
      {
      tmp<-cbind(tmp,c(rep("",(dim(tab)[1]-1)),p.value))
      }
    return(tmp)
    }
  if (!is.null(ref))
    {
    tab<-table(x==ref,y)
    tmp<-paste(tab," (",round(tab/sum(tab)*100,digits=digits),")",sep="")
    som<-matrix(colSums(tab),nrow=dim(tab)[1],ncol=dim(tab)[2],byrow=TRUE)
    tmp<-paste(tab," (",round(tab/som*100,digits=digits),")",sep="")
    tmp<-matrix(tmp,nrow=dim(tab)[1])
    tmp<-c(nom,ref,tmp[2,])
    if (!is.null(test))
      {
      tmp<-c(tmp,p.value)
      }
    return(tmp)
    }
  }
}

###############################################################################################################
###############################################################################################################
###############################################################################################################
###############################################################################################################

quantit<-function(x,y=NULL,test=NULL,digits=max(3, getOption("digits")-3),
baz=NULL,nom=NULL,pres="med_mm")
{
# x : variable d'intérêt ou "nom" de la variable
# y : groupe à comparer ou "nom" de la variable
# test : "t", "wilcox","aov","kruskal"
# digits : précision du pourcentage de sortie
# baz : data
# nom : nom devant apparaitre dans le tableau si différent de x
# pres : "med" (median [Q1;Q3]), "med_mm" (median [Q1;Q3] (min;max)),"mean" (mean +/-sd)
if (!is.null(baz))
  {
  if (is.null(nom))
    {
    nom<-x
    }
  x<-baz[,x]
  }

if (is.null(baz) & is.null(nom))
  {
  nom<-"x"
  }

if (is.null(y))
  {
  nb.na<-sum(is.na(x))
  if (nb.na!=0)
    {
    nom<-paste(nom," (NA=",nb.na,")",sep="")
    }
  tmp<-summary(x,digits=digits)
  if (pres=="med")
    {
    tmp<-paste(tmp[3]," [",tmp[2],";",tmp[5],"]",sep="")
    tmp<-c(nom,"",tmp)
    return(tmp)
    }
  if (pres=="med_mm")
    {
    tmp<-paste(tmp[3]," [",tmp[2],";",tmp[5],"] (",tmp[1],";",tmp[6],")",sep="")
    tmp<-c(nom,"",tmp)
    return(tmp)
    }
  if (pres=="mean")
    {
    tmp<-paste(tmp[4]," +/-",signif(sd(x,na.rm=TRUE),digits=digits),sep="")
    tmp<-c(nom,"",tmp)
    return(tmp)
    }
  }

if (!is.null(y))
  {
  if (!is.null(baz))
    {
    y<-baz[,y]
    }
  nb.na<-tapply(is.na(x[!is.na(y)]),y[!is.na(y)],sum)
  nb.na<-tapply(is.na(x[!is.na(y)]),y[!is.na(y)],sum)
  if (sum(nb.na)!=0)
    {
    nb.na<-paste(nb.na,sep="",collapse="/")
    nom<-paste(nom," (NA=",nb.na,")",sep="")
    }
  if (!is.null(test))
    {
    aov.test<-function(formula)
      {
      p.value<-unlist(summary(aov(x~y)))[9]
      p.value<-as.numeric(p.value)
      return(list(p.value=p.value))
      }
    p.value<-suppressWarnings(eval(call(paste(test,".test",sep=""),x~y)))
    p.value<-signif(p.value$p.value,2)
    p.value_tmp<-p.value
    p.value[p.value<0.0001]<-"<0.0001"
    p.value[p.value_tmp<0.05]<-paste(p.value[p.value_tmp<0.05],"*")
    }
  tmp<-tapply(x,y,summary,digits=digits)
  for (ii in names(tmp))
  {
  tmp[[ii]]<-tmp[[ii]][1:6]
  }
  tmp<-matrix(unlist(tmp),nrow=dim(tmp),byrow=TRUE)
  #print(tmp)
  if (pres=="med")
    {
    tmp<-paste(tmp[,3]," [",tmp[,2],";",tmp[,5],"]",sep="")
    tmp<-c(nom,"",tmp)
    if (!is.null(test))
      {
      tmp<-c(tmp,p.value)
      }
    return(tmp)
    }
  if (pres=="med_mm")
    {
    tmp<-paste(tmp[,3]," [",tmp[,2],";",tmp[,5],"] (",tmp[,1],";",tmp[,6],")",sep="")
    tmp<-c(nom,"",tmp)
    if (!is.null(test))
      {
      tmp<-c(tmp,p.value)
      }
    return(tmp)
    }
  if (pres=="mean")
    {
    tmp2<-tapply(x,y,sd,na.rm=TRUE)
    tmp2<-signif(tmp2,digits=digits)
    tmp<-paste(tmp[,4]," +/-",tmp2,sep="")
    tmp<-c(nom,"",tmp)
    if (!is.null(test))
      {
      tmp<-c(tmp,p.value)
      }
    return(tmp)
    }
  }
}



quantit2<-function(x,y=NULL,test=NULL,digits=max(3, getOption("digits")-3),
baz=NULL,nom=NULL,pres="med_mm")
{
#### IDEM precedente avec une option en sus
#### mean2 : mean (min; max)
# x : variable d'intérêt ou "nom" de la variable
# y : groupe à comparer ou "nom" de la variable
# test : "t", "wilcox","aov","kruskal"
# digits : précision du pourcentage de sortie
# baz : data
# nom : nom devant apparaitre dans le tableau si différent de x
# pres : "med" (median [Q1;Q3]), "med_mm" (median [Q1;Q3] (min;max)),"mean" (mean +/-sd)
if (!is.null(baz))
  {
  if (is.null(nom))
    {
    nom<-x
    }
  x<-baz[,x]
  }

if (is.null(baz) & is.null(nom))
  {
  nom<-"x"
  }

if (is.null(y))
  {
  nb.na<-sum(is.na(x))
  if (nb.na!=0)
    {
    nom<-paste(nom," (NA=",nb.na,")",sep="")
    }
  tmp<-summary(x,digits=digits)
  if (pres=="med")
    {
    tmp<-paste(tmp[3]," [",tmp[2],";",tmp[5],"]",sep="")
    tmp<-c(nom,"",tmp)
    return(tmp)
    }
  if (pres=="med_mm")
    {
    tmp<-paste(tmp[3]," [",tmp[2],";",tmp[5],"] (",tmp[1],";",tmp[6],")",sep="")
    tmp<-c(nom,"",tmp)
    return(tmp)
    }
  if (pres=="mean")
    {
    tmp<-paste(tmp[4]," +/-",signif(sd(x,na.rm=TRUE),digits=digits),sep="")
    tmp<-c(nom,"",tmp)
    return(tmp)
    }
  }

if (!is.null(y))
  {
  if (!is.null(baz))
    {
    y<-baz[,y]
    }
  nb.na<-tapply(is.na(x[!is.na(y)]),y[!is.na(y)],sum)
  nb.na<-tapply(is.na(x[!is.na(y)]),y[!is.na(y)],sum)
  if (sum(nb.na)!=0)
    {
    nb.na<-paste(nb.na,sep="",collapse="/")
    nom<-paste(nom," (NA=",nb.na,")",sep="")
    }
  if (!is.null(test))
    {
    aov.test<-function(formula)
      {
      p.value<-unlist(summary(aov(x~y)))[9]
      p.value<-as.numeric(p.value)
      return(list(p.value=p.value))
      }
    p.value<-suppressWarnings(eval(call(paste(test,".test",sep=""),x~y)))
    p.value<-signif(p.value$p.value,2)
    p.value_tmp<-p.value
    p.value[p.value<0.0001]<-"<0.0001"
    p.value[p.value_tmp<0.05]<-paste(p.value[p.value_tmp<0.05],"*")
    }
  tmp<-tapply(x,y,summary,digits=digits)
  for (ii in names(tmp))
  {
  tmp[[ii]]<-tmp[[ii]][1:6]
  }
  tmp<-matrix(unlist(tmp),nrow=dim(tmp),byrow=TRUE)
  #print(tmp)
  if (pres=="med")
    {
    tmp<-paste(tmp[,3]," [",tmp[,2],";",tmp[,5],"]",sep="")
    tmp<-c(nom,"",tmp)
    if (!is.null(test))
      {
      tmp<-c(tmp,p.value)
      }
    return(tmp)
    }
  if (pres=="med_mm")
    {
    tmp<-paste(tmp[,3]," [",tmp[,2],";",tmp[,5],"] (",tmp[,1],";",tmp[,6],")",sep="")
    tmp<-c(nom,"",tmp)
    if (!is.null(test))
      {
      tmp<-c(tmp,p.value)
      }
    return(tmp)
    }
  if (pres=="mean")
    {
    tmp2<-tapply(x,y,sd,na.rm=TRUE)
    tmp2<-signif(tmp2,digits=digits)
    tmp<-paste(tmp[,4]," +/-",tmp2,sep="")
    tmp<-c(nom,"",tmp)
    if (!is.null(test))
      {
      tmp<-c(tmp,p.value)
      }
    return(tmp)
    }
  if (pres=="mean2")
    {
    tmp2<-tapply(x,y,sd,na.rm=TRUE)
    tmp2<-signif(tmp2,digits=digits)
    tmp<-paste(tmp[,4]," (",tmp[,1],";",tmp[,6],")",sep="")
    tmp<-c(nom,"",tmp)
    if (!is.null(test))
      {
      tmp<-c(tmp,p.value)
      }
    return(tmp)
    }
  }
}





