#### Test ####

library(survival, lib.loc="C:/Program Files/R/R-3.3.2/library")
library(gdata, lib.loc="C:/Program Files/R/R-3.3.2/library")
library(Hmisc)
library(compareGroups)
library(stargazer)
library(xtable)
library(dplyr)
library(ggplot2)
library(cmprsk)


quali <- function(x, nomx, data, RAPPORT=F, SAVEFILE=F, chemin=NULL){   # x = variable ou vecteur de variable en factor
  # nomx = nom de la variable ou vecteur de nom de variable 
  # data= jeu de données
  # RAPPORT = F si pas de rapport ascii ou T si il y a un rapport ascii
  # SAVEFILE=F si on ne veut pas sauvegarder le fichier dans un fichier excel, sinon SAVEFILE=T
  # si SAVEFILE=T, préciser l'emplacement de la sauvegarde
  t4 <- NULL
  for (i in 1:length(x)){
    t3 <- NULL
    v <- data[ ,x[i]]
    t1 <- table(v, useNA="always")
    t2 <- c(round(prop.table(t1[1:(nlevels(v))])*100, 1), "-")
    t3 <- cbind(t1, t2)
    colnames(t3) <- c("n","\\%")
    rownames(t3) <- c(paste(nomx[i], levels(v), sep=": "), paste(nomx[i], "NA", sep=": "))
    t4 <- rbind(t4, t3)
  }      
  t4<-cbind(rownames(t4),t4)
  colnames(t4)<-c("Variables","n","\\%") 
  return(t4)
  
  # Rapport ASCII
  if (RAPPORT) r$add(ascii(t4, include.rownames = TRUE, include.colnames = TRUE, width=60, header=T))
  
  # Sauvegarde au format excel
  if (SAVEFILE) write.table(t4,sep="\t", file=paste(chemin, "t4.xls"))
  
  
}



result.cox <- function (modele)
{
  summary(modele)$conf.int ->  modele.detail
  res <- data.frame (Variable = names(modele$coef))
  res$HR <- round(modele.detail[,1],2)
  res$IC <- paste("[" , round(modele.detail[,3],2) , " - " , round(modele.detail[,4],2) ,"]",sep="")
  for (i in 1:length(res$IC))
  {
    res$pval[i] <- as.character(format.pv(summary(modele)$coef[i,5]))
  }
  return(res)
}
