valeurs.influentes <-
  function(X, critere="Grubbs", z=3.26, data=NULL){options (warn=-1)
    c("outliers")->packages
    .inf <- environment()
    list()->Resultats.valeurs.influentes
    if(any(lapply(packages, require, character.only=T))==FALSE)  {install.packages(packages) 
      require(packages)} 
    if(class(data[,X])=="integer") as.numeric(data[,X])->data[,X]
    if(class(data[,X])!="numeric") return("la variable n est pas numerique")
    if(critere=="z" && class(z)!="numeric") return("z doit etre un nombre")
    if(any(match(c("Grubbs","z"), critere))==FALSE) return("Les valeurs admises pour critere sont  z  et  Grubbs ")
    length(data[,1])->i
    if(critere=="Grubbs"){
      grubbs.test(data[,X], type = 10, opposite = FALSE, two.sided = FALSE)->outliers # test de Grubbs permettant de savoir s il y a des valeurs aberrantes
      names(data[X])->outliers$data.name
      # on realise un boucle du type: tant y est inferieure a 0.05, continue. 
      data.frame()->valeur.influentes
      while(grubbs.test(data[,X], type = 10, opposite = FALSE, two.sided = FALSE)$p.value <0.05)  { 
        which.max(abs(data[,X]))->max #cherche la valeur maximale qu on stocke dans l objet max                                                                                                
        rbind(valeur.influentes,data[max, ])->valeur.influentes
        data<-data[ -max, ] # supprime la valeur maximmal de data
      }  
      data.frame(G=outliers$statistic[1], U=outliers$statistic[2], valeur.p=round(outliers$p.value,4))->Resultats.valeurs.influentes$"Test de Grubbs"
      Resultats.valeurs.influentes$"Valeur la plus elevee"<-outliers$alternative
      
    }
    
    
    if(critere=="z"){
      # on centre reduit les residus et on stocke la valeur absolue du z dans la variable "Var_centree_abs" dans l objet data2
      abs(scale(data[,X], center = TRUE, scale = TRUE))->data$Var_centree_abs 
      valeur.influentes<-data[which(data$Var_centree_abs>z),]
      data<-data[which(data$Var_centree_abs<=z),]  
    }
    length(data[,1])->iso
    i-iso->n # nombre d observations supprimees
    round((n/i)*100,2)-> pourcentage_N # proportions d observations supprimees (nombre / taille de l echantillon)
    rbind(n, paste(pourcentage_N, "%"))->synthese_aberrant # on combine le nombre et le pourcentage. 
    data.frame(information=c("Nombre d'observations retirees", "% d'observations considerees comme influentes"), Synthese=synthese_aberrant)->synthese_aberrant # on cree un data.frame 
    if(all(dim( valeur.influentes)!=0))    Resultats.valeurs.influentes$"observations influentes"<-valeur.influentes
    Resultats.valeurs.influentes$"Synthese des observations influentes" <-synthese_aberrant
    data->>nettoyees
    return(Resultats.valeurs.influentes) 
  }
