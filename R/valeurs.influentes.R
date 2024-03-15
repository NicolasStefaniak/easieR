valeurs.influentes <-
  function(X, critere="Grubbs", z=3.26, data=NULL){options (warn=-1)
    c("outliers")->packages
    .inf <- environment()
    list()->Resultats.valeurs.influentes
    if(any(lapply(packages, require, character.only=T))==FALSE)  {install.packages(packages)
      require(packages)}
    if(class(data[,X])=="integer") as.numeric(data[,X])->data[,X]
    if(class(data[,X])!="numeric") return(desc_non_numeric_variable)
    if(critere=="z" && class(z)!="numeric") return(desc_z_must_be_a_number)
    if(any(match(c("Grubbs","z"), critere))==FALSE) return(desc_accepted_values_are_z_and_grubbs)
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
      data.frame(G=outliers$statistic[1], U=outliers$statistic[2], valeur.p=round(outliers$p.value,4))->Resultats.valeurs.influentes[[txt_grubbs_test]]
      c("G", "U", txt_p_dot_val)-> names(Resultats.valeurs.influentes[[txt_grubbs_test]])
      Resultats.valeurs.influentes[[desc_highest_value]]<-outliers$alternative

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
    data.frame(information=c(desc_number_outliers_removed, desc_percentage_outliers), Synthese=synthese_aberrant)->synthese_aberrant # on cree un data.frame
    c(txt_information, txt_synthesis)->names(synthese_aberrant)
    if(all(dim( valeur.influentes)!=0))    Resultats.valeurs.influentes[[txt_outliers]]<-valeur.influentes
    Resultats.valeurs.influentes[[txt_outliers_synthesis]] <-synthese_aberrant
    data->>nettoyees
    return(Resultats.valeurs.influentes)
  }
