VI.multiples <-
  function(data){ require("pych") 
    Resultats<-list()
    nvar<-length(data)
    try(psych::outlier(data, bad=T, na.rm=T,plot=T),silent=T)->essai
    if(class(essai)=="try-error"){
      msgBox("Votre matrice est singuliere, ce qui pose souci. Nous tentons de  de resoudre le souci. Si possible, la distance de Mahalanobis sera alors calculee sur le maximum d'information tout en evitant la singularite.")
      data->data2
      rankifremoved <- sapply(1:ncol(data2), function (x) qr(data2[,-x])$rank)
      which(rankifremoved == max(rankifremoved))->rangs
      if(length(rangs)==length(data2)){ 
        sample(rangs,1)->rang2
        data2[,-rang2]->data2
      } else {
        while(length(rangs)!=length(data2)){
          sample(rangs,1)->rang2
          data2[,-rang2]->data2
          rankifremoved <- sapply(1:ncol(data2), function (x) qr(data2[,-x])$rank)
          which(rankifremoved == max(rankifremoved))->rangs
        }
      }
      try(psych::outlier(data2), silent=T)->essai
      if(class(essai)=="try-error") {
        corr.test(data2)$r->matrice
        if(any(abs(matrice)==1)) {
          msgBox("vous tenter de faire une matrice de correlations avec des variables parfaitement correlees. Cela pose souci pour le calcul de la distance de Mahalanobis. Nous tentons de resoudre le souci")
          which(abs(matrice)==1, arr.ind=TRUE)->un
          un<-un[-which(un[,1]==un[,2]),]
          data2[,-un[,2]]->data2
          try(psych::outlier(data2), silent=T)->essai
          if(class(essai)=="try-error") {
            writeLines("Desole, nous ne pouvons pas calculer la distance de Mahalanobis sur vos donnees. Les analyses seront resalisees sur les donnees completes")
            0->data$D.Mahalanobis  }
        }else{essai-> data$D.Mahalanobis}
      } else{ essai-> data$D.Mahalanobis
      }
    }else{
      essai-> data$D.Mahalanobis  
    }
    
    qchisq(p=0.001, df=nvar, ncp = 0, lower.tail = FALSE, log.p = FALSE)->seuil
    data[which(data$D.Mahalanobis>seuil),]->outliers
    length(outliers[,1])/length(data[,1])*100->pourcent
    
    msgBox(paste(round(pourcent,2), "% des observations sont considerees comme outliers."))
    
    
    if(pourcent!=0){
      writeLines("Supprimer l'ensemble des outliers supprime l'ensemble des valeurs au-dela p(chi.deux)< 0.001.   
                 Supprimer une observation a la fois permet de faire une analyse detaillee de chaque observation  
                 consideree comme influente en partant de la valeur la plus extreme. La procedure s'arrete  
                 quand plus aucune observation n'est consideree comme influente")  
      
      suppr<- dlgList(c("Suppression de l'ensemble des outliers", "Suppression manuelle"), 
                      preselect=c("Suppression de l'ensemble des outliers"), multiple = FALSE, title="Comment voulez-vous les supprimer?")$res
      if(length(suppr)==0) return(NULL)
      if(suppr=="Suppression de l'ensemble des outliers") {data[which(data$D.Mahalanobis<seuil),]->data 
        outliers->Resultats$"Valeurs considerees comme influentes"}else{
          suppression<-"yes"
          outliers<-data.frame()
          while(suppression=="yes"){
            print(data[which.max(data$D.Mahalanobis),])
            cat ("Appuyez [entree] pour continuer")
            line <- readline()
            dlgMessage("Voulez-vous supprimer cette observation ?", "yesno")$res->suppression
            if(suppression=="yes") {rbind(outliers, data[which.max(data$D.Mahalanobis),])->outliers
              data[-which.max(data$D.Mahalanobis),]->data
              
            }
          }
          Resultats$"Valeurs considerees comme influentes"<-outliers
        }
    }
    Resultats$data<-data
    return(Resultats)
  }
