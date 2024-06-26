VI.multiples <-
  function(data){ require("pych")
    Resultats<-list()
    nvar<-length(data)
    try(psych::outlier(data, bad=T, na.rm=T,plot=T),silent=T)->essai
    if(class(essai)=='try-error'){
      msgBox(.dico[["desc_singular_matrix_mahalanobis_on_max_info"]])
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
      if(class(essai)=='try-error') {
        corr.test(data2)$r->matrice
        if(any(abs(matrice)==1)) {
          msgBox(.dico[["desc_perfectly_correlated_variables_in_matrix_trying_to_solve"]])
          which(abs(matrice)==1, arr.ind=TRUE)->un
          un<-un[-which(un[,1]==un[,2]),]
          data2[,-un[,2]]->data2
          try(psych::outlier(data2), silent=T)->essai
          if(class(essai)=='try-error') {
            writeLines(.dico[["desc_cannot_compute_mahalanobis"]])
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

    msgBox(paste(round(pourcent,2), .dico[["desc_percentage_outliers"]]))


    if(pourcent!=0){
      writeLines(.dico[["desc_outliers_removal_implications"]])

      suppr<- dlgList(c(.dico[["txt_suppress_all_outliers"]], .dico[["txt_suppress_outliers_manually"]]),
                      preselect=c(.dico[["txt_suppress_all_outliers"]]), multiple = FALSE, title=.dico[["ask_how_to_remove"]])$res
      if(length(suppr)==0) return(NULL)
      if(suppr==.dico[["txt_suppress_all_outliers"]]) {data[which(data$D.Mahalanobis<seuil),]->data
        outliers->Resultats[[.dico[["txt_labeled_outliers"]]]]}else{
          suppression<-"yes"
          outliers<-data.frame()
          while(suppression=="yes"){
            print(data[which.max(data$D.Mahalanobis),])
            cat (.dico[["ask_press_enter_to_continue"]])
            line <- readline()
            dlgMessage(.dico[["ask_suppress_this_obs"]], "yesno")$res->suppression
            if(suppression=="yes") {rbind(outliers, data[which.max(data$D.Mahalanobis),])->outliers
              data[-which.max(data$D.Mahalanobis),]->data

            }
          }
          Resultats[[.dico[["txt_labeled_outliers"]]]]<-outliers
        }
    }
    Resultats$data<-data
    return(Resultats)
  }
