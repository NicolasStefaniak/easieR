selectionO <-
  function(data=NULL, info=TRUE){options (warn=-1)
    packages<-c("svDialogs")
    if(any(lapply(packages, require, character.only=T))==FALSE)  {install.packages(packages)
      require(packages)}
    list()->Resultats
    choix.data()->data
    if(length(data)==0) {return(preprocess())}
    if(info==TRUE) writeLines(.dico[["desc_possible_apply_multiple_selection_criterion"]])
    X<-dlgList(c(paste(names(data), "(format :", sapply(data, class), ")", sep=" "), .dico[["txt_other_data"]]), multiple = TRUE,
               title=.dico[["txt_variable"]])$res
    if(length(X)==0 ) return(preprocess())
    listes<-data.frame(paste(names(data), "(format :", sapply(data, class), ")", sep=" "), 1:length(data))
    subset(listes, listes[,1] %in% X)[,2]->X

    for(i in 1:length(X)) {
      if(class(data[,X[i]])=="factor"){
        if(info==TRUE) {writeLines(.dico[["ask_modalities_to_keep"]])
          writeLines(paste(.dico[["ask_modalities_for_variable"]], names(data[,X])[i],"?" ))}
        Y<-dlgList(levels(data[,X[i]]), multiple = TRUE,
                   title=paste(.dico[["ask_modalities_for_variable"]], names(data[,X])[i],"?" ))$res
        if(length(Y)==0) return(selectionO())
        data[data[,X[i]]%in% Y,]->data
        factor(data[,X[i]])->data[,X [i]]}else{
          if(info==TRUE) {print(.dico[["ask_criterion_for_obs_to_keep"]])
            writeLines(paste(.dico[["ask_criterion_for_variable"]], names(data[,X])[i], "?"))}
          dlgList(c(.dico[["txt_superior_to"]],.dico[["txt_superior_or_equal_to"]], .dico[["txt_inferior_to"]], .dico[["txt_inferior_or_equal_to"]], .dico[["txt_equals_to"]], .dico[["txt_is_different_from"]], .dico[["txt_between"]],
                    .dico[["desc_beyond_with_lower_and_upper"]]),
                  preselect=NULL, multiple = FALSE, title=paste(.dico[["ask_criterion_for_variable"]], names(data[,X])[i], "?"))$res->choix
          if(length(choix)==0) return(selectionO())
          if(choix==.dico[["txt_superior_to"]]|choix==.dico[["txt_inferior_to"]]|choix==.dico[["txt_equals_to"]]|choix==.dico[["txt_superior_or_equal_to"]]|
             choix==.dico[["txt_inferior_or_equal_to"]]|choix==.dico[["txt_is_different_from"]]){
            if(info==TRUE) writeLines(.dico[["ask_value_for_selected_obs"]])
            seuil<- dlgInput(.dico[["ask_value"]], 0)$res
            if(length(seuil)==0) return(selectionO()) else {
              strsplit(seuil, ":")->seuil
              tail(seuil[[1]],n=1)->seuil
              as.numeric(seuil)->seuil}} else{seuil.inf<- dlgInput(.dico[["ask_lower_bound"]], 0)$res
              while(length(seuil.inf)==0) {writeLines(.dico[["desc_specify_lower_bound"]])
                dlgMessage(.dico[["ask_exit_no_lower_bound_specified"]], "yesno")$res->quitte
                if(quitte=="yes") return(selectionO())
                seuil.inf<- dlgInput(.dico[["ask_lower_bound"]], 0)$res}
              strsplit(seuil.inf, ":")->seuil.inf
              tail(seuil.inf[[1]],n=1)->seuil.inf
              as.numeric(seuil.inf)->seuil.inf
              seuil.sup<- dlgInput(.dico[["ask_upper_bound"]], 0)$res
              while(length(seuil.sup)==0) {writeLines(.dico[["desc_specify_upper_bound"]])
                dlgMessage(.dico[["ask_exit_no_upper_bound_specified"]], "yesno")$res->quitte
                if(quitte=="yes") return(selectionO())
                seuil.sup<- dlgInput(.dico[["ask_upper_bound"]], 0)$res}
              strsplit(seuil.sup, ":")->seuil.sup
              tail(seuil.sup[[1]],n=1)->seuil.sup
              as.numeric(seuil.sup)->seuil.sup}
          if(choix==.dico[["txt_superior_to"]]){data[data[,X[i]]>seuil,]->data}
          if(choix==.dico[["txt_inferior_to"]]){data[data[,X[i]]<seuil,]->data}
          if(choix==.dico[["txt_equals_to"]]){data[data[,X[i]]==seuil,]->data}
          if(choix==.dico[["txt_is_different_from"]]){data[data[,X[i]]!=seuil,]->data}
          if(choix==.dico[["txt_superior_or_equal_to"]]){data[data[,X[i]]>=seuil,]->data}
          if(choix==.dico[["txt_inferior_or_equal_to"]]){data[data[,X[i]]<=seuil,]->data}
          if(choix==.dico[["txt_between"]]){data[data[,X[i]]>=seuil.inf & data[,X[i]]<=seuil.sup,]->data}
          if(choix==.dico[["desc_beyond_with_lower_and_upper"]]){data[data[,X[i]]<seuil.inf & data[,X[i]]>seuil.sup,]->data}
        }
    }

    fichier<- dlgInput(.dico[["ask_filename"]], .dico[["txt_selection"]])$res
    if(length(fichier)==0) return(selectionO())
    strsplit(fichier, ":")->fichier
    tail(fichier[[1]],n=1)->fichier
    assign(x=fichier, value=data, envir=.GlobalEnv)
    View(data, .dico[["txt_selected_data"]])
    Resultats<-paste(.dico[["desc_selected_obs_are_in"]], fichier)
    return(Resultats)
  }
