selectionO <-
  function(data=NULL, info=TRUE){options (warn=-1)
    packages<-c("svDialogs")
    if(any(lapply(packages, require, character.only=T))==FALSE)  {install.packages(packages)
      require(packages)}
    list()->Resultats
    choix.data()->data
    if(length(data)==0) {return(preprocess())}
    if(info==TRUE) writeLines(desc_possible_apply_multiple_selection_criterion)
    X<-dlgList(c(paste(names(data), "(format :", sapply(data, class), ")", sep=" "), txt_other_data), multiple = TRUE,
               title=txt_variable)$res
    if(length(X)==0 ) return(preprocess())
    listes<-data.frame(paste(names(data), "(format :", sapply(data, class), ")", sep=" "), 1:length(data))
    subset(listes, listes[,1] %in% X)[,2]->X

    for(i in 1:length(X)) {
      if(class(data[,X[i]])=="factor"){
        if(info==TRUE) {writeLines(ask_modalities_to_keep)
          writeLines(paste(ask_modalities_for_variable, names(data[,X])[i],"?" ))}
        Y<-dlgList(levels(data[,X[i]]), multiple = TRUE,
                   title=paste(ask_modalities_for_variable, names(data[,X])[i],"?" ))$res
        if(length(Y)==0) return(selectionO())
        data[data[,X[i]]%in% Y,]->data
        factor(data[,X[i]])->data[,X [i]]}else{
          if(info==TRUE) {print(ask_criterion_for_obs_to_keep)
            writeLines(paste(ask_criterion_for_variable, names(data[,X])[i], "?"))}
          dlgList(c(txt_superior_to,txt_superior_or_equal_to, txt_inferior_to, txt_inferior_or_equal_to, txt_equals_to, txt_is_different_from, txt_between,
                    desc_beyond_with_lower_and_upper),
                  preselect=NULL, multiple = FALSE, title=paste(ask_criterion_for_variable, names(data[,X])[i], "?"))$res->choix
          if(length(choix)==0) return(selectionO())
          if(choix==txt_superior_to|choix==txt_inferior_to|choix==txt_equals_to|choix==txt_superior_or_equal_to|
             choix==txt_inferior_or_equal_to|choix==txt_is_different_from){
            if(info==TRUE) writeLines(ask_value_for_selected_obs)
            seuil<- dlgInput(ask_value, 0)$res
            if(length(seuil)==0) return(selectionO()) else {
              strsplit(seuil, ":")->seuil
              tail(seuil[[1]],n=1)->seuil
              as.numeric(seuil)->seuil}} else{seuil.inf<- dlgInput(ask_lower_bound, 0)$res
              while(length(seuil.inf)==0) {writeLines(desc_specify_lower_bound)
                dlgMessage(ask_exit_no_lower_bound_specified, "yesno")$res->quitte
                if(quitte=="yes") return(selectionO())
                seuil.inf<- dlgInput(ask_lower_bound, 0)$res}
              strsplit(seuil.inf, ":")->seuil.inf
              tail(seuil.inf[[1]],n=1)->seuil.inf
              as.numeric(seuil.inf)->seuil.inf
              seuil.sup<- dlgInput(ask_upper_bound, 0)$res
              while(length(seuil.sup)==0) {writeLines(desc_specify_upper_bound)
                dlgMessage(ask_exit_no_upper_bound_specified, "yesno")$res->quitte
                if(quitte=="yes") return(selectionO())
                seuil.sup<- dlgInput(ask_upper_bound, 0)$res}
              strsplit(seuil.sup, ":")->seuil.sup
              tail(seuil.sup[[1]],n=1)->seuil.sup
              as.numeric(seuil.sup)->seuil.sup}
          if(choix==txt_superior_to){data[data[,X[i]]>seuil,]->data}
          if(choix==txt_inferior_to){data[data[,X[i]]<seuil,]->data}
          if(choix==txt_equals_to){data[data[,X[i]]==seuil,]->data}
          if(choix==txt_is_different_from){data[data[,X[i]]!=seuil,]->data}
          if(choix==txt_superior_or_equal_to){data[data[,X[i]]>=seuil,]->data}
          if(choix==txt_inferior_or_equal_to){data[data[,X[i]]<=seuil,]->data}
          if(choix==txt_between){data[data[,X[i]]>=seuil.inf & data[,X[i]]<=seuil.sup,]->data}
          if(choix==desc_beyond_with_lower_and_upper){data[data[,X[i]]<seuil.inf & data[,X[i]]>seuil.sup,]->data}
        }
    }

    fichier<- dlgInput(ask_filename, txt_selection)$res
    if(length(fichier)==0) return(selectionO())
    strsplit(fichier, ":")->fichier
    tail(fichier[[1]],n=1)->fichier
    assign(x=fichier, value=data, envir=.GlobalEnv)
    View(data, txt_selected_data)
    Resultats<-paste(desc_selected_obs_are_in, fichier)
    return(Resultats)
  }
