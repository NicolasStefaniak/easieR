Centrer.red <-
  function(x, data=NULL, info=TRUE){options (warn=-1)
    packages<-c('svDialogs')
    #faire l analyse par groupe # regler le probleme des noms
    list()->Resultats
    X<-.dico[["txt_other_data"]]
    while(any(X==.dico[["txt_other_data"]])){nom <- Filter( function(x) 'data.frame' %in% class( get(x) ), ls(envir=.GlobalEnv) )
    if(info==TRUE) {print((.dico[["ask_chose_database"]]))}
    nom<-dlgList(c(nom,.dico[["txt_other_data"]]) , multiple = FALSE, title=.dico[["txt_dataframe_choice"]])$res
    if(length(nom)==0) return(preprocess())
    data<-get(nom)
    if(info==TRUE) {print((.dico[["ask_chose_variables"]]))}
    X<-dlgList(names(data), multiple = TRUE, title=.dico[["txt_variables"]])$res
    if(length(X)==0) X<-donnees()
    if(any(sapply(data[,X], class) %in% c("integer", "numeric")==FALSE)) {print(.dico[["desc_at_least_one_non_numeric"]])
      X<-.dico[["txt_other_data"]]
      str(data)}
    }


    if(info==TRUE) {writeLines(.dico[["desc_center_and_center_reduce_explaination"]])}
    dlgList(c(.dico[["txt_center"]], .dico[["txt_center_reduce"]], .dico[["txt_inferior_proba"]], .dico[["txt_superior_proba"]]), preselect=.dico[["txt_center_reduce"]], multiple = TRUE, title=.dico[["ask_what_to_do"]])$res->choix
    if(length(choix)==0) return(preprocess())

    for(i in 1:length(choix)){
      if(choix[i]==.dico[["txt_center"]]) {S<-FALSE
      nn<-.dico[["txt_center"]]}else {S<-TRUE
      nn<-.dico[["txt_centered_dot_reduced"]]}
      scale(data[,X], scale=S)->centree
      matrix(centree, ncol=length(X))->centree
      if(choix[i]==.dico[["txt_superior_proba"]]|choix[i]==.dico[["txt_inferior_proba"]]){
        if(choix[i]==.dico[["txt_superior_proba"]]){
          nn<-"p.sup"
          lower<-FALSE
        }else {
          nn<-"p.inf"
          lower<-TRUE
        }
        round(pnorm(centree, lower.tail = lower),4)->centree
      }
      data.frame(data, centree)->data
      names(data)[(length(data)+1-length(X)):length(data)]<-paste(X, nn, sep=".")
    }

    assign(nom, data, envir=.GlobalEnv)
    View(data)
    Resultats<-paste(.dico[["desc_succesful_operation"]])
    return(Resultats)
    }
