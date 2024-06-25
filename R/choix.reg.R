choix.reg <-
  function(html=T){
    try(library(svDialogs), silent=T)->test2
    if(class(test2)== 'try-error') return(ez.install())

    dlgList(c(.dico[["txt_regressions"]],
              .dico[["txt_mediation_effect"]],
              .dico[["txt_logistic_regressions"]]), preselect=.dico[["txt_regressions"]], multiple = FALSE, title=.dico[["ask_which_regression_type"]])$res->choix
    if(length(choix)==0) return(analyse())
    if(choix==.dico[["txt_regressions"]]) regressions(html=html)->Resultats
    if(choix==.dico[["txt_mediation_effect"]]) ez.mediation(html=html)->Resultats
    if(choix==.dico[["txt_logistic_regressions"]]) regressions.log(html=html)->Resultats
    return(Resultats)

  }

