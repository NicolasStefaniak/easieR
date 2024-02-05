choix.reg <-
  function(html=T){
    try(library(svDialogs), silent=T)->test2
    if(class(test2)== 'try-error') return(ez.install())

    dlgList(c(txt_regressions,
              txt_mediation_effect,
              txt_logistic_regressions), preselect=txt_regressions, multiple = FALSE, title=ask_which_regression_type)$res->choix
    if(length(choix)==0) return(analyse())
    if(choix==txt_regressions) regressions(html=html)->Resultats
    if(choix==txt_mediation_effect) ez.mediation(html=html)->Resultats
    if(choix==txt_logistic_regressions) regressions.log(html=html)->Resultats
    return(Resultats)

  }

