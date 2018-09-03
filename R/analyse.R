analyse <-
  function(){options (warn=-1)
    require(svDialogs)
    dlgList(c("Statistiques descriptives","chi deux","correlations", 
              "t de Student", "analyse de variance et covariance",
              "regressions",
              "analyses de facteurs et de composantes",
              "analyse de fiabilite et d accord"), preselect=NULL, multiple = FALSE, title="Quelle analyse voulez-vous realiser?")$res->choix
    if(length(choix)==0) return(easieR())
    if(choix=="chi deux") chi()->Resultats
    if(choix=="t de Student") test.t()->Resultats
    if(choix=="analyse de variance et covariance") {
      Filter( function(x) 'aovplus' %in% class( get(x) ), ls(envir=.GlobalEnv))->nom1
      if(length(nom1)==0) ez.anova()->Resultats else {
        dlgList(c("Analyse principale", 
                  "Resultats complementaires (e.g. contrastes d'interaction et moyennes ajustees)"), 
                preselect=NULL, multiple = FALSE, title="Quelle analyse voulez-vous realiser?")$res->choix
        if(choix== "Analyse principale") ez.anova()->Resultats else aov.plus()->Resultats
        
      }
      
    }
    if(choix=="correlations") choix.corr()->Resultats
    if(choix=="regressions") choix.reg()->Resultats
    #if(choix=="regressions logistiques") regressions.log()->Resultats
    if(choix=="analyses de facteurs et de composantes") factor.an()->Resultats
    if(choix=="analyse de fiabilite et d accord") fiabilite()->Resultats
    if(choix=="Statistiques descriptives") stat.desc()->Resultats
    return(Resultats)
  }
