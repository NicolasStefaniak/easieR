analyse <-
function(){options (warn=-1)
  require(svDialogs)
  dlgList(c("Statistiques descriptives","chi deux","corrélations", 
            "t de Student", "analyse de variance et covariance",
            "régressions",
            "analyses de facteurs et de composantes",
            "analyse de fiabilité et d accord"), preselect=NULL, multiple = FALSE, title="Quelle analyse voulez-vous réaliser?")$res->choix
  if(length(choix)==0) return(easieR())
  if(choix=="chi deux") chi()->Resultats
  if(choix=="t de Student") test.t()->Resultats
  if(choix=="analyse de variance et covariance") {
    Filter( function(x) 'aovplus' %in% class( get(x) ), ls(envir=.GlobalEnv))->nom1
    if(length(nom1)==0) AN.C.OVA()->Resultats else {
      dlgList(c("Analyse principale", 
                "Résultats complémentaires (e.g. contrastes d'interaction et moyennes ajustées)"), 
              preselect=NULL, multiple = FALSE, title="Quelle analyse voulez-vous réaliser?")$res->choix
      if(choix== "Analyse principale") AN.C.OVA()->Resultats else aov.plus()->Resultats
      
    }
    
   }
  if(choix=="corrélations") choix.corr()->Resultats
  if(choix=="régressions") choix.reg()->Resultats
  #if(choix=="régressions logistiques") regressions.log()->Resultats
  if(choix=="analyses de facteurs et de composantes") factor.an()->Resultats
  if(choix=="analyse de fiabilité et d accord") fiabilite()->Resultats
  if(choix=="Statistiques descriptives") stat.desc()->Resultats
  return(Resultats)
}
