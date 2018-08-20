donnees <-
  function(){options (warn=-1)
    require(svDialogs)
    dlgList(c("importer des donnees","nouveau set de donnees", "voir des donnees", "importer des resultats","exporter des donnees",
              "generer un rapport"), preselect=NULL, multiple = FALSE, 
            title="Quelle analyse voulez-vous realiser?")$res->choix
    if(length(choix)==0) return(easieR())
    if(choix=="nouveau set de donnees") blank.data()->Resultats
    if(choix=="voir des donnees") voir()->Resultats
    if(choix=="importer des resultats") import.results()->Resultats
    if(choix=="importer des donnees") import()->Resultats
    if(choix=="exporter des donnees") exporterD()->Resultats
    if(choix=="generer un rapport") ez.html(ez.results)
    return(Resultats)
  }
