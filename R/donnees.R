donnees <-
  function(){options (warn=-1)
    require(svDialogs)
  if(grepl("French",Sys.setlocale()) | grepl("fr",Sys.setlocale())) {
     choix<- c("importer des donnees", "voir des donnees", "importer des resultats","exporter des donnees",
              "generer un rapport")   
     if( "RGtk2Extras" %in% installed.packages()) choix<-c("nouveau set de donnees", choix)
     title<-"Que voulez-vous realiser ?"
       }else{
    choix<- c("import data", "View data", "import results","export data", "Compile the report of the session")   
     if( "RGtk2Extras" %in% installed.packages()) choix<-c("new data set", choix)
    title<-"What do you want to do?"
  }
    dlgList(choix, preselect=NULL, multiple = FALSE, 
            title=title)$res->choix
    if(length(choix)==0) return(easieR())
    if(choix %in% c("nouveau set de donnees", "new data set")) blank.data()->Resultats
    if(choix %in% c("voir des donnees","View data")) voir()->Resultats
    if(choix %in% c("importer des resultats", "import results")) import.results()->Resultats
    if(choix %in% c("importer des donnees","import data") ) import()->Resultats
    if(choix %in% c("exporter des donnees", "export data")) exporterD()->Resultats
    if(choix %in% c("generer un rapport", "Compile the report of the session")) ez.html(ez.results)
    return(Resultats)
  }

