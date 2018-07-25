donnees <-
function(){options (warn=-1)
  require(svDialogs)
  dlgList(c("importer des données","nouveau set de données", "voir des données", "importer des résultats","Exporter des données",
            "Générer un rapport"), preselect=NULL, multiple = FALSE, 
          title="Quelle analyse voulez-vous réaliser?")$res->choix
  if(length(choix)==0) return(easieR())
  if(choix=="nouveau set de données") blank.data()->Resultats
  if(choix=="voir des données") voir()->Resultats
  if(choix=="importer des résultats") import.results()->Resultats
  if(choix=="importer des données") import()->Resultats
  if(choix=="Exporter des données") exporterD()->Resultats
  if(choix=="Générer un rapport") ez.html(ez.results)
  return(Resultats)
}
