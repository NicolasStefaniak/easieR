import.results <-
function(){
  require(pander)
    if (Sys.info()[['sysname']] == 'Linux') {
    	require('tcltk')
    	fichier <- try(tk_file.choose(), silent=TRUE)
    } else {
  	fichier <- try(file.choose(), silent=TRUE)
    }
  if(class(fichier)=='try-error') return(donnees())
  openFileInOS(fichier)
    Resultats<-paste(.dico[["desc_result_succesfully_imported_in"]], fichier)
  return(Resultats)
}
