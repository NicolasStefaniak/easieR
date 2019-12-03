import.results <-
function(){
  require(pander)
  fichier <- try(file.choose(), silent=TRUE)
  if(class(fichier)=="try-error") return(donnees())
  openFileInOS(fichier)
    Resultats<-paste("Les resultats ont ete correctement importes dans", fichier)
  return(Resultats)
}
