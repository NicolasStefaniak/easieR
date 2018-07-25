import.results <-
function(){
  
  file.choose()->fichier
  dget(fichier)->data1
  fichier<- dlgInput("Quel nom voulez-vous donner au fichier?", "Resultats")$res
  if(length(fichier)==0) fichier<-"data1"
  strsplit(fichier, ":")->fichier
  tail(fichier[[1]],n=1)->fichier
  assign(x=fichier, value=data1, envir=.GlobalEnv)
  Resultats<-paste("Les résultats ont été correctement importés dans", fichier)
  return(Resultats)
}
