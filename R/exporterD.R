exporterD <-
function(data=NULL, nom=NULL){options (warn=-1)   
  packages<-c("svDialogs")
  if(any(lapply(packages, require, character.only=T))==FALSE)  {install.packages(packages) 
    require(packages)}
  list()->Resultats
  data <- dlgList(Filter( function(x) 'data.frame' %in% class( get(x) ), ls(envir=.GlobalEnv)), multiple = FALSE, 
                  title="Quelles données voulez-vous exporter ?")$res 
  if(length(data)==0) return(donnees())
  data<-get(data)
  nom <- dlgInput("Quel nom voulez-vous attribuer au fichier ?", "Nouveau.fichier")$res
  if(length(nom)==0) nom<-"Nouveau.fichier"
  strsplit(nom, ":")->nom
  tail(nom[[1]],n=1)->nom
  write.csv(data, file=paste(nom, ".csv"))
  paste("le fichier est sauvegardé dans", getwd())->Resultats
  return(Resultats)
}