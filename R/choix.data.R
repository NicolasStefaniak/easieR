choix.data <-
function(data=NULL, info=TRUE, nom=FALSE) {
  # data : character corresponding to the object name representing data. 
  # info : donne une explication sur les arguments
  # nom : logique. Spécifie si le nom de la base de données doit être importé en même temps. Dans ce cas, l'objet renvoyé est une liste
  list()->Resultats
  Filter( function(x) 'data.frame' %in% class( get(x) ), ls(envir=.GlobalEnv))->nom1
  if(length(nom1)==0) {
    writeLines("il n'y a pas de données dans la mémoire de R, veuillez importer les donnnées sur lesquelles réaliser l'analyse")
    import()
    choix.data(data=NULL,info=T, nom=nom)->Resultats
    return(Resultats)}
  if(!is.null(data) && data%in% nom1) data->nom1
  if(length(nom1)==1)  data<-get(nom1) else{
    if(info=="TRUE") writeLines("Veuillez choisir la base de données")
    nom1 <- dlgList(nom1, multiple = FALSE, title="Données ?")$res
    if(length(nom1)==0) {nom1<-NULL
    data<-NULL}
    if(!is.null(nom1))  data<-get(nom1)
  }
  if(nom==TRUE){
    nom1->Resultats[[1]]
    data->Resultats[[2]]}else data->Resultats
  return(Resultats)
}
