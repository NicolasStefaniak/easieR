choix.data <-
  function(data=NULL, info=TRUE, nom=FALSE) {
    # data : character corresponding to the object name representing data.
    # info : donne une explication sur les arguments
    # nom : logique. Specifie si le nom de la base de donnees doit etre importe en meme temps. Dans ce cas, l'objet renvoye est une liste
    library('svDialogs')
    list()->Resultats
    Filter( function(x) 'data.frame' %in% class( get(x) ), ls(envir=.GlobalEnv))->nom1
    if(length(nom1)==0) {
      writeLines(.dico[["desc_no_data_in_R_memory"]])
      import()
      choix.data(data=NULL,info=T, nom=nom)->Resultats
      return(Resultats)}
    if(any(!is.null(data)) && data%in% nom1) data->nom1
    if(length(nom1)==1)  data<-get(nom1) else{
      if(info=="TRUE") writeLines(.dico[["ask_chose_database"]])
      nom1 <- dlgList(nom1, multiple = FALSE, title=.dico[["ask_data"]])$res
      if(length(nom1)==0) {nom1<-NULL
      data<-NULL}
      if(!is.null(nom1))  data<-get(nom1)
    }
    if(nom==TRUE){
      nom1->Resultats[[1]]
      data->Resultats[[2]]}else data->Resultats
    return(Resultats)
  }
