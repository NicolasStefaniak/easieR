trier <-
  function(X, data=NULL, info=TRUE){options (warn=-1) 
    packages<-c("svDialogs")
    # faire en sorte que les donnees triees portent le nom initial des donnees
    if(any(lapply(packages, require, character.only=T))==FALSE)  {install.packages(packages) 
      require(packages)}
    list()->Resultats
    choix.data(info=TRUE,nom=TRUE)->data
    if(length(data)==0) return(preprocess())
    data[[1]]->nom1
    data[[2]]->data
    if(info==TRUE) writeLines("Veuillez selectionner la (les) variable(s) a trier")
    X<-dlgList(c(names(data), "autres donnees"), multiple = TRUE, title="Variable(s)")$res
    if(any(X=="autres donnees")) return(trier())
    if(length(X)==0) return(preprocess())
    X->diff
    Y2<-c()
    d<-c()
    for(i in 1:length(diff)) {
      writeLines(paste("Veuillez choisir le niveau", i, "de tri"))
      Y<-dlgList(diff, multiple = FALSE, title="Variable(s)")$res
      if(length(Y)==0) return(trier())
      setdiff(diff, Y)->diff
      c(Y2,Y)->Y2
    }
    data[do.call("order", data[Y2]), ]->data
    View(data)
    Resultats<-"les donnees ont ete triees correctement "
    assign(x=nom1, value=data, envir=.GlobalEnv)
    return(Resultats)}
