trier <-
  function(X, data=NULL, info=TRUE){options (warn=-1)
    packages<-c('svDialogs')
    # faire en sorte que les donnees triees portent le nom initial des donnees
    if(any(lapply(packages, require, character.only=T))==FALSE)  {install.packages(packages)
      require(packages)}
    list()->Resultats
    choix.data(info=TRUE,nom=TRUE)->data
    if(length(data)==0) return(preprocess())
    data[[1]]->nom1
    data[[2]]->data
    if(info==TRUE) writeLines(ask_variables_to_order)
    X<-dlgList(c(names(data), txt_other_data), multiple = TRUE, title=txt_variables)$res
    if(any(X==txt_other_data)) return(trier())
    if(length(X)==0) return(preprocess())
    X->diff
    Y2<-c()
    d<-c()
    for(i in 1:length(diff)) {
      writeLines(paste(ask_level, i, desc_order))
      Y<-dlgList(diff, multiple = FALSE, title=txt_variables)$res
      if(length(Y)==0) return(trier())
      setdiff(diff, Y)->diff
      c(Y2,Y)->Y2
    }
    data[do.call("order", data[Y2]), ]->data
    View(data)
    Resultats<-desc_data_succesfully_ordered
    assign(x=nom1, value=data, envir=.GlobalEnv)
    return(Resultats)}
