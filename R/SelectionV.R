SelectionV <-
  function(data=NULL,info=TRUE){options (warn=-1)
    packages<-c("svDialogs")
    if(any(lapply(packages, require, character.only=T))==FALSE)  {install.packages(packages) 
      require(packages)}
    list()->Resultats
    choix.data()->data
    if(length(data)==0) return(preprocess())
    if(info==TRUE) print("Quelles sont les variables a selectionner ?")
    X<-dlgList(c(paste(names(data), "(format :", sapply(data, class), ")", sep=" "), "autres donnees"), multiple = TRUE, 
               title="Variable")$res
    if(length(X)==0) return(preprocess())
    if( X== "autres donnees") return(SelectionV())
    listes<-data.frame(paste(names(data), "(format :", sapply(data, class), ")", sep=" "), 1:length(data))
    subset(listes, listes[,1] %in% X)[,2]->X
    data[,X]->data
    fichier<- dlgInput("Quel nom voulez-vous donner au fichier?", "selection")$res
    if(length(fichier)==0) fichier<-"selection"
    strsplit(fichier, ":")->fichier
    tail(fichier[[1]],n=1)->fichier
    assign(x=fichier, value=data, envir=.GlobalEnv)
    View(data, "donnees que vous venez de selectionner")
    Resultats<-paste("les variables selectionnees sont dans", fichier)
    return(Resultats)
  }
