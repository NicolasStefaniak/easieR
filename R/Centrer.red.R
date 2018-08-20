Centrer.red <-
  function(x, data=NULL, info=TRUE){options (warn=-1) 
    packages<-c("svDialogs")
    #faire l analyse par groupe # regler le probleme des noms
    list()->Resultats
    X<-"autres donnees" 
    while(any(X=="autres donnees")){nom <- Filter( function(x) 'data.frame' %in% class( get(x) ), ls(envir=.GlobalEnv) )
    if(info==TRUE) {print(("veuillez choisir la base de donnees"))}
    nom<-dlgList(c(nom,"autres donnees") , multiple = FALSE, title="Choix du dataframe")$res
    if(length(nom)==0) return(preprocess())
    data<-get(nom)
    if(info==TRUE) {print(("veuillez choisir la ou les variables "))}
    X<-dlgList(names(data), multiple = TRUE, title="Variable(s)")$res
    if(length(X)==0) X<-donnees()
    if(any(sapply(data[,X], class) %in% c("integer", "numeric")==FALSE)) {print("au moins une variable n'est pas numerique")
      X<-"autres donnees"
      str(data)}
    }
    
    
    if(info==TRUE) {writeLines(
      "Centrer permet d'avoir une moyenne à zero en maintenant l'ecart-type. Centrer reduire correspond à la formule du z. 
      La moyenne est de 0 et l'ecart-type vaut 1. La probabilite inferieure correspond à la probabilite d'avoir un z inferieur ou egal au z.
      La probabilite superieure correspond à la probabilite d'avoir un z superieur ou egal au z")}
    dlgList(c("centrer", "centrer reduire", "probabilite inferieure", "probabilite superieure"), preselect="centrer reduire", multiple = TRUE, title="Que voulez-vous faire ?")$res->choix
    if(length(choix)==0) return(preprocess())
    
    for(i in 1:length(choix)){
      if(choix[i]=="centrer") {S<-FALSE 
      nn<-"centrer"}else {S<-TRUE
      nn<-"centrer.reduite"}
      scale(data[,X], scale=S)->centree
      matrix(centree, ncol=length(X))->centree
      if(choix[i]=="probabilite superieure"|choix[i]=="probabilite inferieure"){
        if(choix[i]=="probabilite superieure"){
          nn<-"p.sup"
          lower<-FALSE
        }else {
          nn<-"p.inf"
          lower<-TRUE
        }
        round(pnorm(centree, lower.tail = lower),4)->centree
      }
      data.frame(data, centree)->data
      names(data)[(length(data)+1-length(X)):length(data)]<-paste(X, nn, sep=".")  
    }
    
    assign(nom, data, envir=.GlobalEnv)
    View(data)
    Resultats<-paste("L'operation a ete realisee correctement")
    return(Resultats)
    }
