Centrer.red <-
function(x, data=NULL, info=TRUE){options (warn=-1) 
  packages<-c("svDialogs")
  #faire l analyse par groupe # regler le probleme des noms
  list()->Resultats
  X<-"autres données" 
  while(any(X=="autres données")){nom <- Filter( function(x) 'data.frame' %in% class( get(x) ), ls(envir=.GlobalEnv) )
  if(info==TRUE) {print(("veuillez choisir la base de données"))}
  nom<-dlgList(c(nom,"autres données") , multiple = FALSE, title="Choix du dataframe")$res
  if(length(nom)==0) return(preprocess())
  data<-get(nom)
  if(info==TRUE) {print(("veuillez choisir la ou les variables "))}
  X<-dlgList(names(data), multiple = TRUE, title="Variable(s)")$res
  if(length(X)==0) X<-donnees()
  if(any(sapply(data[,X], class) %in% c("integer", "numeric")==FALSE)) {print("au moins une variable n'est pas numérique")
    X<-"autres données"
    str(data)}
  }
  
  
  if(info==TRUE) {writeLines(
    "Centrer permet d'avoir une moyenne à zéro en maintenant l'écart-type. Centrer réduire correspond à la formule du z. 
    La moyenne est de 0 et l'écart-type vaut 1. La probabilité inférieure correspond à la probabilité d'avoir un z inférieur ou égal au z.
    La probabilité supérieure correspond à la probabilité d'avoir un z supérieur ou égal au z")}
  dlgList(c("centrer", "centrer réduire", "probabilité inférieure", "probabilité supérieure"), preselect="centrer réduire", multiple = TRUE, title="Que voulez-vous faire ?")$res->choix
  if(length(choix)==0) return(preprocess())
  
  for(i in 1:length(choix)){
    if(choix[i]=="centrer") {S<-FALSE 
    nn<-"centrer"}else {S<-TRUE
    nn<-"centrer.réduite"}
    scale(data[,X], scale=S)->centree
    matrix(centree, ncol=length(X))->centree
    if(choix[i]=="probabilité supérieure"|choix[i]=="probabilité inférieure"){
      if(choix[i]=="probabilité supérieure"){
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
  Resultats<-paste("L'opération a été réalisée correctement")
  return(Resultats)
  }
