selectionO <-
function(data=NULL, info=TRUE){options (warn=-1)
  packages<-c("svDialogs")
  if(any(lapply(packages, require, character.only=T))==FALSE)  {install.packages(packages) 
    require(packages)}
  list()->Resultats
  choix.data()->data
  if(length(data)==0) {return(preprocess())}
  if(info==TRUE) writeLines("Il est possible d'appliquer plusieurs critères de sélection simultanément, impliquant ou non plusieurs variables. 
                            Veuillez préciser le nombre de variables sur lesquelles vous désirez appliquer un ou plusieurs critères de selection. 
                            Veuillez choisir les variables sur lesquelles vous déirez appliquer une sélection") 
  X<-dlgList(c(paste(names(data), "(format :", sapply(data, class), ")", sep=" "), "autres donnees"), multiple = TRUE, 
             title="Variable")$res
  if(length(X)==0 ) return(preprocess())
  listes<-data.frame(paste(names(data), "(format :", sapply(data, class), ")", sep=" "), 1:length(data))
  subset(listes, listes[,1] %in% X)[,2]->X
  
  for(i in 1:length(X)) {  
    if(class(data[,X[i]])=="factor"){
      if(info==TRUE) {writeLines("Veuillez sélectionner les modalités que vous désirez conserver.")
        writeLines(paste("Quelles modalités voulez-vous sélectionner pour la variable", names(data[,X])[i],"?" ))}
      Y<-dlgList(levels(data[,X[i]]), multiple = TRUE, 
                 title=paste("Quelles modalités voulez-vous sélectionner pour la variable", names(data[,X])[i],"?" ))$res
      if(length(Y)==0) return(selectionO())
      data[data[,X[i]]%in% Y,]->data
      factor(data[,X[i]])->data[,X [i]]}else{
        if(info==TRUE) {print("Veuillez spécifier les critères des observations que vous désirez conserver/garder.")
          writeLines(paste("Quel critère voulez-vous utiliser pour la variable", names(data[,X])[i], "?"))}
        dlgList(c("supérieur à","supérieur ou égal à", "inférieur à", "inférieur ou égal à", "égal à", "est différent de", "entre", 
                  "au-delà (avec une limite inférieure et supérieure"), 
                preselect=NULL, multiple = FALSE, title=paste("Quel critère voulez-vous utiliser pour la variable", names(data[,X])[i], "?"))$res->choix
        if(length(choix)==0) return(selectionO())
        if(choix=="supérieur à"|choix=="inférieur à"|choix=="égal à"|choix=="supérieur ou égal à"|
           choix=="inférieur ou égal à"|choix=="est différent de"){
          if(info==TRUE) writeLines("Veuillez préciser la valeur sur laquelle les observations doivent être sélectionnées.")
          seuil<- dlgInput("Precisez la valeur?", 0)$res
          if(length(seuil)==0) return(selectionO()) else {
            strsplit(seuil, ":")->seuil
            tail(seuil[[1]],n=1)->seuil
            as.numeric(seuil)->seuil}} else{seuil.inf<- dlgInput("Limite inférieure?", 0)$res
            while(length(seuil.inf)==0) {writeLines("vous devez préciser la limite inférieure")
              dlgMessage("Vous n'avez pas precisé la limite inférieure. Voulez-vous quitter la sélection ?", "yesno")$res->quitte
              if(quitte=="yes") return(selectionO())
              seuil.inf<- dlgInput("Limite inférieure?", 0)$res}
            strsplit(seuil.inf, ":")->seuil.inf
            tail(seuil.inf[[1]],n=1)->seuil.inf
            as.numeric(seuil.inf)->seuil.inf
            seuil.sup<- dlgInput("Limite supérieure?", 0)$res
            while(length(seuil.sup)==0) {writeLines("vous devez préciser la limite supérieure")
              dlgMessage("Vous n'avez pas precisé la limite supérieure. Voulez-vous quitter la sélection ?", "yesno")$res->quitte
              if(quitte=="yes") return(selectionO())
              seuil.sup<- dlgInput("Limite superieure?", 0)$res}
            strsplit(seuil.sup, ":")->seuil.sup
            tail(seuil.sup[[1]],n=1)->seuil.sup
            as.numeric(seuil.sup)->seuil.sup}
        if(choix=="supérieur à"){data[data[,X[i]]>seuil,]->data}
        if(choix=="inférieur à"){data[data[,X[i]]<seuil,]->data}
        if(choix=="égal à"){data[data[,X[i]]==seuil,]->data}
        if(choix=="est différent de"){data[data[,X[i]]!=seuil,]->data}
        if(choix=="supérieur ou égal à"){data[data[,X[i]]>=seuil,]->data}
        if(choix=="inférieur ou égal à"){data[data[,X[i]]<=seuil,]->data}
        if(choix=="entre"){data[data[,X[i]]>=seuil.inf & data[,X[i]]<=seuil.sup,]->data}
        if(choix=="au-delà (avec une limite inférieure et supérieure"){data[data[,X[i]]<seuil.inf & data[,X[i]]>seuil.sup,]->data}
      }
  }
  
  fichier<- dlgInput("Quel nom voulez-vous donner au fichier?", "selection")$res
  if(length(fichier)==0) return(selectionO())
  strsplit(fichier, ":")->fichier
  tail(fichier[[1]],n=1)->fichier
  assign(x=fichier, value=data, envir=.GlobalEnv)
  View(data, "données que vous venez de sélectionner")
  Resultats<-paste("les observations que vous avez sélectionnées sont dans", fichier)
  return(Resultats)
}
