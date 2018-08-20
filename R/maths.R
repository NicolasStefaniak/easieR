maths <-
  function(info=TRUE){
    options (warn=-1) 
    packages<-c("svDialogs")
    #faire l analyse par groupe # regler le probleme des noms
    if(any(lapply(packages, require, character.only=T))==FALSE)  {install.packages(packages) 
      require(packages)}
    list()->Resultats
    
    choix.data(nom=TRUE)->data1
    if(length(data1)==0) {return(preprocess())}
    data1[[1]]->nom1
    data1[[2]]->data
    if(info=="TRUE") writeLines("Veuillez  choisir l'opération mathématique que vous désirez réaliser ")
    dlgList(c("additions","multiplication", "division", "soustraction","moyenne de colonnes", "exposant ou racine", 
              "logarithme", "exponentiel","valeur absolue","modele complexe"), preselect="additions", multiple = FALSE, title="Quelle opération voulez-vous?")$res->choix
    if(length(choix)==0) return(preprocess())
    
    variable<-function(multiple=TRUE){
      X<-dlgList(c(names(data), "annuler"), multiple = multiple, title="Variable(s)")$res
      if(any(sapply(data[,X], class)=="factor")) {writeLines("au moins une des variables n'est pas numérique")
        writeLines(str(data))
        return(maths())}
      return(X)}
    
    valeur<-function(info=TRUE, out=NULL){
      # info : logique pour déterminer les informations relatives aux parametres doivent s'afficher dans la console
      # out : valeur renvoyée si valeur non numérique ou annulation
      if(info) writeLines("Veuillez spécifier la valeur pour réaliser votre opération mathématique.")
      msg<-"no"
      while(msg=="no" ){
        valeur1 <- dlgInput("Quelle valeur voulez-vous pour votre opération mathématique ?", out)$res 
        if(length(valeur1)!=0){
          strsplit(valeur1, ":")->valeur1
          if(class(valeur1)=="list") {  tail(valeur1[[1]],n=1)->valeur1}
          if(grepl("/",valeur1)) apply(sapply(strsplit(valeur1, split = "/"), as.numeric), 2, function(x) x[1] / x[2])->valeur1
          if(valeur1=="e") valeur1<-exp(1)
          as.numeric(valeur1)->valeur1
          msg<-"yes"} else return(out) 
        if(is.na(valeur1) ) { dlgMessage("la valeur que vous avez entrée n'est pas numérique.Voulez-vous annuler cette analyse ?", "yesno")$res->msg
          if(msg=="yes") return(out)}
        
      }
      return(valeur1)
    }
    nom<-function(data,info, nom1){
      if(info=="TRUE") writeLines("Quel nom voulez-vous attribuer a la nouvelle variable ? ")
      variable<-dlgInput("Nom de la nouvelle variable ?","nouvelle.variable")$res
      if(length(variable)==0) variable<-"nouvelle.variable"
      strsplit(variable, ":")->variable
      tail(variable[[1]],n=1)->variable
      names(data)<-c(names(data)[1:(length(data)-1)], variable)
      assign(nom1, data, envir=.GlobalEnv)
      Resultats<-paste("La variable", variable, "a été ajoutée a", nom1)
      return(Resultats)}
    
    if(choix=="additions") {
      if(info=="TRUE") writeLines("Si vous sélectionnez les deux options en meme temps, la valeur spécifiée sera ajoutée a l'ensemble des colonnes choisies 
                                  et ensuite les colonnes choisies seront additionnées. Pour additionner une valeur spécifique au total,
                                  veuillez choisir l'option addition de colonnes uniquement.")
      dlgList(c("addition de colonnes","addition d'une valeur spécifique"), preselect="addition de colonnes", multiple = TRUE, title="Quelle opération voulez-vous?")$res->choix2
      if(length(choix2)==0) return(maths())
      if(any(choix2== "addition d'une valeur spécifique")){
        variable()->X
        if(length(X)==0|| any(X=="annuler")) return(maths())
        valeur(info=info)->valeur1
        if(is.null(valeur1)) return(maths())
        data.frame(data, data[,X]+valeur1)->data
        if(valeur1>0)      names(data)[(length(data)-(length(X)-1)):length(data)]<-paste(X, "plus", valeur1, sep=".") else names(data)[(length(data)-(length(X)-1)):length(data)]<-paste(X, "moins", abs(valeur1), sep=".")
        assign(nom1, data, envir=.GlobalEnv)
        paste(valeur1, "a été ajoutée a la variable", X)->Resultats
      }
      
      if(any(choix2== "addition de colonnes")) {
        if(info=="TRUE") writeLines("Veuillez sélectionner les variables a additionner.")
        variable()->X
        if(length(X)==0|| any(X=="annuler")) return(maths())
        X->X1
        X2<-X1[1]
        X1[-1]->X1
        while(length(X1)!=0){paste(X2,"+",X1[1])->X2
          X1[-1]->X1}
        rowSums(data[,X])->data$nouvelle_variable
        if(info=="TRUE") writeLines("Vous pouvez encore ajouter une valeur spécifique au total. Laissez 0 si vous ne souhaitez rien ajouter")    
        valeur(info=info, out=0)->valeur1
        if(valeur1!=0) {data$nouvelle_variable+valeur1->data$nouvelle_variable
          paste(X2, "+", valeur1)->X2}
        writeLines(paste("vous avez réalisé l'opération suivante :", X2))
        writeLines("voulez-vous encore ajouter une valeur au total ?")
        nom(data=data, info=info,nom1=nom1)->Resultats
      }
    }
    
    if(choix=="multiplication"){
      if(info=="TRUE") writeLines("Si vous sélectionnez les deux options en meme temps, la valeur spécifiée sera multipliée a l'ensemble des colonnes choisies 
                                  et ensuite les colonnes choisies seront multipliées entre elles. Pour multiplier une valeur spécifique au total,
                                  veuillez choisir l'option multipication de colonnes uniquement.")
      dlgList(c("multiplication de colonnes","multiplication d'une valeur spécifique"), preselect="multiplication de colonnes", multiple = TRUE, title="Quelle opération voulez-vous?")$res->choix2
      if(length(choix2)==0) return(maths())
      if(any(choix2== "multiplication d'une valeur spécifique")){
        if(info=="TRUE") writeLines("Veuillez sélectionner les variables a multiplier. ")
        variable()->X
        if(length(X)==0|| any(X=="annuler")) return(maths())
        valeur(info=info, out=NULL)->valeur1
        if(is.null(valeur1)) return(maths())
        data.frame(data, data[,X]*valeur1)->data
        names(data)[(length(data)-(length(X)-1)):length(data)]<-paste(X, "multiplié.par", valeur1, sep=".")
        assign(nom1, data, envir=.GlobalEnv)
        paste(valeur1, "a multiplié la -les- variable-s", X)->Resultats
      }
      
      if(any(choix2== "multiplication de colonnes")) {
        variable()->X
        if(length(X)==0|| any(X=="annuler")) return(maths())
        
        X->X1
        X2<-X1[1]
        X1[-1]->X1
        while(length(X1)!=0){paste(X2,"*",X1[1])->X2
          X1[-1]->X1}
        1*data[,X[1]]->nouvelle
        for(i in 1:(length(X)-1)) nouvelle*data[,X[i+1]]->nouvelle
        data.frame(data, nouvelle)->data
        
        if(info=="TRUE") writeLines("Vous pouvez encore multiplier le total par une valeur spécifique. Laissez 1 si vous ne souhaitez plus multiplier par une nouvelle valeur")    
        valeur(info=info, out=1)->valeur1
        if(valeur1!=1) {data$nouvelle*valeur1->data$nouvelle
          paste(X2, "*", valeur1)->X2}
        writeLines(paste("vous avez réalisé l'opération suivante :", X2))
        nom(data=data, info=info,nom1=nom1)->Resultats
      }
    }
    if(choix=="division"){
      if(info=="TRUE") writeLines("Le numérateur est-il une variable ou une valeur ? ")
      numer<-dlgList(c("valeur", "variable"), multiple = FALSE, title="Numérateur")$res
      if(length(numer)==0) return(maths())
      if(numer=="valeur") valeur(info=info, out=1)->X else{
        if(info=="TRUE") writeLines("Veuillez sélectionner la variable au numérateur ")
        variable(multiple=FALSE)->X
        if(length(X)==0|| any(X=="annuler")) return(maths())
        data[,X]->X
      }
      
      if(info=="TRUE") writeLines("Le dénominateur est-il une variable ou une valeur ? ")
      denom<-dlgList(c("valeur", "variable"), multiple = FALSE, title="Dénominateur")$res
      if(length(denom)==0) return(maths())
      if(denom=="valeur") valeur(info=info, out=1)->Y else{
        if(info=="TRUE") writeLines("Veuillez sélectionner la variable au dénominateur ")
        variable(multiple=FALSE)->Y
        if(length(X)==0|| any(X=="annuler")) return(maths())
        data[,Y]->Y
        if(any(Y)==0) writeLines("Au moins une des valeurs au dénominateur est un 0. La valeur renvoyée dans ce cas est infinie - inf")
      }
      X/Y->data$nouvelle_variable
      nom(data=data, info=info,nom1=nom1)->Resultats
    }
    
    if(choix=="soustraction") {
      if(info=="TRUE") writeLines("Veuillez sélectionner les valeurs situées a gauche du symbole *moins*. Si plusieurs variables sont sélectionnées, 
                                  les regles du calcul matriciel sont appliqués.")
      if(info=="TRUE") writeLines("Les valeurs positives sont-elles une/des variable(s) ou une valeur ? ")
      numer<-dlgList(c("valeur", "variable"), multiple = FALSE, title="Valeurs positives")$res
      if(length(numer)==0) return(maths())
      if(numer=="valeur") valeur(info=info, out=0)->X else{
        if(info=="TRUE") writeLines("Veuillez sélectionner la -les- variable(s) a gauche du symbole *moins*")
        variable(multiple=TRUE)->X
        if(length(X)==0|| any(X=="annuler")) return(maths())
        data[,X]->X
        data.frame(X)->X
      }
      
      if(info=="TRUE") writeLines("Les valeurs a droite du symbole *moins* sont-elles une/des variable(s) ou une valeur  ? ")
      denom<-dlgList(c("valeur", "variable"), multiple = FALSE, title="Valeurs négatives")$res
      if(length(denom)==0) return(maths())
      if(denom=="valeur") valeur(info=info, out=0)->Y else{
        if(info=="TRUE") writeLines("Veuillez sélectionner la -les- variable(s) a droite du symbole *moins*.")
        Y<-NULL
        while(is.null(Y)){
          variable(multiple=TRUE)->Y
          if(length(Y)==0|| any(Y=="annuler")) return(maths())
          data[,Y]->Y
          data.frame(Y)->Y 
          if(length(X)!=1 & length(Y)!=1 & length(X)!=length(Y)) {
            writeLines("Il ne doity avoir qu'une colonne ou le nombre de colonnes a droite du symbole *moins* doit etre égal 
                       au nombre de colonnes a gauche du symbole *moins*")
            Y<-NULL} else Y<-Y
        }
        }
      X-Y->new.var
      names(new.var)<-paste0(names(X), ".moins.", names(Y))
      data<-data.frame(data, new.var)
      assign(nom1, data, envir=.GlobalEnv)
      #nom(data=data, info=info,nom1=nom1)->Resultats
      Resultats<-"L'opération mathématique s'est déroulée correctement."
      }
    
    if(choix=="moyenne de colonnes")  {
      if(info=="TRUE") writeLines("Veuillez sélectionner les variables a moyenner ")
      X<-variable()
      if(length(X)==0|| any(X=="annuler")) return(maths())
      rowMeans(data[,X])->data$nouvelle_variable
      nom(data=data, info=info,nom1=nom1)->Resultats
    }
    if(choix== "exposant ou racine"){
      if(info=="TRUE") writeLines("Veuillez sélectionner les variables auxquelles s'applique l'exposant ")
      variable(multiple=TRUE)->X
      if(length(X)==0|| any(X=="annuler")) return(maths())
      if(info=="TRUE") writeLines("Veuillez préciser la valeur de l'exposant. 
                                  NOTE : Pour les racines, l'exposant est l'inverse la valeur. Par exemple, La racine carrée vaut 1/2, la racine cubique 1/3... ")
      valeur(info=info)->Y
      if(class(Y)!="numeric") {writeLines("la valeur entrée n'est pas numérique")
        return(maths())}
      data.frame(data, data[,X]^Y)->data
      names(data)[(length(data)-(length(X)-1)):length(data)]<-paste(X, "exposant", Y, sep=".")
      assign(nom1, data, envir=.GlobalEnv)
      paste("la variable", X, " a été élevée a la puissance", Y)->Resultats
      
    }
    if(choix== "logarithme"){
      if(info=="TRUE") writeLines("Veuillez sélectionner les variables dont il faut faire le logarithme ")
      variable(multiple=TRUE)->X
      if(length(X)==0|| any(X=="annuler")) return(maths())
      if(info=="TRUE") writeLines("Veuillez préciser la base du logarithme.Pour obtenir e, tapez e")
      valeur(info=info)->Y
      if(class(Y)!="numeric") {writeLines("la valeur entrée n'est pas numérique")
        return(maths())}
      if(Y<0) {writeLines("il n'est pas possible de calculer des logarithmes pour une base est négative. NA est renvoyé")
        return(maths()) }
      data.frame(data, log(data[,X], base=Y))->data
      names(data)[(length(data)-(length(X)-1)):length(data)]<-paste("log.", X,  sep=".")
      assign(nom1, data, envir=.GlobalEnv)
      paste("le logarithme de base", Y, " a été appliqué a la variable", X)->Resultats
    }
    if(choix== "exponentiel"){
      if(info=="TRUE") writeLines("Veuillez sélectionner les variables servant a l'exponentiel ")
      variable(multiple=TRUE)->X
      if(length(X)==0|| any(X=="annuler")) return(maths())
      data.frame(data, exp(data[,X]))->data
      names(data)[(length(data)-(length(X)-1)):length(data)]<-paste("exp.", X,  sep=".")
      assign(nom1, data, envir=.GlobalEnv)
      paste("l'exponentiel a été appliqué a la variable", X)->Resultats
    }
    if(choix== "valeur absolue"){
      if(info=="TRUE") writeLines("Veuillez sélectionner les variables dont il faut faire la valeur absolue ")
      variable(multiple=TRUE)->X
      if(length(X)==0|| any(X=="annuler")) return(maths())
      data.frame(data, abs(data[,X]))->data
      names(data)[(length(data)-(length(X)-1)):length(data)]<-paste("valeur.absolue.", X,  sep=".")
      assign(nom1, data, envir=.GlobalEnv)
      paste("la valeur absolue a été appliqué a la variable", X)->Resultats
    }
    if(choix== "modele complexe"){
      writeLines("L'expression doit etre correcte. Vous pouvez utiliser directement le nom des variables
                 les opérateurs sont +,-,*,/,^,(,). Une expression correcte serait :")
    print(paste(names(data)[1],"^2+5"), quote=FALSE)
    print(names(data))
    valeur1 <- dlgInput("Veuillez spécifier le modele a réaliser")$res 
    if(length(valeur1)==0) return(maths())
    strsplit(valeur1, ":")->valeur1
    tail(valeur1[[1]],n=1)->valeur1
    try(eval(parse(text=valeur1), envir=data), silent=TRUE)->nouvelle
    if(class(nouvelle)=="try-error") {writeLines("Le modele ne peut etre évalué. Il doit contenir une erreur")
      return(maths())} else nouvelle->data$nouvelle
    
    nom(data=data,info=info, nom1=nom1)->Resultats
    
  }
  
  return(Resultats)
  }
