AN.C.OVA <-
function(option=T, longdata=NULL, inter=NULL, intra=NULL, VD=NULL, cov=NULL, desires=c("Données complètes","Identification des outliers", "Données sans valeur influente"),
                   desires2=c("Modèle paramétrique","Modèle linéaire mixte"),ES="ges", sauvegarde=F, SumS=3, p.adjust=NULL, type.cont="aucun"){
  # option : logique, si TRUE, permet de spécifier à l'aide de boîtes de dialogue les options suivantes : desires, desires2, sauvegarde, SumS, ES par des boîtes de dialogue, 
  #          Dans le cas contraire, ce sont les valeurs spécifiées ou celles par défaut qui sont utilisées
  # longdata : données en format long (nécessaire le cas pour anova à groupes indépendants)
  # inter : variables intergroupes 
  # intra : variables intragroupes
  # VD : variable dépendante
  # cov : covariables
  # desires : vecteur avec une plusieurs des possibilités suivantes : c("Données complètes","Identification des outliers", "Données sans valeur influente"). Si desires est spécifié
  # desires2 : 
  # ES : taille d'effet qui doit être calculée. "ges" pour eta carré généralisé, et "pes" pour êta carré partiel
  # sauvegarde : logique, indique si les résultats doivent être sauvegardés
  # Sums : choix du type des sommes des carrés calculées. Peut être 2 ou 3
  # p.adjust : type de correction de la probabilité si type.cont vaut "Comparaison 2 à 2". La correction peut être "holm", "hochberg", "hommel", "bonferroni", "BH", "BY","fdr", "none"
  
  packages<-c( "DescTools","outliers", "nortest", "psych", "reshape2", "car", "lawstat", "pgirmess","WRS","svDialogs", "WRS2", "nlme", "afex")
  if(any(lapply(packages, require, character.only=T))==FALSE)  {install.packages(packages) 
    require(packages)}
  large.long<-function(data, VIR){
    data[complete.cases(data[,VIR]),]->data
    paste("p", 1:length(data[,1]))->data$IDeasy
    factor(data$IDeasy)->data$IDeasy
    melt(data, setdiff(names(data),VIR))->longdata
    if(length(VIR)>3) N.facteurs <- dlgInput("Combien de facteurs en mesure répétée ?", 1)$res else N.facteurs<-"1"
    while(length(N.facteurs)=="0"){writeLines("vous devez spécifier le nombre de facteurs en mesure répétée")
      dlgMessage("Vous n avez pas précisé le nombre de facteurs en mesure répétée, voulez-vous quitté ?", "yesno")$res->quitte
      if(quitte=="yes") return(NULL) else  N.facteurs <- dlgInput("Combien de facteurs en mesure répétée ?", 1)$res }
    strsplit(N.facteurs, ":")->N.facteurs
    tail(N.facteurs[[1]],n=1)->N.facteurs
    as.numeric(N.facteurs)->N.facteurs
    if(is.na(N.facteurs)) { writeLines("La valeur entrée n'est pas numérique")
      return(NULL)}
    if(N.facteurs==1){list()->intra
      list()->modalites
      dlgInput("Nom du facteur ?", "Variable.1")$res->intra[[1]]
      if(length(intra[[1]])==0) return(large.long(data=data, VIR=VIR))
      strsplit(intra[[1]], ":")->intra[[1]]
      tail(intra[[1]][[1]],n=1)->intra[[1]]
      colnames(longdata)[length(longdata)-1]<-intra[[1]]
    } else {
      c()->N.modalites2
      while(prod(N.modalites2)!=length(VIR)){list()->intra
        list()->modalites
        c()->N.modalites2
        writeLines(paste("vous avez sélectionné", length(VIR), "colonnes"))
        writeLines("le produit des modalités de chacune des variables doit correspondre au nombre de colonnes sélectionnées.")
        for(i in 1:N.facteurs) {dlgInput(paste("Nom du facteur",i,  "?"), paste("Variable",i, sep="."))$res->intra[[i]]
          if(length(intra[[i]])==0) return(large.long(data=data, VIR=VIR))
          strsplit(intra[[i]], ":")->intra[[i]]
          tail(intra[[i]][[1]],n=1)->intra[[i]]
          N.modalites <- dlgInput(paste("Combien de modalités", intra[[i]]), 2)$res
          if(length(N.modalites)==0) return(large.long(data=data, VIR=VIR))
          strsplit(N.modalites, ":")->N.modalites
          tail(N.modalites[[1]],n=1)->N.modalites
          as.numeric(N.modalites)->N.modalites
          if(is.na(N.modalites)) writeLines("Vous n'avez pas entré une valeur numérique.")
          c(N.modalites2,N.modalites)->N.modalites2
          dlgForm(setNames(as.list(paste("modalité", 1:N.modalites2[i])), paste("modalité", 1:N.modalites2[i])),
                  paste("Noms des modalités pour", intra[[i]]) )$res->modalites[[i]]
        }
        
      }
      for(i in 1:length(intra)){
        if(i==length(intra)){a<-1} else {
          a<-prod(N.modalites2[(i+1):length(intra)])
        }
        gl(n=N.modalites2[[i]], k=length(data[,1])*a, length=length(data[,1])*prod(N.modalites2), labels=modalites[[i]])->longdata$variable1
        names(longdata)<-c(names(longdata[1:(length(longdata)-1)]),intra[[i]])
      }}
    View(longdata)
    cat ("Appuyez [entree] pour continuer")
    line <- readline()
    dlgMessage("Est-ce que la structure dans un format long de vos données est correcte ?", "yesno")$res->suppression
    if(suppression=="no") return(large.long(data=data, VIR=VIR)) else {assign("intra",intra,envir=.e)
      return(longdata)}
  }
  options.aov<-function(inter, intra){
    list()->Resultats
    writeLines("le modèle paramétrique renvoie l'anova classique,le non paramétrique calcule le test de Kruskal Wallis
               si c'est un modèle à groupes indépendants, ou une anova de Friedman pour un modèle en mesure répétée.
               Le modèle mixte est l'équivalent du modèle testé dans l'anova par un modèle linéaire mixte,
               les statistiques robustes sont des anovas sur des médianes avec ou sans bootstrap.")
    if(!is.null(cov)) {
      Resultats$desires2<- dlgList(c("Modèle paramétrique","Modèle linéaire mixte"),
                                   preselect=c("Modèle paramétrique", "Modèle linéaire mixte"),
                                   multiple = TRUE, title="Quelle(s) analyses voulez-vous  ?")$res
    } else {
      if((exists("inter") && (length(inter)==1 & is.null(intra))) | (!is.null(intra) && (length(intra)==1 & is.null(inter)))) {
        Resultats$desires2<- dlgList(c("Modèle paramétrique", "Modèle non paramétrique", "Modèle linéaire mixte",
                                       "Statistiques robustes - peut prendre du temps"),
                                     preselect=c("Modèle paramétrique", "Modèle linéaire mixte", "Modèle non paramétrique", "Statistiques robustes - peut prendre du temps"),
                                     multiple = TRUE, title="Quelle(s) analyses voulez-vous  ?")$res
      } else {
        if((exists("inter") && (length(inter)==1 & !is.null(intra) && length(intra)==1)) || (exists("inter") && (length(inter)<4 & is.null(intra)))) {
          Resultats$desires2<- dlgList(c("Modèle paramétrique","Modèle linéaire mixte", "Statistiques robustes - peut prendre du temps"),
                                       preselect=c("Modèle paramétrique", "Modèle linéaire mixte", "Statistiques robustes - peut prendre du temps"),
                                       multiple = TRUE, title="Quelle(s) analyses voulez-vous  ?")$res
        } else {
          Resultats$desires2<- dlgList(c("Modèle paramétrique","Modèle linéaire mixte"), preselect=c("Modèle paramétrique", "Modèle linéaire mixte"),
                                       multiple = TRUE, title="Quelle(s) analyses voulez-vous  ?")$res
        }
      }
    }
    if(length(Resultats$desires2)==0) return(NULL)
    if(any(Resultats$desires2 %in% c("Modèle paramétrique","Modèle linéaire mixte"))){
      writeLines("Les données complètes représentent l'analyse réalisée sur l'ensemble des observations. L'analyse sans les valeurs influentes
                 est une analyse pour laquelle les valeurs influentes ont été supprimées. L'identification des valeurs influentes est realisée sur la bae du test de Grubbs")
      Resultats$desires<- dlgList(c("Données complètes","Identification des outliers", "Données sans valeur influente"),
                                  preselect=c("Données complètes","Identification des outliers", "Données sans valeur influente"),
                                  multiple = TRUE, title="Quels Resultats voulez-vous obtenir ?")$res
      if(length(Resultats$desires)==0) return(options.aov(inter=inter, intra=intra))}else Resultats$desires<-"Données complètes"
    
    
    
    if(any(Resultats$desires2=="Modèle paramétrique")){
      writeLines("la taille d'effet la plus fréquente est le êta carré partiel - pes.
                 La taille d'effet la plus précise est le êta carré géneralisé - ges")
      Resultats$ES<- dlgList(c("ges", "pes"), preselect=c("ges"),multiple = FALSE, title="Quelle taille d effet voulez-vous  ?")$res
      if(length(Resultats$ES)==0) return(options.aov(inter=inter, intra=intra))
      writeLines("Il existe plusieurs manière de calculer la somme des carrés. Le choix par defaut des logiciels commerciaux est une somme des carrés
                 de type 3, mettant la priorité sur les interactions plutôt que sur les effets principaux.")
      SumS<- dlgList(c(2,3), preselect=3,multiple = FALSE, title="Quels sommes des carrés voulez-vous utiliser ?")$res
      as.numeric(SumS)->Resultats$SumS
      if(length(Resultats$SumS)==0) return(options.aov(inter=inter, intra=intra))
    }
    writeLines("Voulez-vous sauvegarder les résultats de l'analyse ?")
    dlgList(c("TRUE","FALSE"), preselect="FALSE", multiple = FALSE, title="Voulez-vous sauvegarder?")$res->Resultats$sauvegarde
    if(length(Resultats$sauvegarde)==0) return(options.aov(inter=inter, intra=intra))
    return(Resultats)
  }
  contrastes.ez<-function(longdata, inter=NULL, intra=NULL){
    Resultats<-list()
    writeLines("Les contrastes a priori correspondent aux contrastes sans correction de la probabilité en suivant les règles de contrastes.
               Les contrastes 2 à 2 permettent de faire toutes les comparaisons 2 à 2 en appliquant ou non une correction à la probabilité")
    type.cont<- dlgList(c("a priori",  "Comparaison 2 à 2", "aucun"), preselect="a priori",multiple = FALSE, title="Quel types de contraste voulez-vous ?")$res
    if(length(type.cont)==0) return(NULL)
    Resultats$type.cont<-type.cont
    c(inter, unlist(intra))->interintra
    if(type.cont=="a priori") {
      contrastes<-list()
      writeLines("Vous pouvez choisir les contrastes que vous souhaitez. Néanmoins les règles concernant l'application des contrastes doivent être respectées.
                 Les contrastes peuvent etre specifiés manuellement. Dans ce cas, veuillez choisir spécifier les contrastes")
      cont.exemple<-list()
      contr.helmert(3)->cont.exemple$Orthogonaux
      apply(contr.helmert(3), 2, rev)->cont.exemple$Orthogonaux.inversés
      contr.poly(3)->cont.exemple$Polynomiaux
      contr.treatment(3, contrasts = TRUE, sparse = FALSE)->cont.exemple$comparaison.ligne.de.base
      print(cont.exemple)
      
      for (i in 1:length(interintra)){
        if(i>1) {
          type.cont2<- dlgList(c("orthogonaux", "orthogonaux inversés", "polynomiaux","comparaison à une ligne de base", "spécifier les contrastes"),
                               preselect=c("orthogonaux"), multiple = FALSE, title=paste("Quels contrastes pour la variable",names(longdata[interintra])[i],"?"))$res} else {
                                 type.cont2<- dlgList(c("orthogonaux", "orthogonaux inversés", "polynomiaux","comparaison à une ligne de base",
                                                        "spécifier les contrastes"),preselect=c("orthogonaux"), multiple = FALSE, title=paste("Quels contrastes pour la variable",names(longdata[interintra])[i],"?"))$res
                               }
        if(length(type.cont2)==0) return(contrastes.ez())
        if(type.cont2=="orthogonaux") {contr.helmert(nlevels(longdata[,interintra[i]]))->contrastes[[i]]}
        if(type.cont2=="orthogonaux inversés") {apply(contr.helmert(nlevels(longdata[,interintra[i]])), 2, rev)->contrastes[[i]]}
        if(type.cont2=="polynomiaux")  contr.poly(nlevels(longdata[,interintra[i]]))->contrastes[[i]]
        if(type.cont2=="comparaison à une ligne de base") {
          base<- dlgList(levels(longdata[, interintra[i]]), preselect=levels(longdata[,interintra[i]])[1],
                         multiple = FALSE, title="Quelle est la ligne de base?")$res
          which(levels(longdata[, interintra[i]])==base)->base
          contr.treatment(levels(longdata[, interintra[i]]), base = base, contrasts = TRUE, sparse = FALSE)->contrastes[[i]]
        }
        if(type.cont2=="spécifier les contrastes"){
          ortho<-FALSE
          while(ortho!=TRUE){
            matrix(rep(0,times=nlevels(longdata[,interintra[i]])*(nlevels(longdata[,interintra[i]])-1)), nrow=nlevels(longdata[,interintra[i]]))->contrastes3
            dimnames(contrastes3)[[1]]<-levels(longdata[,interintra[i]])
            dimnames(contrastes3)[[2]]<-paste("contraste", 1:(nlevels(longdata[,interintra[i]])-1), sep=".")
            fix(contrastes3)->contrastes3
            if(any(colSums(contrastes3)!=0)|(nlevels(longdata[,interintra[i]])>2 & max(rle(c(contrastes3))$lengths)>2*(nlevels(longdata[,interintra[i]])-2))) ortho<-FALSE else {
              test.out<-rep(1, length(contrastes3[,1]))
              for(j in 1:length(contrastes3[1,])) {contrastes3[,j]*test.out->test.out}
              if(sum(test.out)==0) ortho<-TRUE else ortho<-FALSE}
            if(ortho==FALSE) {dlgMessage("Les contrastes doivent respecter l orthogonalité. Voulez-vous continuer ?", "yesno")$res->cont
              if(cont=="no") return(contrastes.ez(longdata=longdata, inter=inter, intra=intra ))  }
            contrastes[[i]]<-contrastes3
            
          }
          
        }
        
        dimnames(contrastes[[i]])[[2]]<-paste("contraste", 1:(nlevels(longdata[,interintra[i]])-1), sep=".")
        dimnames(contrastes[[i]])[[1]]<-levels(longdata[,interintra[i]])
      }
      names(contrastes)<-interintra
      Resultats$contrastes<-contrastes
      
    }
    if(type.cont== "Comparaison 2 à 2"){
      list()->p.adjust
      writeLines("Quelle correction de la probabilité voulez-vous appliquer ? Pour ne pas appliquer de correction, choisir +none+")
      dlgList(c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY","fdr", "none"), preselect="holm", multiple = FALSE, title="Type de correction ?")$res->p.adjust
      if(length(p.adjust)==0) return(contrastes.ez())
      Resultats$p.adjust<-p.adjust
    }
    return(Resultats)
  }
  
  
  .e <- environment()
  Resultats<-list()
  if(!is.null(c(inter,intra))) {type.v<-c()
  if(!is.null(inter)) c(type.v,("Groupes indépendants"))->type.v
  if(!is.null(intra)) c(type.v,("Mesure répétée"))->type.v
  } else { writeLines("Veuillez préciser le(s) type(s) de variable(s) que vous souhaitez inclure dans l'analyse.")
    type.v<-dlgList(c("Groupes indépendants", "Mesure répétée", "Covariables"), multiple = TRUE, title="Quel-s type-s de variables?")$res
    if(length(type.v)==0) return(analyse())}
  
  if(any(type.v== "Groupes indépendants") & any(type.v== "Mesure répétée")) plan<-"Plan mixte" else {
    if(all(type.v!="Mesure répétée") & any(type.v== "Groupes indépendants"))plan<-"Groupes independants" else {
      if(any(type.v=="Mesure répétée") & all(type.v!= "Groupes indépendants")) plan<-"Mesure repetee"
      else {
        writeLines("il est indispensable d'avoir au minimum des variables à groupes independants ou en mesure répétée")
        return(AN.C.OVA())
      }
    }
  }
  # revoir data pour passer de large en long par ligne de commamande
  if(is.null(longdata)) {
    choix.data(info=TRUE, nom=TRUE)->data
    if(length(data)==0) return(AN.C.OVA())
    data[[1]]->nom
    data[[2]]->data
    listes<-data.frame(paste(names(data), "(format :", sapply(data, class), ")", sep=" "), names(data))
  }
  
  if(is.null(c(inter,intra))) {
    if(plan=="Groupes independants"){data->longdata
      data<-NULL
      intra<-NULL
      paste("p", 1:length(longdata[,1]))->longdata$IDeasy
      factor(longdata$IDeasy)->longdata$IDeasy}else{
      }
    if(any(type.v=="Mesure répétée")) {
      VIR<-"autres"
      while(any(VIR=="autres")){
        writeLines("veuillez sélectionner les variables OU les modalités de la (des) variables à mesure(s) répétée(s).")
        VIR<-dlgList(paste(names(data), "(format :", sapply(data, class), ")", sep=" "), multiple = TRUE, 
                     title="Mesures répétées")$res
        if(length(VIR)==0) return(AN.C.OVA())
        subset(listes, listes[,1] %in% VIR)[,2]->VIR
        as.character(VIR)->VIR
        if(all(sapply(data[,VIR], class)=="factor")){
          data->longdata
          VIR->intra
          writeLines("Quelle est la variable identifiant les participants ?")
          IDeasy<-dlgList(paste(names(longdata), "(format :", sapply(data, class), ")", sep=" "), multiple = TRUE, title="Identifiant participant")$res
          if(length(IDeasy)==0) return(AN.C.OVA())
          subset(listes, listes[,1] %in% IDeasy)[,2]->IDeasy
          names(longdata)[which(names(longdata)== IDeasy)]<-"IDeasy"
          factor(longdata$IDeasy)->longdata$IDeasy
          format<-"long"
          if(length(intra)==1) nlevels(longdata[,unlist(intra)])->N.modalites2 else {sapply(longdata[,unlist(intra)],nlevels)->N.modalites2 }
          if(nlevels(longdata$IDeasy)*prod(N.modalites2)!=length(longdata[,1])) {
            okCancelBox("Chaque participant doit apparaître une et une seule fois pour chaque combinaison des modalités")
            return(AN.C.OVA())}
        }else{
          format<-"court"
          if(length(VIR)==1) {
            writeLines("Pour un facteur en mesure répétée, il faut au moins deux colonnes")
            VIR<-"autres"}
          if(length(setdiff(sapply(data[,VIR], class), c("numeric","integer")))!=0 ){ 
            writeLines("Si vos données sont en format large, les mesures doivent toutes être numériques ou des integers.")
            VIR<-"autres" 
            
          }
          if( all(sapply(data[,VIR], class)%in% c("numeric", "integer"))) {
            data[complete.cases(data[,VIR]),]->data
            large.long(data=data, VIR=VIR)->longdata
            assign(x=paste0(nom,".format.long"), value=longdata, envir=.GlobalEnv)}
        }
      }
      if(plan=="Mesure repetee") inter<-NULL
    }
  }
  
  if(plan=="Groupes independants"|plan=="Plan mixte"){ 
    if(plan=="Groupes independants") intra<-NULL
    setdiff(names(longdata), c("IDeasy", "variable", "value", intra))->diffs
    inter<-"autres donnees"
    while(inter=="autres donnees"){ 
      writeLines("Veuillez choisir les variable-s à groupes indépendants")
      if(length(diffs)==1) {inter<-dlgList(paste(diffs, "(format :",class(longdata[,diffs]),")"), multiple = TRUE, 
                                           title="Variables à groupes indépendants")$res} else {
                                             inter<-dlgList(paste(diffs, "(format :", sapply(longdata[,diffs], class), ")", sep=" "), multiple = TRUE, 
                                                            title="Variables à groupes indépendants")$res}
      
      if(length(inter)==0) {
        if(okCancelBox("Vous n avez pas choisi de variable à groupes indépendants. Voulez-vous continuer  (ok) ou abandonner (annuler) cette analyse ?"))  inter<-"autres donnees" else return(AN.C.OVA())
      }
      if(inter!="autres donnees") {subset(listes, listes[,1] %in% inter)[,2]->inter
        as.character(inter)->inter}
    }
    if(length(inter)==1){
      if(class(longdata[,inter])!="factor") factor(longdata[,inter])->longdata[,inter]
    }else {
      if(any(sapply(longdata[,inter],class)!="factor")) lapply(longdata[,inter],factor)->longdata[,inter] 
    }
  }
  
  if(plan=="Groupes independants" || format=="long") {
    writeLines("Veuillez choisir la variable dépendante.") 
    setdiff(names(longdata), c("IDeasy", "variable", unlist(intra), inter))->diffs 
    vd.num<-FALSE
    while( vd.num!=TRUE){
      if(length(diffs)==1)  VD<-dlgList(paste(diffs, "(format :", class(longdata[, diffs]), ")", sep=" "), multiple = FALSE, title="Variable dépendante")$res else {
        VD<-dlgList(c(paste(diffs, "(format :", sapply(longdata[, diffs], class), ")", sep=" ")), multiple = FALSE, title="Variable dépendante")$res}
      if(length(VD) == 0L) return(AN.C.OVA())
      subset(listes, listes[,1] %in% VD)[,2]->VD
      as.character(VD)->VD
      if(!is.element(class(longdata[,VD]), c("integer", "numeric"))) {
        if (okCancelBox("Vous n'avez pas choisi une variable dépendante numérique. La variable dépendante doit être numérique. Continuer ?")) vd.num<-FALSE  else return(AN.C.OVA())}else vd.num<-TRUE
    }
    
    if(!is.null(intra)) {
      if( min(table(longdata$IDeasy))!=  max(table(longdata$IDeasy)))  msgBox("Certains participants ont des valeurs manquantes sur les facteurs en mesures répétées. Ils vont être supprimés des analyses")
      
      while(min(table(longdata$IDeasy))!=  max(table(longdata$IDeasy))){
        names(table(longdata$IDeasy))[which.min(table(longdata$IDeasy))]->mid
        longdata[-which(longdata$IDeasy==mid) , ]->longdata
        factor(longdata$IDeasy)->longdata$IDeasy
      }       
    }
  }
  
  
  if(any(type.v=="Covariables")) {
    if(exists("diffs")) setdiff(names(longdata), c("IDeasy", "variable", "value", unlist(intra),VD, inter))->diffs else setdiff(names(longdata), c("IDeasy", "variable", "value",inter, VD, unlist(intra)))->diffs
    writeLines("Veuillez choisir la ou les covariables")
    cov<-dlgList(c(paste(diffs, "(format :",sapply(longdata[, diffs], class),")")), multiple = TRUE, title="Covariable-s?")$res
    if(length(cov) == 0L | cov=="aucune") return(AN.C.OVA())
    subset(listes, listes[,1] %in% cov)[,2]->cov
    as.character(cov)->cov
    longdata[complete.cases(longdata[,c(cov)]),]->longdata
  }else cov<-NULL
  
  if(length(intra)==1 & is.null(inter)) nlevels(longdata[,unlist(intra)])->N.modalites2 else {
    if(length(inter)==1 & is.null(intra)) nlevels(longdata[,unlist(inter)])->N.modalites2 else sapply(longdata[,c(inter, unlist(intra))],nlevels)->N.modalites2 }
  if(prod(N.modalites2)>3*length(longdata[,1])) return("Il n'y a pas assez d'observations pour réaliser l'analyse. Veuillez vérifier vos données et vous assurez qu'il y a au moins trois observations par modalité de chaque facteur")
  
  if(option) {options.aov(inter=inter, intra=intra)->options.out
    if(is.null(options.out)) return(AN.C.OVA())
    options.out$desires->desires
    options.out$desires2->desires2
    options.out$sauvegarde->sauvegarde
    options.out$ES->ES
    options.out$SumS->SumS}
  # REVOIR Pour FORMAT LONG      
  if(is.null(VD)) VD<-"value"
  
  longdata[complete.cases(longdata[,c(inter,unlist(intra), VD)]),]->longdata
  ftable(longdata[,c(inter,unlist(intra))])->aov.check
  if(any(is.na(aov.check)) || min(aov.check)<3) {msgBox("Certains groupes ont moins de 3 observations. Vérifiez vos données.")
    return(aov.check)
  }
  if(length(unique(longdata[,VD]))<3) return("La variable dépendante a moins de trois valeurs différentes. Vérifiez vos données ou l'analyse que vous tentez de réaliser n'est pas pertinente.")
  
  if(any(desires2%in%c("Modèle paramétrique","Modèle linéaire mixte"))){
    contrastes.ez(longdata, inter=inter, intra=intra)->cont
    if(is.null(cont)) return(AN.C.OVA()) 
    cont$type.cont->type.cont
    cont$p.adjust->p.adjust
    cont$contrastes->contrastes    
  } else{
    type.cont<-"aucun"
    p.adjust<-NULL
    contrastes<-NULL
  }
  
  anova2<-function(VD=NULL, inter=NULL, intra=NULL, longdata,  type.cont,p.adjust, SumS, desires2, cov=NULL, contrastes=NULL)   {
    
    list()->Resultats
    cov1<-NULL
    if(!is.null(cov)) { 
      for(i in 1:length(cov)) {paste0(cov1,cov[i],"+")->cov1}}
    
    if(!is.null(inter))  {pred.ind<-inter[1]  
    if(length(inter)>1) {
      for(i in 1:(length(inter)-1)){ paste(pred.ind, "*",inter[1+i])->pred.ind}}
    paste0("~1|IDeasy")->random}
    
    if(!is.null(intra))  {
      ez.principal<-intra[[1]]
      erreur<-paste0("+Error(", intra[[1]])
      random<-paste0("~1|IDeasy/", intra[[1]])
      if(length(intra)>1) {for(i in 1:(length(intra)-1)){
        paste(ez.principal, "*",intra[[i+1]])->ez.principal
        paste(erreur, "*", intra[[i+1]])->erreur
        paste0(random, "/", intra[[i+1]])->random
      }
      }
      paste(ez.principal, erreur,"|IDeasy)")->pred.rep
    }
    
    if(!is.null(inter) & !is.null(intra)) paste(pred.ind, "*",pred.rep)->predicteurs else {
      if(!is.null(inter) & is.null(intra)) paste0(pred.ind,"+Error(1|IDeasy)")->predicteurs else pred.rep->predicteurs
    }
    as.formula(paste0(VD, "~",cov1, predicteurs))->modele  
    modele->Resultats$"Modèle testé"
    
    psych::describeBy(longdata[,VD], longdata[ ,c(inter, unlist(intra))] ,mat=TRUE,type=3)->Resultats$"statistiques descriptives"$indices
    list()->aov.plus.in
    for(i in 1:length(c(inter, unlist(intra)))){
      combn(c(inter, unlist(intra)), i)->facteurs
      for(j in 1:ncol(facteurs)){
        psych::describeBy(longdata[,VD], longdata[ ,facteurs[,j]] ,mat=TRUE,type=3)->sd.aov
        
        if(nrow( facteurs) ==1) paste("Statistiques descriptives de la variable", facteurs[,j])->nsd else {
          paste("Statistiques descriptives de l'interaction entre", facteurs[1,j])->nsd
          for(k in 2: nrow( facteurs)){paste(nsd, ":",  facteurs[k,j])->nsd
          }
          
        }
        sd.aov->aov.plus.in[[nsd]]
      }
    }
    Resultats$"statistiques descriptives"$Information<-"Pour obtenir les statistiques descriptives par facteur, veuillez utiliser aov.plus() "
    
    if(any(Resultats$"statistiques descriptives"$n<2)) {
      "il y a moins de 3 observations pour un des groupes"-> Resultats$"information"
      return(Resultats)
    }  
    
    if(any(desires2=="Modèle paramétrique") | any(desires2=="Modèle linéaire mixte")){
      if(any(Resultats$"statistiques descriptives"$indices$sd==0)) Resultats$Avertissement<-"La variance d'au moins un groupe vaut 0. Les résultats risquent d'être considerablement biaisés"  
      
      if(exists("pred.ind") & exists("ez.principal")) paste(pred.ind, "*",ez.principal)->predicteurs else {
        if(exists("pred.ind") & !exists("ez.principal")) pred.ind->predicteurs else ez.principal->predicteurs}
      lm(as.formula(paste0(VD,"~",predicteurs)),na.action=na.exclude, data=longdata)->lm.r1
      resid(lm.r1)->longdata$residu
      assign(x="longdata", value=longdata, envir=.e)
      if(length(longdata$residu)<5000){
        shapiro.test(longdata$residu)->Shapiro_Wilk # realise le Shapiro-Wilk
        lillie.test(longdata$residu)->Lilliefors  # realise le Lilliefors
        round(data.frame(Shapiro_Wilk$statistic,Shapiro_Wilk$p.value, Lilliefors$statistic, Lilliefors$p.value),4)->normalite
        names(normalite)<-c("W de Shapiro-Wilk", "valeur.p SW", "D de Lilliefors", "valeur.p Llfrs")
        dimnames(normalite)[1]<-" "
        format(normalite, width = max(sapply(names(normalite), nchar)), justify = "centre")->Resultats$"Tests de normalité"}
      h<-hist(longdata$residu, breaks=10, density=10, col="black", xlab="residus", main="Distribution des résidus") 
      xfit<-seq(min(longdata$residu),max(longdata$residu),length=40) 
      yfit<-dnorm(xfit,mean=mean(longdata$residu),sd=sd(longdata$residu)) 
      yfit <- yfit*diff(h$mids[1:2])*length(longdata$residu) 
      lines(xfit, yfit, col="darkblue", lwd=2) 
      if(!is.null(cov) & !is.null(inter)){
        options(contrasts = c("contr.helmert", "contr.poly"))
        for(i in 1:length(cov)){
          aov(as.formula(paste0(cov[i], "~",pred.ind)), data=longdata)->aov.cov
          Anova(aov.cov, type="III")->aov.cov
          names(aov.cov)<-c("SC", "ddl", "F", "valeur.p")
          aov.cov->Resultats$"Conditions d'application de l'ancova"[[paste0("Test de l'absence de différence entre les groupes sur ", cov[i])]]
          if(i==1) {paste(cov[1],"*")->cov2} else {paste0(cov2, cov[i],"*")->cov2}
        }
        aov(as.formula(paste0(VD, "~", cov2,pred.ind)), data=longdata)->aov.cov
        Anova(aov.cov, type="III")->aov.cov
        names(aov.cov)<-c("SC", "ddl", "F", "valeur.p")
        aov.cov-> aov.cov->Resultats$"Conditions d'application de l'ancova"$"Test de l'homogénéité des pentes entre les groupes sur la variable dépendante"
        
      }
      if(any(desires2=="Modèle paramétrique")){
        if(!is.null(inter)){
          paste0(VD, "~",pred.ind)->modele2
          leveneTest(as.formula(modele2),data=longdata)->Levene # test de Levene pour homogeneite des variances
          round(unlist(Levene)[c(1,2,3,5)],3)->Levene
          names(Levene)<-c("ddl1","ddl2","F","valeur.p")
          Levene->Resultats$"Test de Levene vérifiant l'homogénéité des variances"
        }
        options(contrasts=c("contr.sum","contr.poly"))
        if(!is.null(cov)) factorize<-FALSE else factorize<-TRUE
        aov_4(as.formula(modele),data=longdata, es_aov=ES, type=SumS,factorize=factorize)->aov.out
        
        if(length(c(inter, unlist(intra)))>1) {
          c(unlist(intra), inter)->intrainter
          graph.modele<-paste0(intrainter[1],"~",intrainter[2])
          if(length(intrainter)>2){paste0(graph.modele, "|",intrainter[3] )->graph.modele
            if(length(intrainter)>3){ for(i in 4:length(intrainter)){paste0(graph.modele, "*",intrainter[i] )->graph.modele} 
              
            }} 
          x11()
          print(emmip(aov.out,as.formula(graph.modele),CIs=T))
        }
        
        
        summary(aov.out)->aov.out2 
        nice(aov.out, correction="none", intercept=T, es=ES,type=SumS)->aov.out
        names(aov.out)<-c("Effet","ddl.num, ddl.denom", "CME", "F", names(aov.out)[5], "valeur.p" )
     #   format(aov.out, width = max(sapply(names(aov.out), nchar)), justify = "centre")->aov.out
     #   format(names(aov.out), justify = "centre")->names(aov.out)
        if(!is.null(intra) && any( sapply(longdata[,c(unlist(intra))],nlevels)>2)) {
          round(aov.out2$sphericity.test,5)->Resultats$"test de Mauchly testant la sphéricité de la matrice de covariance"
        }
        
        aov.out->Resultats$"Analyse principale"
        if(!is.null(intra) && any( sapply(longdata[,c(unlist(intra))],nlevels)>2)) {data.frame(round(aov.out2$pval.adjustments,5))->GG.HF
          names(GG.HF)<-c("GG.eps", "GG.valeur.p","HF.eps", "HF.valeur.p")
          GG.HF->Resultats$"Correction de Greenhouse-Geisser et de  Hyunh-Feldt"}
        if(length(inter)==1 & is.null(intra) & is.null(cov)) {oneway.test(as.formula(paste(VD,"~", inter)),data=longdata)->Welch
          round(data.frame("F"=Welch$statistic,"ddl.num"=Welch$parameter[1],"ddl.denom"=Welch$parameter[2],"valeur.p"=Welch$p.value),4)->Welch
          Welch->Resultats$"Anova avec correction de Welch pour variances hétérogènes"
        }  
      }
      
      if(type.cont=="a priori" | any(desires2== "Modèle linéaire mixte")){
        if(type.cont=="a priori" ){
          for(i in 1:length(contrastes)){
            contrastes[[i]]->contrasts(longdata[,names(contrastes)[i]])
          }
          
        }
        modele.lme<-paste0(VD, "~",cov1, predicteurs) 
        #   paste0("lme(", modele.lme, ", random=", random, ",data=", ifelse(!is.null(intra) , paste0(nom,".format.long"), nom),
        #         ", method='REML')->modele.lme")->Resultats$"Modèle linéaire mixte"
        
        try(  eval(parse(text=paste0("lme(", modele.lme, ", random=", random, ",data= longdata, method='REML')"))),silent=T)->modele.lme1        
        if(class(modele.lme1)=="try-error"){
          while(class(modele.lme1)=="try-error")
            dlgMessage("Le modèle n a pas pu converger. Vous pouvez modifier les paramètres de convergence ou abandonner. Voulez-vous modifier les paramètres de convergence ?", "yesno")$res->modele.lme1
          if(modele.lme1=="yes"){
            Form <- list("maxIter:NUM"=50, "msMaxIter:NUM"=50, "niterEM:NUM"=25)
            dlgForm(Form, "Paramètres du modèle LME")$res->Form  
            if(any(is.na(unlist(Form))))  Form <- list("maxIter:NUM"=50, "msMaxIter:NUM"=50, "niterEM:NUM"=25)
            lmeControl(maxIter=Form$maxIter,msMaxIter=Form$msMaxIter,niterEM=Form$niterEM )->controle
            #try( lme(as.formula(modele.lme), random=as.formula(random), data=longdata, method="REML", control=controle),silent=T)->modele.lme
            try(eval(parse(text=paste0("lme(", modele.lme, ", random=", random, ",data= longdata, method='REML')"))),silent=T)->modele.lme1
            
          }
        }
        if(class(modele.lme1)=="lme"){ 
          modele.lme1->aov.plus.in$modele.lme1
          if(any(desires2== "Modèle linéaire mixte")) anova(modele.lme1)->Resultats$"modèle linéaire mixte avec comme estimateur le maximum de vraisemblance - REML"
          if(type.cont=="a priori"){
            contrastes->Resultats$"Contrastes a priori"$"Matrice de coefficients variables"  
            round(summary(modele.lme1)$tTable,4)->tableT
            data.frame(tableT)->tableT
            names(tableT)<-c("estimateur", "erreur.st", "ddl","valeur.t", "valeur.p")
            round(tableT$valeur.t^2/(tableT$valeur.t^2+tableT$ddl),4)->tableT$R.deux
            if(!is.null(inter)) {
              grepl(paste(inter,collapse = "|"), unlist(dimnames(tableT)[1]))->tableT$D.Cohen
              round( ifelse(tableT$D.Cohen==T, (2*tableT$valeur.t)/(nlevels(longdata$IDeasy)^0.5), tableT$valeur.t/(nlevels(longdata$IDeasy)^0.5)),4)->tableT$D.Cohen
            }else round(tableT$valeur.t/((nlevels(longdata$IDeasy))^0.5),4)->tableT$D.Cohen
            
            tableT[1,"D.Cohen"]<-""
            tableT[1,"R.deux"]<-""
            tableT->Resultats$"Table des contrastes sur le modele linéaire mixte"
          }
        }
        
        
        if(!is.null(intra) & is.null(inter) & is.null(cov) & type.cont=="a priori"){
          longdata[do.call("order", longdata[unlist(intra)]), ]->longdata
          list()->combinaison
          for(i in 1:length(contrastes)){ combn(1:length(contrastes), i)->combinaison[[i]]        }
          Table.contrastes<-c()
          for(i in 1:length(combinaison) ){
            
            for(j in 1:ncol(combinaison[[i]])){
              M1<-matrix(rep(1, length(longdata[,VD])), ncol=1)
              for(k in 1:nrow(combinaison[[i]])){
                M2<-c()
                for(l in 1:ncol(contrastes[[combinaison[[i]][k,j]]])){
                  rep(contrastes[[combinaison[[i]][k,j]]][,l], each=length(longdata[,VD])/prod(N.modalites2[1:combinaison[[i]][k,j]]), len =length(longdata[,VD]))->coef1
                  cbind(M2,coef1)->M2
                  
                }
                M4<-c()
                for(m in 1:ncol(M1))  {
                  for(n in 1 : ncol(M2)){
                    M1[,m]*M2[,n]->M3
                    cbind(M4, M3)->M4
                  }
                  
                }
                M4->M1
              }
              for(o in 1:ncol(M1)){
                longdata[,VD]*M1[,o]->coef1
                t.test(rowSums( matrix(coef1, ncol=prod(N.modalites2))), mu = 0, paired = FALSE, conf.level = 0.95)->C1
                rbind(Table.contrastes,c(C1$estimate, C1$parameter, C1$statistic, C1$p.value))->Table.contrastes
                
              }
            }
            
          }
          
          round(Table.contrastes,4)->Table.contrastes
          data.frame(Table.contrastes)->Table.contrastes  
          names(Table.contrastes)<-c("estimateur", "ddl","valeur.t", "valeur.p")
          dimnames(Table.contrastes)[[1]]<-dimnames(tableT)[[1]][-1]
          Table.contrastes$valeur.t^2/(Table.contrastes$valeur.t^2+Table.contrastes$ddl)->Table.contrastes$R.deux
          round(Table.contrastes$valeur.t/(nlevels(longdata$IDeasy))^0.5,4)->Table.contrastes$D.Cohen
          Table.contrastes->Resultats$"Table des contrastes imitant les logiciels commerciaux"
          
        } 
        
      }
      
      if(type.cont== "Comparaison 2 à 2"){
        c(inter, unlist(intra))->interintra
        list()[1:length(interintra)]->comparaisons
        names(comparaisons)<-interintra
        for(i in 1:length(interintra)){
          if(interintra[i] %in% intra) {pairwise.t.test(longdata[,VD],longdata[,interintra[[i]]], paired=T,p.adj=p.adjust)$p.value->comparaisons[[i]]$"table des probabilités"}else{
            pairwise.t.test(longdata[,VD],longdata[,interintra[[i]]], paired=F,p.adj=p.adjust)$p.value->comparaisons[[i]]$"table des probabilités"
          }
        }
        Resultats$"Comparaisons 2 à 2"<-comparaisons
      }
    }
    
    assign("aov.plus.in",aov.plus.in,envir=.e)
    if(any(desires2=="Modèle non paramétrique" )){
      if(!is.null(inter)){
        kruskal.test(as.formula( paste0(VD, "~",inter[1])), data = longdata)->KW
        round(data.frame(KW$statistic,KW$parameter,KW$p.value),4)->KW
        names(KW)<-c("H","ddl","valeur.p")
        round((KW$H-nlevels(longdata[,inter])+1)/(length(longdata[,1])-nlevels(longdata[,inter])),4)->eta
        if(eta<0.0001) "<0.001"->KW$eta.carré.de.H else KW$eta.carré.de.H
        round(KW$H/((length(longdata[,1])^2-1)/(length(longdata[,1])+1)),4)->KW$espilon.carré
        KW->Resultats$"Analyse non paramétrique"$"Test de Kruskal-Wallis"
        
        if(!is.null(contrastes) && any(rowSums((contrastes[[1]]!=0))==0)) {kruskalmc( as.formula(paste0(VD, "~",inter[1])), 
                                                                                      data=longdata, cont='two-tailed')->Resultats$"Analyse non paramétrique"$"Test de Kruskal-Wallis - Comparaison à une ligne de base"} else{
                                                                                        kruskalmc( as.formula( paste0(VD, "~",inter[1])), data=longdata)->Resultats$"Analyse non paramétrique"$"Test de Kruskal-Wallis - Comparaison deux à deux"   
                                                                                        
                                                                                      }
      }else{
        friedman.test(as.formula(paste(VD,"~", intra[[1]], "|IDeasy" )),data=longdata)->friedman
        round(data.frame(friedman$statistic,friedman$parameter,friedman$p.value),4)->friedman
        names(friedman)<-c("chi.deux","ddl","valeur.p")
        round(friedman$chi.deux/(length(longdata[,1])*(nlevels(longdata[,unlist(intra)])-1)),4)->friedman$W.de.Kendall
        friedman->Resultats$"Analyse non paramétrique"$"Anova de Friedman"
        friedmanmc(longdata[,VD], longdata[,intra[[1]]], longdata$IDeasy)->Resultats$"Comparaison 2 à 2 pour ANOVA de Friedman"
      }
    }
    
    if(any(desires2=="Statistiques robustes - peut prendre du temps")){
      if(length(inter)==1 & is.null(intra)){
        if(is.null(contrastes)) Contrasts(levels(longdata[,inter]))->contrastes else contrastes[[1]]->contrastes
        split(longdata[,VD], longdata[,inter])->robuste
        try(unlist(WRS::med1way(robuste,iter = 1000)), silent=T)->mediane
        if(class(mediane)!="try-error"){
          names(mediane)<-c("Test", "Valeur.critique","valeur.p")
          round(mediane,4)->Resultats$"Anova basée sur les médianes"$"Analyse principale"
          WRS::medpb(robuste,alpha=.05,nboot=1000,con=contrastes,bhop=FALSE)->cont
          dimnames(cont$output)[[2]]<-c("Numéro.contraste","Valeur.contraste",
                                        "valeur.p","p.critique.corrigée","lim.inf.IC","lim.sup.IC")
          cont$output->Resultats$"Anova basée sur les médianes"$"Contrastes"
        }else {
          "Désolé, nous n'avons pas pu calculé l'anova sur les médianes, possiblement en raison d'un nombre import d'ex aequo."->Resultats$"Anova basée sur les médianes"
        }
        try( WRS2::t1way(as.formula(paste0(VD, "~",inter)), tr=.2,data=longdata),silent=T)->AR1
        if(class(AR1)!="try-error"){
          WRS2::t1way(as.formula(paste0(VD, "~",inter)), tr=.2,data=longdata)->AR1
          WRS2::t1waybt(as.formula(paste0(VD, "~",inter)), tr=.2, nboot=2000,data=longdata)->AR2
          data.frame(AR1[[2]],AR1[[3]],AR1[[1]],AR2[[2]],AR2[[3]],AR2[[4]], AR2[[5]])->AR1
          names(AR1)<-c("ddl.num","ddl.denom","Stat","valeur.p","Var.expliquée","Taille.effet","Nombre.bootstrap" )
          AR1->Resultats$"Anova basée sur les moyennes tronquées"$"Analyse principale"
          "Les probabilités et les IC sont estimés sur la base d'un bootsrap. L'IC est corrigé pour comparaison multiple, contrairement à la probabilité reportée"->Resultats$"Anova basée sur les moyennes tronquées"$"Information"
          try(WRS::lincon(robuste, tr=.2, con=contrastes),silent=T)->cont
          try(WRS::mcppb20(robuste, tr=.2, nboot=2000, con=contrastes),silent=T)->cont2
          if(class(cont)!= "try-error") {data.frame(cont$psihat[,2],cont$test[,4],cont$test[,5],cont$test[,2],cont$test[,3],cont2$psihat[,4],cont2$psihat[,5],cont2$psihat[,6])->cont
            names(cont)<-c("Valeur.contraste","erreur.standard","ddl","test","seuil.critique","lim.inf.IC","lim.sup.IC","valeur.p")
            cont->Resultats$"Anova basée sur les moyennes tronquées"$"Contrastes"}
          if(class(cont2)!="try-error") cont2[3]->Resultats$"Anova basée sur les moyennes tronquées"$"Coefficients des contrastes"
          
        }else{
          "Désolé, nous n'avons pas pu calculé l'anova sur les moyennes tronquées."->Resultats$"Anova basée sur les moyennes tronquées"
        }
        
      }
      
      
      if(length(inter)==2 & is.null(intra)) { 
        
        
        try( WRS2::t2way(as.formula(paste0(VD, "~",inter[1],"*",inter[2])), data=longdata, tr = 0.2), silent=T)->T2
        if(class(T2)!="try-error"){
          round(matrix(unlist(T2[1:6]), ncol=2, byrow=T),4)->T2
          dimnames(T2)[[2]]<-c("valeur", "valeur.p")
          c(names(longdata[,inter]), paste(names(longdata[,inter])[1],":",names(longdata[,inter])[2]))->dimnames(T2)[[1]]
          T2->Resultats$"ANOVA sur moyennes tronquées a 0.2"$"Analyse principale"
        }
        try(WRS2::pbad2way(as.formula(paste0(VD, "~",inter[1],"*",inter[2])), data=longdata, est = "mom", nboot = 599)->Resultats$"ANOVA sur M estimator"$"Analyse principale",silent=T)
        try(WRS2::pbad2way(as.formula(paste0(VD, "~",inter[1],"*",inter[2])), data=longdata, est = "median", nboot = 599)->Resultats$"ANOVA sur les médianes"$"Analyse principale",silent=T)
        try(model.matrix(mcp2a(as.formula(paste0(VD, "~",inter[1],"*",inter[2])), data=longdata, est = "median"))->Resultats$"Comparaisons post hoc"$"Matrice de contrastes", silent=T)
        try(WRS2::mcp2a(as.formula(paste0(VD, "~",inter[1],"*",inter[2])), data=longdata, est = "mom", nboot = 599), silent=T)->mediane
        if(class(mediane)!="try-error") {
          paste0("WRS2::mcp2a(formula = ", paste0(VD, "~", inter[1], "*", inter[2]), ", data = longdata, est = 'mom', nboot = 599)")->mediane$call
          mediane->Resultats$"Comparaisons post hoc"$"ANOVA sur le M estimator"
        }
        
        try(WRS2::mcp2a(as.formula(paste0(VD, "~",inter[1],"*",inter[2])), data=longdata, est = "median", nboot = 599), silent=T)->mediane
        if(class(mediane)!="try-error") {
          paste0("WRS2::mcp2a(formula = ", paste0(VD, "~", inter[1], "*", inter[2]), ", data = longdata, est = 'median', nboot = 599)")->mediane$call
          mediane->Resultats$"Comparaisons post hoc"$"ANOVA sur la médiane"
        }
      }
      
      if(length(inter)==3 & is.null(intra)){
        try( WRS2::t3way(as.formula(paste0(VD, "~",inter[1],"*",inter[2],"*",inter[3])), data=longdata, tr = 0.2), silent=T)->tronquees
        if(class(tronquees)!="try-error") {paste0("WRS2::t3way(", VD, "~",inter[1],"*",inter[2],"*",inter[3], ", data=longdata, tr = 0.2)")->tronquees$call
          tronquees->Resultats$'Anova sur les moyennes tronquées'  
        }
      }
      if(length(intra)==1 & is.null(inter)){
        try( rmanova(longdata$value,longdata[,intra[[1]]] ,longdata$IDeasy), silent=T)->ANOVA.tr
        if(class(ANOVA.tr)!="try-error"){
          round(data.frame("Valeur.test"= ANOVA.tr$test,"ddl1"=ANOVA.tr$df1, "ddl2"=ANOVA.tr$df2,"valeur.p"=ANOVA.tr$p.value),4)->ANOVA.tr
          ANOVA.tr->Resultats$"Statistiques robustes"$"Anova sur moyennes tronqueés à 20%"
          if((nlevels(longdata[,intra[[1]]]))>2) {rmmcp(longdata[,VD],longdata[, intra[[1]]],longdata$IDeasy)->comp
            comp$call<-paste0("rmmcp(longdata$", VD, ", longdata$", intra[[1]], ",longdata$IDeasy")
            comp->Resultats$"Statistiques robustes"$"Comparaisons 2 à 2 sur moyennes tronquées à 20%"}else Resultats$"Statistiques robustes"<-"Désolé, nous n'avons pas pu calculé l'anova robuste"
        }
        try( rmanovab(longdata[,VD],longdata[,intra[[1]]] ,longdata$IDeasy), silent=T)->ANOVA.tr
        if(class(ANOVA.tr)!="try-error"){
          data.frame("Valeur.test"=ANOVA.tr[[1]],"Valeur critique"=ANOVA.tr[[2]], "significativité"=if(ANOVA.tr[[1]]<ANOVA.tr[[2]]){"non significatif"}else"significatif")->ANOVA.tr
          ANOVA.tr->Resultats$"Statistiques robustes"$"Anova sur moyennes tronquées à 20% avec bootstrap"   
        }else Resultats$"Statistiques robustes"<-"Désolé, nous n'avons pas pu calculé l'anova robuste"
        
        if((nlevels(longdata[,intra[[1]]]))>2) {
          try(pairdepb(longdata[,VD],longdata[,intra[[1]]] ,longdata$IDeasy), silent=T)->comp
          if(class(comp)!="try-error") {paste0("pairdepb(y = longdata$", VD, ", groups = longdata$", intra[[1]],", blocks = longdata$IDeasy)" )->comp$call
            comp->Resultats$"Statistiques robustes"$"Comparaisons 2 à 2 sur les moyennes tronquées à 20% avec bootsrap"}else Resultats$"Statistiques robustes"<-"Désolé, nous n'avons pas pu calculé l'anova robuste"
        }
      } 
      
      if(length(inter)==1 & length(intra)==1){
        as.formula(paste0(VD, "~", predicteurs))->modeleR
        try(WRS2::tsplit( modeleR, IDeasy, data=longdata, tr = 0.2), silent=T)->tronquees
        if(class(tronquees)!="try-error"){
          tronquees$call<- paste0("WRS2::tsplit(", VD,"~", intra[[1]],"*", inter, ", IDeasy, data=longdata, tr = 0.2)")
          tronquees->Resultats$'Anova sur les moyennes tronquées' # anova mixte sur moyennes tronquÃÂÃÂÃÂÃÂ©es 
          WRS2::sppba(modeleR, IDeasy, data=longdata, est = "mom", avg = TRUE, nboot = 500, MDIS = FALSE)->MoMa # anova sur moyenne oÃÂÃÂÃÂÃÂ¹ on enlÃÂÃÂÃÂÃÂ¨ve les valeurs aberrantes avec bootstrap pour l'effet de A
          WRS2::sppbb(modeleR, IDeasy, data=longdata, est = "mom", nboot = 500)->MoMb# anova avec bootstrap pour l'effet de B
          WRS2::sppbi(modeleR, IDeasy, data=longdata, est = "mom", nboot = 500)->MoMi # # anova avec bootstrap pour l'effet d'interaction
          data.frame("effet"= c(inter,intra[[1]],"interaction"), "valeur.p"=c(MoMa$p.value,MoMb$p.value, MoMi$p.value) )->MoM
          MoM->Resultats$"Anova sur l'estimateur modifié de localisation de Huber"
        }else Resultats$"Statistiques robustes"<-"Désolé, nous n'avons pas pu calculé l'anova robuste"
        
      }
      
    }
    return(Resultats)
  }   
  
  list()->aov.plus.list
  anova2(VD=VD, inter=inter, intra=intra, longdata=longdata,  type.cont=type.cont, SumS=SumS,
         desires2=desires2, cov=cov, p.adjust=p.adjust, contrastes=contrastes)->complet
  if(any(desires=="Données complètes")){
    complet->Resultats$"Données complètes"
    aov.plus.in->aov.plus.list$"Données complètes"}
  
  if(any(desires=="Identification des outliers")|any(desires=="Données sans valeur influente")) { 
    if(is.null(longdata$residu)) {"L'analyse n'a pas pu aboutir"->Resultats$"Arrêt prématuré de l'analyse"
      return(Resultats)}
    valeurs.influentes(X="residu", critere="Grubbs",z=3.26, data=longdata)->influentes
    
    if(any(desires=="Identification des outliers")) influentes->Resultats$"Valeurs influentes"
    if(any(desires=="Données sans valeur influente")){
      if(!is.null(influentes$"observations influentes"$IDeasy)){
        setdiff(longdata$IDeasy,influentes$"observations influentes"$IDeasy)->diffs
        longdata[which(longdata$IDeasy%in%diffs), ]->nettoyees
        factor(nettoyees$IDeasy)->nettoyees$IDeasy
        anova2(VD=VD, inter=inter, intra=intra, longdata=nettoyees, type.cont=type.cont, SumS=SumS,
               desires2=desires2, cov=cov,p.adjust=p.adjust, contrastes=contrastes)->Resultats$"Données sans valeur influente"
        aov.plus.in->aov.plus.list$"Données sans valeur influente"
      }
      if(all(desires!="Données complètes"))  complet->Resultats$"Données sans valeur influente"
      
    }
    
  }
  class(aov.plus.list)<-"aovplus"
  assign("aov.plus.in", aov.plus.list,envir=.GlobalEnv) 
  ref1(packages)->Resultats$"Références des packages utilisés pour cette analyse"
  if(sauvegarde==T) save(Resultats=Resultats ,choix =paste("anova sur", nom), env=.e)
  
  ez.html(Resultats)
  return(Resultats)
  }
