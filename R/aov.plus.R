aov.plus <-
  function(aov.plus.list=NULL, info=T){
    options (warn=-1)
    packages<-c("psych","svDialogs","phia")
    if(any(lapply(packages, require, character.only=T))==FALSE)  {install.packages(packages) 
      require(packages)}
    
    contrastes.ez2<-function(longdata, var=NULL){
      Resultats<-list()
      contrastes<-list()
      for(i in 1:length(var)){
        matrix(rep(0,times=nlevels(longdata[,var[i]])), nrow=nlevels(longdata[,var[i]]))->contrastes3
        dimnames(contrastes3)[[1]]<-levels(longdata[,var[i]])
        fix(contrastes3)->contrastes3
        contrastes3[which(is.na(contrastes3))]<-0
        b<-rle(c(contrastes3))
        if(b$values[which.max(b$lengths)]==0 & max(b$lengths)>2*(nlevels(longdata[,var[i]])-2)) {
          if(okCancelBox("il ne peut pas y avoir uniquement des 0 dans une colonne. Appuyez sur ok pour continuer, et annuler pour annuler")) {return(contrastes.ez2(longdata, var))}else return(NULL)
        }
        
        paste0(contrastes3, "*",dimnames(contrastes3)[[1]])->noms
        for(j in 1:(length(noms)/length(dimnames(contrastes3)[[1]]))){
          inf<-1+(length(dimnames(contrastes3)[[1]])*(j-1))
          sup<-j*length(dimnames(contrastes3)[[1]])
          paste(noms[inf:sup],collapse="")->nom
          nom->dimnames(contrastes3)[[2]][j]
        }
        contrastes[[i]]<-contrastes3
      }
      names(contrastes)<-var
      return(contrastes)
    }
    
    if(is.null(aov.plus.list)){
      Filter( function(x) 'aovplus' %in% class( get(x) ), ls(envir=.GlobalEnv))->nom1
      if(length(nom1)==0) {
        writeLines("il n'y a pas d'objet compatible avec aov.plus dans la memoire de R. Vous devez realiser une analyse de variance au prealable")
        return(AN.C.OVA())}
      if(length(nom1)==1)  aov.plus.list<-get(nom1) else{
        if(info=="TRUE") writeLines("Veuillez choisir le modele que vous desirez analyser avec aov.plus")
        nom1 <- dlgList(nom1, multiple = FALSE, title="Modele ?")$res
        if(length(nom1)==0) {nom1<-NULL
        aov.plus.list<-NULL}
        if(!is.null(nom1))  aov.plus.list<-get(nom1)
      } 
    }
    
    .e <- environment()
    if(is.null(aov.plus.list)) {
      return(writeLines("La fonction aov.plus necessite qu'une anova ait ete realisee. 
                        Pour pouvoir utiliser l'ensemble des options, il est necessaire d'avoir choisi modele lineaire mixte"))
    }
    if(length(aov.plus.list)==2){
      writeLines("Voulez-vous realiser les analyses sur les donnees completes ou sur les donnees sans les valeurs influentes ?")
      type<-dlgList(names(aov.plus.list), multiple = FALSE, title="Quelles donnees voulez-vous analyser?")$res
      print(type)
      if(length(type)==0) return("vous avez quitte aov.plus")
      if(type=="Donnees completes") aov.plus.list[[1]]->aov.plus.list else aov.plus.list[[2]]->aov.plus.list
    }else aov.plus.list[[1]]->aov.plus.list
    
    if(length(grep("modele.lme1", names(aov.plus.list))!=0)) {
      aov.plus.list[[grep("modele.lme1", names(aov.plus.list))]]->modele.lme
      aov.plus.list[[grep("modele.lme1", names(aov.plus.list))]]<-NULL
      writeLines("Cette fonction permet de fournir les statistiques descriptives detaillees par variable avec le choix statistiques
                 descriptives completes. Vous pouvez afficher les moyennes et erreurs-types ajustees ainsi que le graphique correspondant.
                 Avec le choix post hoc sur les interactions, vous pouvez tester les effets d'interaction 2 à 2 et les effet simpes.")
      choix<-dlgList(c("statistiques descriptives detaillees","moyennes et erreurs-types ajustees","contrastes sur les interactions"), 
                     multiple = TRUE, title="Quelles donnees voulez-vous analyser?")$res 
      if(length(choix)==0) return(analyse())
    }else{writeLines("Cette fonction permet de fournir les statistiques descriptives detaillees par variable avec le choix statistiques
descriptives completes.")
      choix<-"statistiques descriptives detaillees"
    }
    
    Resultats<-list()
    if(any(choix=="statistiques descriptives detaillees")){
      writeLines("Veuillez choisir les variables ou combinaison de variables pour lesquelles vous desirez afficher les statistiques descriptives")
      vars<-dlgList(names(aov.plus.list), multiple = TRUE, title="Que voulez-vous afficher ?")$res
      if(length(vars)==0) return(aov.plus())
      for(i in 1:length(vars)){View(aov.plus.list[[vars[i]]],title= vars[i])}    
    }
    
    if(any(choix=="moyennes et erreurs-types ajustees")){
      writeLines("Pour quels (combinaison de) facteurs desirez-vous afficher les moyennes ajustees ?")
      facteurs<-dlgList(names(modele.lme$contrasts), multiple = TRUE, title="Que voulez-vous afficher ?")$res
      if(length(facteurs)==0) return(aov.plus()) 
      # rajouter la pente
      means.lme <- interactionMeans(modele.lme, facteurs)
      plot(means.lme, abbrev.levels=TRUE)
      recordPlot()->graphe
      Resultats$"Moyennes ajustee"<- means.lme
    }
    
    if(any(choix=="contrastes sur les interactions")){
      writeLines("Vous pouvez selectionner plusieurs options. Quelles options, voulez-vous specifier ? Les comparaisons 2 à 2 vous permettent d'avoir les comparaisons 2 à 2;
                 Specifier les contrastes vous permettent de tester virtuelle n'importe quel contraste. Si plusieurs variables sont introduites pour 
                 l'ensemble des deux options, seuls les contrastes d'interaction seront calcules.
                 La decomposition des effets va vous peremttre d'obtenir les comparaisons specifiees dans les deux options precedentes pour chaque modalite des variables specifiees à ce niveau.
                 Toutes les variables ne doivent pas necessairement etre introduites dans l'analyse. Dans ce cas, les contrastes choisis seront calcules sur
                 l'ensemble des modalites confondues")
      choix<-dlgList(c("Comparaison 2 à 2", "Specifier contrastes", "Decomposer les effets par modalite"), multiple = TRUE, title="Que voulez-vous specifier ?")$res
      if(length(choix)==0) return(aov.plus())
      facteurs<-names(modele.lme$contrasts)
      if(any(choix=="Comparaison 2 à 2")) {   
        paires<-dlgList(facteurs, multiple = TRUE, title="Comparaison 2 à 2 ?")$res
        if(length(paires)==0) return(aov.plus())
      }else paires<-NULL
      setdiff(facteurs, paires)->diff
      if(length(diff)!=0 & any(choix=="Specifier contrastes")) {   
        cont.spe1<-dlgList(diff, multiple = TRUE, title="Variables à specifier ?")$res
        if(length(cont.spe1)==0) return(aov.plus())
        contrastes.ez2(longdata=modele.lme$data, var=cont.spe1)->cont.spe
      }else {cont.spe<-NULL
      cont.spe1<-NULL}
      setdiff(diff, cont.spe1)->diff
      
      if(length(diff)!=0 & any(choix=="Decomposer les effets par modalite")){
        fixed1<-dlgList(diff, multiple = TRUE, title="Variable à decomposer ?")$res
        if(length(fixed1)==0) return(aov.plus())}else {
          fixed1<-NULL 
        }
      dlgList(c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY","fdr", "none"), preselect="holm", multiple = FALSE, title="Type de correction ?")$res->p.adjust
      if(length(p.adjust)==0) p.adjust<-"none"
      testInteractions(modele.lme,pairwise=paires, fixed=fixed1, adjustment=p.adjust, custom=cont.spe)->Resultats$"Contrastes d'interaction"
    }
    
    
    ref1(packages)->Resultats$"References des packages utilises pour cette analyse"
    writeLines("Voulez-vous sauvegarder les resultats de l'analyse ?")
    dlgList(c("TRUE","FALSE"), preselect="FALSE", multiple = FALSE, title="Voulez-vous sauvegarder?")$res->sauvegarde
    if(length(sauvegarde)==0) sauvegarde<-F
    if(sauvegarde==T) save(Resultats=Resultats ,choix ="Resultats.aov.plus", env=.e)
    ez.html(Resultats)
    return(Resultats)
    
    }
