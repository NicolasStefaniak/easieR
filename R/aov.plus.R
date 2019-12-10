aov.plus <-
  function(aov.plus.list=NULL, info=T){
    options (warn=-1)
    packages<-c("svDialogs","emmeans")
    if(any(lapply(packages, require, character.only=T))==FALSE)  {install.packages(packages) 
      require(packages)}
      .e <- environment()  

    if(is.null(aov.plus.list)){
      Filter( function(x) 'aovplus' %in% class( get(x) ), ls(envir=.GlobalEnv))->nom1
      if(length(nom1)==0) {
        writeLines("il n'y a pas d'objet compatible avec aov.plus dans la memoire de R. 
                   Vous devez realiser une analyse de variance au prealable")
        return(ez.anova())}
      if(length(nom1)==1)  aov.plus.list<-get(nom1) else{
        if(info=="TRUE") writeLines("Veuillez choisir le modele que vous desirez analyser avec aov.plus")
        nom1 <- dlgList(nom1, multiple = FALSE, title="Modele ?")$res
        if(length(nom1)==0) {nom1<-NULL
        aov.plus.list<-NULL}
        if(!is.null(nom1))  aov.plus.list<-get(nom1)
      } 
    }
    


    if(length(aov.plus.list)==3){
      writeLines("Voulez-vous realiser les analyses sur les donnees completes ou sur les donnees sans les valeurs influentes ?")
      type<-dlgList(names(aov.plus.list)[2:3], multiple = FALSE, title="Quelles donnees voulez-vous analyser?")$res
      if(length(type)==0) return("vous avez quitte aov.plus")
      if(type=="Donnees completes") aov.plus.list[[2]]->aov.plus.list else aov.plus.list[[2]]->aov.plus.list
    }else aov.plus.list[[2]]->aov.plus.list
    
    
    writeLines("Cette fonction permet de fournir les moyennes et erreurs-types ajustees ainsi que le graphique correspondant.
               Avec le choix post hoc sur les interactions, vous pouvez tester les effets d'interaction 2 a 2 et les effet simples.")
    choix<-dlgList(c("moyennes et erreurs-types ajustees","contrastes"), 
                   multiple = TRUE, title="Quelles donnees voulez-vous analyser?")$res 
    if(length(choix)==0) return(analyse())
    
    Resultats<-list()
    noms<-names(summary(aov.plus.list[["em.out"]]))[which(sapply(summary(aov.plus.list[["em.out"]]),class) =="factor")]
    
    if(any(choix=="moyennes et erreurs-types ajustees")){
      writeLines("Pour quelle combinaison de facteurs desirez-vous afficher les moyennes ajustees ?")
      facteurs<-dlgList(noms, multiple = TRUE, title="Que voulez-vous afficher ?")$res
      if(length(facteurs)==0) return(aov.plus()) 
      formula<-paste0("~",facteurs[[1]])
    if(length(facteurs)>1){      
      for(i in 2:length(facteurs)){
        formula<-paste(formula, "+", facteurs[i])
      }}
      recordPlot()->graphe
      Resultats$"Moyennes ajustee-Graphique"<-emmip(object= aov.plus.in$`Donnees completes`$em.out,as.formula(formula) , CIs=T)
      em.out<-emmeans(object= aov.plus.in$`Donnees completes`$em.out,as.formula(formula), CIs=T)
      Resultats$"Moyennes ajustee"<-data.frame(em.out)
 
    }
    
    if(any(choix=="contrastes")){
      writeLines("Veuillez spécifier les contrastes.")
      if(length(choix)==0) return(aov.plus())
      p.adjust<-dlgList(c("holm", "hochberg", "hommel", "bonferroni", "fdr","tukey","scheffe",
                "sidak","dunnettx","mvt" ,"none" ), preselect="holm", multiple = FALSE, title="Type de correction ?")$res
      if(length(p.adjust)==0) p.adjust<-"none"
    
        cont.data<-data.frame(aov.plus.in$`Donnees completes`$em.out)
        cont.data<-cont.data[, noms]
        cont.data<-fix(cont.data)
        suppress<-which(colSums(is.na(cont.data)) > 0)
        if(length(suppress>0)) cont.data<-cont.data[,-suppress]
        Resultats$Contrates$coefficients<-cont.data
        emm.out<-contrast(aov.plus.in$`Donnees completes`$em.out, 
                                                method= list(cont.data[, which(sapply(cont.data, class)=="numeric")]), adjust=p.adjust)
        emm.out<-data.frame(emm.out)
        names(emm.out)[6]<-"valeur.p"
       
        emm.out$contrast<-names(cont.data)[which(sapply(cont.data, class)=="numeric")]
        Resultats$Contrates$contrastes<-emm.out
        }
        ref1(packages)->Resultats$"References des packages utilises pour cette analyse"
        .add.result(Resultats=Resultats, name =paste("Anova plus", Sys.time() ))
#    if(sauvegarde==T) save(Resultats=Resultats ,choix ="Resultats.aov.plus", env=.e)
      ez.html(Resultats)
      return(Resultats)
    
  }
