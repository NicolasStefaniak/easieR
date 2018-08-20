interfaceR <-
  function(){
    options (warn=-1) 
    packages <- c("svDialogs","pkgmaker")
    lapply(packages, require,character.only=T)
    Resultats <- list()
    write.pkgbib(packages, file='references')
    
    
    choix <- dlgList(c("obtenir le repertoire de travail","specifier le repertoire de travail", "Suppression d objet en memoire", 
                       "liste des objets en memoire", "rechercher une nouvelle fonction", "mise a jour des packages","Verifier l installation des packages"), preselect=NULL, multiple = FALSE, title="Quel est votre choix ?")$res
    while(length(choix)==0) return(easieR())
    
    switch(choix, 
           "obtenir le repertoire de travail" = Resultats$"Repertoire de travail" <- getwd(),
           "liste des objets en memoire"= Resultats$"Objets en memoire" <- ls(envir=.GlobalEnv),
           "specifier le repertoire de travail"={
             repertoire <- dlgDir(title="Veuillez choisir le repertoire de travail")$res
             if(length(repertoire)==0) repertoire <- getwd()
             setwd(repertoire)
             Resultats$"nouveau repertoire" <- paste("Le repertoire de travail est a present", repertoire)
           },
           "Suppression d objet en memoire"={
             ls(envir=.GlobalEnv)->tout
             Filter( function(x) 'function' %in% class( get(x) ), ls(envir=.GlobalEnv) )->fonctions
             tout[!is.element(tout,fonctions)]->tout
             X<-dlgList(tout, multiple = TRUE, title="Objets a supprimer")$res
             if(length(X)==0) return(easieR())
             rm(list=X, envir=.GlobalEnv)
             Resultats <- list()
             Resultats$"Liste des objects encore en memoire de R" <- ls(envir=.GlobalEnv)
           },
           "rechercher une nouvelle fonction"={
             require(sos)
             writeLines("Pour trouver une nouvelle analyse, il est necessaire de faire votre recherche en anglais. Vous pouvez utiliser plusieurs mots dans la recherche.
Une page html reprenant l'ensemble des packages faisant reference a l'analyse recherchee va s'ouvrir.")
             critere <- dlgInput("Quelle analyse recherchez vous ?", "Tapez votre recherche ici")$res
             if(length(critere)==0) return(easieR())
             critere <- strsplit(critere, ":")
             critere <- tail(critere[[1]],n=1)
             Resultats<- findFn(critere)
             return(Resultats)
           },
           "mise a jour des packages"= {update.packages(ask=FALSE)},
           "Verifier l installation des packages"=vef.pack()->Resultats$"Verification des packages")
    bibtex::read.bib('references.bib')->Resultats$"References des packages utilises"
    file.remove('references.bib')
    
    return(Resultats)
  }
