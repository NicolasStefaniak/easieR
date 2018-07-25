interfaceR <-
function(){
  options (warn=-1) 
  packages <- c("svDialogs","pkgmaker")
  lapply(packages, require,character.only=T)
  Resultats <- list()
  write.pkgbib(packages, file='references')
  
  
  choix <- dlgList(c("obtenir le répertoire de travail","spécifier le répertoire de travail", "Suppression d objet en mémoire", 
                     "liste des objets en mémoire", "rechercher une nouvelle fonction", "mise à jour des packages","Vérifier l installation des packages"), preselect=NULL, multiple = FALSE, title="Quel est votre choix ?")$res
  while(length(choix)==0) return(easieR())
  
  switch(choix, 
         "obtenir le répertoire de travail" = Resultats$"Répertoire de travail" <- getwd(),
         "liste des objets en mémoire"= Resultats$"Objets en mémoire" <- ls(envir=.GlobalEnv),
         "spécifier le répertoire de travail"={
           repertoire <- dlgDir(title="Veuillez choisir le répertoire de travail")$res
           if(length(repertoire)==0) repertoire <- getwd()
           setwd(repertoire)
           Resultats$"nouveau répertoire" <- paste("Le répertoire de travail est à présent", repertoire)
         },
         "Suppression d objet en mémoire"={
           ls(envir=.GlobalEnv)->tout
           Filter( function(x) 'function' %in% class( get(x) ), ls(envir=.GlobalEnv) )->fonctions
           tout[!is.element(tout,fonctions)]->tout
           X<-dlgList(tout, multiple = TRUE, title="Objets à supprimer")$res
           if(length(X)==0) return(easieR())
           rm(list=X, envir=.GlobalEnv)
           Resultats <- list()
           Resultats$"Liste des objects encore en mémoire de R" <- ls(envir=.GlobalEnv)
         },
         "rechercher une nouvelle fonction"={
           require(sos)
           writeLines("Pour trouver une nouvelle analyse, il est nécessaire de faire votre recherche en anglais. Vous pouvez utiliser plusieurs mots dans la recherche.
Une page html reprenant l'ensemble des packages faisant référence à l'analyse recherchée va s'ouvrir.")
           critere <- dlgInput("Quelle analyse recherchez vous ?", "Tapez votre recherche ici")$res
           if(length(critere)==0) return(easieR())
           critere <- strsplit(critere, ":")
           critere <- tail(critere[[1]],n=1)
           Resultats<- findFn(critere)
           return(Resultats)
         },
         "mise à jour des packages"= {update.packages(ask=FALSE)},
         "Vérifier l installation des packages"=vef.pack()->Resultats$"Vérification des packages")
  bibtex::read.bib('references.bib')->Resultats$"Références des packages utilisés"
  file.remove('references.bib')
  
  return(Resultats)
}
