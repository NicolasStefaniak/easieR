preprocess <-
  function(){
    choix<-dlgList(c("Rangs", "Imputation de valeurs manquantes",
                     "Selectionner des observations","Selectionner des variables","Centrer / centrer reduire","Trier",
                     "Operations mathematiques sur des variables","Tableau croise dynamique"), multiple = F, preselect="rangs", title="Que voulez-vous faire ?")$res
    if(length(choix)==0) return(easieR())
    switch(choix, "Rangs"= ez.rank()->Resultats,
           "Imputation de valeurs manquantes"=ez.imp()->Resultats,
           "Selectionner des observations"=selectionO()->Resultats,
           "Selectionner des variables"=SelectionV()->Resultats,
           "Centrer / centrer reduire"= Centrer.red()->Resultats,"Trier"= trier()->Resultats,
           "Operations mathematiques sur des variables"= maths()->Resultats,
           "Tableau croise dynamique"={ 
             try(library("rpivotTable"), silent=T)->test2
             if(class(test2)== "try-error") return(ez.install())
             return( rpivotTable(choix.data(nom=F)))
           }
    )
    return(Resultats)
  }
