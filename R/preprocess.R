preprocess <-
function(){
  choix<-dlgList(c("Rangs", "Imputation de valeurs manquantes",
                   "Sélectionner des observations","Sélectionner des variables","Centrer / centrer réduire","Trier",
                   "Opérations mathématiques sur des variables","Tableau croisé dynamique"), multiple = F, preselect="rangs", title="Que voulez-vous faire ?")$res
  if(length(choix)==0) return(easieR())
  switch(choix, "Rangs"= ez.rank()->Resultats,
         "Imputation de valeurs manquantes"=ez.imp()->Resultats,
         "Sélectionner des observations"=selectionO()->Resultats,
           "Sélectionner des variables"=SelectionV()->Resultats,
           "Centrer / centrer réduire"= Centrer.red()->Resultats,"Trier"= trier()->Resultats,
         "Opérations mathématiques sur des variables"= maths()->Resultats,
         "Tableau croisé dynamique"={ 
           try(library("rpivotTable"), silent=T)->test2
           if(class(test2)== "try-error") return(ez.install())
          return( rpivotTable(choix.data(nom=F)))
         }
         )
  return(Resultats)
}
