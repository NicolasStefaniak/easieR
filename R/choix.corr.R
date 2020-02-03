choix.corr <-
  function(html=T){options (warn=-1) 
    c( "svDialogs")->packages
    if(any(lapply(packages, require, character.only=T))==FALSE)  {install.packages(packages) 
      require(packages)} 
    writeLines("l'analyse detaillee permet d'avoir les statistiques descriptives, les tests de normalite, le nuage de points,
               \n des statistiques robustes, l'ensemble des coefficients de correlations. 
               \n la matrice de correlation permet de contrôler l'erreur de 1e espece et est adaptee pour un grand nombre de correlations
               \n la comparaison de correlations permet de comparer 2 correlations dependantes ou independantes
               \n Le choix + autre correlations + permet d'avoir les correlation tetrachoriques et polychoriques")
    dlgList(c("Analyse detaillee (Bravais Pearson/Spearman/tau) pour une ou peu de correlations", 
              "Matrice de correlations", 
              "Comparaison de deux correlations",
              "Autres correlations"), preselect=NULL, multiple = FALSE, title="Quelle analyse voulez-vous?")$res->choix
    if(length(choix)==0) return(analyse())
    switch(choix,
           "Analyse detaillee (Bravais Pearson/Spearman/tau) pour une ou peu de correlations"=corr.complet(html=html)->Resultats,
           "Matrice de correlations"= corr.matrice(html=html)->Resultats,
           "Comparaison de deux correlations"= comp.corr(html=html)->Resultats,
           "Autres correlations"= tetrapoly(html=html)->Resultats
    )
    return(Resultats)
  }
