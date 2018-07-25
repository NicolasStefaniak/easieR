choix.corr <-
function(){options (warn=-1) 
  c( "svDialogs")->packages
  if(any(lapply(packages, require, character.only=T))==FALSE)  {install.packages(packages) 
    require(packages)} 
  writeLines("l'analyse détaillée permet d'avoir les statistiques descriptives, les tests de normalité, le nuage de points,
\n des statistiques robustes, l'ensemble des coefficients de corrélations. 
\n la matrice de corrélation permet de contrôler l'erreur de 1e espèce et est adaptée pour un grand nombre de corrélations
\n la comparaison de corrélations permet de comparer 2 corrélations dépendantes ou indépendantes
\n Le choix + autre correlations + permet d'avoir les correlation tétrachoriques et polychoriques")
  dlgList(c("Analyse détaillee (Bravais Pearson/Spearman/tau) pour une ou peu de corrélations", 
            "Matrice de corrélations", 
            "Comparaison de deux corrélations",
            "Autres corrélations"), preselect=NULL, multiple = FALSE, title="Quelle analyse voulez-vous?")$res->choix
  if(length(choix)==0) return(analyse())
  switch(choix,
         "Analyse détaillee (Bravais Pearson/Spearman/tau) pour une ou peu de corrélations"=corr.complet()->Resultats,
         "Matrice de corrélations"= corr.matrice()->Resultats,
         "Comparaison de deux corrélations"= comp.corr()->Resultats,
         "Autres corrélations"= tetrapoly()->Resultats
  )
  return(Resultats)
}
